/**
 * Decompiler for converting IR (Intermediate Representation) back to AST
 *
 * The decompiler takes compiled IR blocks and converts them back to FUSE AST nodes.
 *
 * Key features:
 * - Pre-processes all variable names from scope during creation to avoid conflicts
 * - Generates valid identifiers and handles name conflicts
 * - Detects compiler-generated function formats vs export formats
 * - Reconstructs complex operators (!=, <=, >=) from operator_not patterns
 * - Matches namespace entries for unknown blocks
 * - Auto-generates and stores namespace entries for unmatched blocks
 * - All generated tokens have line=0, column=0
 *
 * Usage:
 * ```typescript
 * import { createDecompiler, Scope, Namespace } from '@scratch-fuse/compiler'
 *
 * // Setup global scope and namespaces
 * const globalScope = new Scope(globalVariables)
 * const namespaces = new Map<string, Namespace>()
 * const compiledFunctions = [...] // Array of CompiledFunction
 *
 * // Create decompiler - variable names are pre-processed here
 * const decompiler = createDecompiler(globalScope, namespaces, compiledFunctions)
 *
 * // Decompile a variable
 * const varDecl = decompiler.decompileVariable(variable, initialValue)
 *
 * // Decompile blocks to statements
 * const statements = blocks.map(block => decompiler.decompileBlock(block))
 *
 * // Decompile a function
 * const funcDecl = decompiler.decompileFunction(compiledFunction)
 *
 * // Access auto-generated namespaces (for blocks not in the original namespaces)
 * const generatedNamespaces = decompiler.namespaces
 * ```
 */

import {
  Variable,
  Reporter,
  Block,
  Script,
  AnyInput,
  BooleanInput,
  SubstackInput
} from './base'
import type {
  FunctionDeclaration,
  Program,
  Statement,
  Expression,
  CallExpression,
  MemberExpression,
  IdentifierExpression,
  LiteralExpression,
  BinaryExpression,
  UnaryExpression,
  ArrayExpression,
  ExpressionStatement,
  AssignmentStatement,
  IfStatement,
  WhileStatement,
  LoopStatement,
  ReturnStatement,
  VariableDeclaration,
  BlockStatement,
  Parameter,
  IncrementStatement,
  ForStatement,
  DecoratorStatement,
  NoopStatement
} from '@scratch-fuse/core'

import {
  Scope,
  Namespace,
  NamespaceEntry,
  CompiledFunction,
  NamespaceEntryArgumentAny,
  NamespaceEntryArgumentBoolean,
  NamespaceEntryArgumentField,
  NamespaceEntryArgumentSubstack
} from './compiler'

export interface CompiledFunctionWithDefault extends CompiledFunction {
  defaultValues: string[]
}

type NamespaceMatch = {
  type: 'bool' | 'any' | 'void' | 'hat'
  value: CallExpression
}

export class DecompilerError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'DecompilerError'
  }
}

/**
 * Check if a string is a valid identifier
 */
function isValidIdentifier(name: string): boolean {
  if (!name || name.length === 0) return false
  // Must start with letter or underscore
  if (!/^[a-zA-Z_]/.test(name)) return false
  // Rest can be letters, digits, or underscores
  if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(name)) return false
  // Cannot be a reserved keyword
  const keywords = [
    'if',
    'else',
    'while',
    'for',
    'loop',
    'return',
    'global',
    'true',
    'false',
    'namespace',
    'export'
  ]
  return !keywords.includes(name)
}

/**
 * Create a token-like identifier with line=0, column=0
 */
function makeIdentifier(name: string): IdentifierExpression {
  return {
    type: 'Identifier',
    name,
    line: 0,
    column: 0
  }
}

/**
 * Create a literal expression
 */
function makeLiteral(
  value: string | number | boolean,
  raw?: string
): LiteralExpression {
  return {
    type: 'Literal',
    value,
    raw: raw ?? JSON.stringify(value),
    line: 0,
    column: 0
  }
}

/**
 * Variable name manager for coordinating global and local variable names
 * - Global variables use a shared naming pool with 'global' prefix for invalid identifiers
 * - Local variables use a sprite-specific pool with 'local' prefix for invalid identifiers
 * - Valid identifiers can be used as-is if no conflict exists
 * - Manages variable to name mappings to ensure consistency across contexts
 * - Uses separate Maps for global and local variables to enable name reuse across sprites
 */
export class VariableNameManager {
  // Shared across all decompilers for global variables
  private globalVariableNames: Map<Variable, string> = new Map()
  // Sprite-specific for local variables - allows name reuse across different sprites
  private localVariableNames: Map<Variable, string> = new Map()

  /**
   * Get all used names in the specified scope
   */
  private getUsedNames(isGlobal: boolean): Set<string> {
    const used = new Set<string>()

    // Global names are always reserved
    for (const name of this.globalVariableNames.values()) {
      used.add(name)
    }

    // For local scope, also include local names
    if (!isGlobal) {
      for (const name of this.localVariableNames.values()) {
        used.add(name)
      }
    }

    return used
  }

  /**
   * Check if a name is already used in the appropriate scope
   */
  isUsed(name: string, isGlobal: boolean): boolean {
    const usedNames = this.getUsedNames(isGlobal)
    return usedNames.has(name)
  }

  /**
   * Generate a unique name for a variable
   */
  private generateName(preferredName: string, isGlobal: boolean): string {
    // Check if the preferred name is a valid identifier
    if (!isValidIdentifier(preferredName)) {
      // Generate prefixed name
      const prefix = isGlobal ? 'global' : 'local'
      let counter = 1
      let generatedName = `${prefix}${counter}`
      while (this.isUsed(generatedName, isGlobal)) {
        counter++
        generatedName = `${prefix}${counter}`
      }
      return generatedName
    }

    // Check if the preferred name is available
    if (!this.isUsed(preferredName, isGlobal)) {
      return preferredName
    }

    // Name conflict - generate prefixed name
    const prefix = isGlobal ? 'global' : 'local'
    let counter = 1
    let generatedName = `${prefix}${counter}`
    while (this.isUsed(generatedName, isGlobal)) {
      counter++
      generatedName = `${prefix}${counter}`
    }
    return generatedName
  }

  /**
   * Assign a name to a variable and store the mapping
   * Returns the assigned name
   */
  assignVariableName(
    variable: Variable,
    preferredName: string,
    isGlobal: boolean
  ): string {
    // Check if already assigned
    const existingGlobal = this.globalVariableNames.get(variable)
    if (existingGlobal !== undefined) {
      return existingGlobal
    }
    const existingLocal = this.localVariableNames.get(variable)
    if (existingLocal !== undefined) {
      return existingLocal
    }

    // Generate and store the name
    const generatedName = this.generateName(preferredName, isGlobal)
    if (isGlobal) {
      this.globalVariableNames.set(variable, generatedName)
    } else {
      this.localVariableNames.set(variable, generatedName)
    }
    return generatedName
  }

  /**
   * Get the assigned name for a variable
   * Returns undefined if the variable has not been assigned a name
   */
  getVariableName(variable: Variable): string | undefined {
    // Check global first, then local
    return (
      this.globalVariableNames.get(variable) ??
      this.localVariableNames.get(variable)
    )
  }

  /**
   * Create a new local scope manager that shares the global scope
   * This allows different sprites to reuse local variable names
   */
  createLocalScope(): VariableNameManager {
    const manager = new VariableNameManager()
    manager.globalVariableNames = this.globalVariableNames // Share global variable mappings
    // manager.localVariableNames remains separate - allows name reuse across sprites
    return manager
  }
}

/**
 * Decompiler context for tracking variable names and namespaces
 */
export interface DecompilerContext {
  globalScope: Scope
  namespaces: Map<string, Namespace>
  // Map to store auto-generated namespace entries
  generatedNamespaces: Map<string, Namespace>
  // Variable name manager for coordinating global and local names (also manages variable mappings)
  nameManager: VariableNameManager
  // Registry mapping proccode to compiled function (with assigned function names)
  functionRegistry: Map<string, CompiledFunctionWithDefault>
  // Map from argument ID/name to generated parameter name (for current function)
  currentFunctionParams: Map<string, string>
}

/**
 * Parse proccode to extract function information
 * Returns null if proccode doesn't match compiler format
 */
function parseProccode(proccode: string): {
  name: string
  params: Array<{ name: string; type: 'any' | 'bool' }>
} | null {
  // Compiler format: functionName(param1 = %s, param2 = %b)
  // Use negative lookbehind to avoid matching escaped %% sequences
  const match = proccode.match(/^([a-zA-Z_][a-zA-Z0-9_]*)\((.*)\)$/)
  if (!match) return null

  const name = match[1]
  const paramsStr = match[2].trim()

  if (!paramsStr) {
    return { name, params: [] }
  }

  const params: Array<{ name: string; type: 'any' | 'bool' }> = []
  const paramParts = paramsStr.split(',')

  for (const part of paramParts) {
    const trimmed = part.trim()
    // Match = %s or = %b, but not = %%s or = %%b
    const paramMatch = trimmed.match(/^(.+?)\s*=\s*(?<!%)%([sb])$/)
    if (!paramMatch) return null

    const paramName = paramMatch[1].trim()
    const paramType = paramMatch[2] === 'b' ? 'bool' : 'any'
    params.push({ name: paramName, type: paramType })
  }

  return { name, params }
}

/**
 * Parse proccode to extract argument types in order
 * Returns array of argument types ('any' for %s, 'bool' for %b)
 * Example: "aaa %s bbb %b ccc" returns ['any', 'bool']
 * Note: Escaped sequences like %%s or %%b are not treated as parameters
 * FIXME: Also used in @scratch-fuse/serializer, migrate to @scratch-fuse/utility?
 */
function parseProccodeArgumentTypes(proccode: string): ('any' | 'bool')[] {
  const types: ('any' | 'bool')[] = []
  // Use negative lookbehind to match %s or %b but not %%s or %%b
  const paramRegex = /(?<!%)%([sb])/g
  let match: RegExpExecArray | null

  while ((match = paramRegex.exec(proccode)) !== null) {
    const paramType = match[1] === 'b' ? 'bool' : 'any'
    types.push(paramType)
  }

  return types
}

/**
 * Generate parameter names avoiding conflicts
 * If parameter name conflicts with variables or is invalid, use arg1, arg2, etc.
 */
function generateParameterNames(
  params: Array<{ name: string; type: 'any' | 'bool' }>,
  functionVariables: Set<string>
): Array<{ name: string; originalName: string; type: 'any' | 'bool' }> {
  const usedNames = new Set<string>(functionVariables)
  const result: Array<{
    name: string
    originalName: string
    type: 'any' | 'bool'
  }> = []

  for (let i = 0; i < params.length; i++) {
    const param = params[i]
    const originalName = param.name

    // Check if name is valid identifier and doesn't conflict with variables
    if (!isValidIdentifier(originalName) || usedNames.has(originalName)) {
      // Use arg1, arg2, etc. for invalid identifiers or conflicts
      let counter = 1
      let generatedName = `arg${counter}`
      while (usedNames.has(generatedName)) {
        counter++
        generatedName = `arg${counter}`
      }
      usedNames.add(generatedName)
      result.push({ name: generatedName, originalName, type: param.type })
    } else {
      // No conflict and valid identifier - use original name
      usedNames.add(originalName)
      result.push({ name: originalName, originalName, type: param.type })
    }
  }

  return result
}

/**
 * Parse export format proccode and combine with argumentNames
 * Format: "func arg %s argbool %b" with argumentNames: ["aa", "bb"]
 * Returns parameter info and template parts for constructing export name
 */
function parseExportProccode(
  proccode: string,
  argumentNames: string[]
): {
  params: Array<{ name: string; type: 'any' | 'bool' }>
  templateParts: Array<{ text: string; paramIndex: number | null }>
} | null {
  const params: Array<{ name: string; type: 'any' | 'bool' }> = []
  const templateParts: Array<{ text: string; paramIndex: number | null }> = []

  let currentPos = 0
  let paramIndex = 0
  // Use negative lookbehind to match %s or %b but not %%s or %%b
  const paramRegex = /(?<!%)%([sb])/g
  let match: RegExpExecArray | null

  while ((match = paramRegex.exec(proccode)) !== null) {
    // Add text before this parameter
    if (match.index > currentPos) {
      templateParts.push({
        text: proccode.slice(currentPos, match.index),
        paramIndex: null
      })
    }

    // Add parameter placeholder
    const paramType = match[1] === 'b' ? 'bool' : 'any'
    const paramName = argumentNames[paramIndex] || `arg${paramIndex + 1}`
    params.push({ name: paramName, type: paramType })
    templateParts.push({
      text: '', // Will be filled with [paramName] later
      paramIndex: paramIndex
    })

    currentPos = match.index + match[0].length
    paramIndex++
  }

  // Add remaining text
  if (currentPos < proccode.length) {
    templateParts.push({
      text: proccode.slice(currentPos),
      paramIndex: null
    })
  }

  // Check if we have the right number of arguments
  if (paramIndex !== argumentNames.length) {
    return null
  }

  return { params, templateParts }
}

/**
 * Extract namespace and member name from opcode
 * Example: pen_menu_colorParam -> { namespace: 'pen', member: 'menu_colorParam' }
 */
function parseOpcodeNamespace(
  opcode: string
): { namespace: string; member: string } | null {
  const parts = opcode.split('_')
  if (parts.length < 2) return null

  const namespace = parts[0]
  const member = parts.slice(1).join('_')

  return { namespace, member }
}

/**
 * Create a fallback call expression from a block/reporter when no namespace entry matches
 */
function createFallbackCall(
  block: Block | Reporter,
  namespace: string,
  member: string
): CallExpression {
  const args: Expression[] = []

  // Add all fields as arguments (with menu set to null)
  for (const [fieldName, fieldValue] of Object.entries(block.fields)) {
    args.push(makeLiteral(fieldValue))
  }

  // Add all inputs as arguments
  for (const [inputName, input] of Object.entries(block.inputs)) {
    if (input.type === 'any') {
      // This would need to be decompiled, but we can't call instance methods here
      // So we'll handle this in the Decompiler class methods
    } else if (input.type === 'bool') {
      // Same as above
    }
  }

  const memberExpr: MemberExpression = {
    type: 'MemberExpression',
    object: makeIdentifier(namespace),
    property: makeIdentifier(member),
    computed: false,
    line: 0,
    column: 0
  }

  const callExpr: CallExpression = {
    type: 'CallExpression',
    callee: memberExpr,
    arguments: args,
    line: 0,
    column: 0
  }

  return callExpr
}

/**
 * Decompiler class
 */
export class Decompiler {
  constructor(private context: DecompilerContext) {}

  get namespaces() {
    return this.context.generatedNamespaces
  }

  /**
   * Decompile a variable to VariableDeclaration
   * Also handles @export decorator if exportName differs from name
   */
  decompileVariable(
    variable: Variable,
    value: string | number | boolean | (string | number | boolean)[]
  ): VariableDeclaration | DecoratorStatement {
    // Get pre-generated variable name from context
    const name = variable.name

    let initializer: Expression
    if (Array.isArray(value)) {
      const arrayExpr: ArrayExpression = {
        type: 'ArrayExpression',
        elements: value.map(v => makeLiteral(v)),
        line: 0,
        column: 0
      }
      initializer = arrayExpr
    } else {
      initializer = makeLiteral(value)
    }

    if (variable.exportName) {
      const varDecl: VariableDeclaration = {
        type: 'VariableDeclaration',
        name,
        isGlobal: variable.isGlobal,
        initializer,
        line: 0,
        column: 0
      }
      const decorator: DecoratorStatement = {
        type: 'DecoratorStatement',
        name: makeIdentifier('export'),
        arguments: [makeLiteral(variable.exportName)],
        target: varDecl,
        line: 0,
        column: 0
      }
      return decorator
    }

    return {
      type: 'VariableDeclaration',
      name,
      isGlobal: variable.isGlobal,
      initializer,
      line: 0,
      column: 0
    }
  }

  /**
   * Decompile a reporter (value block) to Expression
   */
  decompileReporter(reporter: Reporter | string): Expression {
    if (typeof reporter === 'string') {
      // Try to parse as number
      const num = Number(reporter)
      if (!isNaN(num) && String(num) === reporter) {
        return makeLiteral(num, reporter)
      }
      return makeLiteral(reporter)
    }

    const { opcode, fields, inputs } = reporter

    // Handle variable references
    if (opcode === 'data_variable') {
      const varName = fields.VARIABLE
      // Find variable by exportName or name
      for (const [_, variable] of this.context.globalScope.variables) {
        if (
          (variable.exportName && variable.exportName === varName) ||
          variable.name === varName
        ) {
          const generatedName =
            this.context.nameManager.getVariableName(variable)
          if (!generatedName) {
            throw new DecompilerError(`Variable name not assigned: ${varName}`)
          }
          return makeIdentifier(generatedName)
        }
      }
      throw new DecompilerError(`Variable not found: ${varName}`)
    }

    // Handle list contents
    if (opcode === 'data_listcontents') {
      const listName = fields.LIST
      for (const [_, variable] of this.context.globalScope.variables) {
        if (
          (variable.exportName && variable.exportName === listName) ||
          variable.name === listName
        ) {
          const generatedName =
            this.context.nameManager.getVariableName(variable)
          if (!generatedName) {
            throw new DecompilerError(
              `List variable name not assigned: ${listName}`
            )
          }
          return makeIdentifier(generatedName)
        }
      }
      throw new DecompilerError(`List variable not found: ${listName}`)
    }

    // Handle procedure arguments
    if (opcode === 'argument_reporter_string_number') {
      const argId = fields.VALUE
      // Try to resolve from current function parameter context
      const paramName = this.context.currentFunctionParams.get(argId)
      if (paramName) {
        return makeIdentifier(paramName)
      }
      // Fallback to using the field value directly
      return makeIdentifier(argId)
    }

    if (opcode === 'argument_reporter_boolean') {
      const argId = fields.VALUE
      // Try to resolve from current function parameter context
      const paramName = this.context.currentFunctionParams.get(argId)
      if (paramName) {
        return makeIdentifier(paramName)
      }
      // Fallback to using the field value directly
      return makeIdentifier(argId)
    }

    // Handle operators
    switch (opcode) {
      case 'operator_add': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.NUM1 as AnyInput).value),
          operator: '+',
          right: this.decompileReporter((inputs.NUM2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_subtract': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.NUM1 as AnyInput).value),
          operator: '-',
          right: this.decompileReporter((inputs.NUM2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_multiply': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.NUM1 as AnyInput).value),
          operator: '*',
          right: this.decompileReporter((inputs.NUM2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_divide': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.NUM1 as AnyInput).value),
          operator: '/',
          right: this.decompileReporter((inputs.NUM2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_mod': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.NUM1 as AnyInput).value),
          operator: '%',
          right: this.decompileReporter((inputs.NUM2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_join': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.STRING1 as AnyInput).value),
          operator: '..',
          right: this.decompileReporter((inputs.STRING2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_equals': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.OPERAND1 as AnyInput).value),
          operator: '==',
          right: this.decompileReporter((inputs.OPERAND2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_lt': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.OPERAND1 as AnyInput).value),
          operator: '<',
          right: this.decompileReporter((inputs.OPERAND2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_gt': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.OPERAND1 as AnyInput).value),
          operator: '>',
          right: this.decompileReporter((inputs.OPERAND2 as AnyInput).value),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_and': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: inputs.OPERAND1
            ? this.decompileReporter((inputs.OPERAND1 as BooleanInput).value)
            : makeLiteral(false), // Fallback if missing
          operator: '&&',
          right: inputs.OPERAND2
            ? this.decompileReporter((inputs.OPERAND2 as BooleanInput).value)
            : makeLiteral(false), // Fallback if missing
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_or': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: inputs.OPERAND1
            ? this.decompileReporter((inputs.OPERAND1 as BooleanInput).value)
            : makeLiteral(false), // Fallback if missing
          operator: '||',
          right: inputs.OPERAND2
            ? this.decompileReporter((inputs.OPERAND2 as BooleanInput).value)
            : makeLiteral(false), // Fallback if missing
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_not': {
        const operand = (inputs.OPERAND as BooleanInput)?.value

        if (!operand) {
          return makeLiteral(true) // Fallback
        }

        // Check if this is actually !=, <=, or >=
        if (
          typeof operand !== 'string' &&
          operand.opcode === 'operator_equals'
        ) {
          // This is != (NOT equals)
          const innerExpr: BinaryExpression = {
            type: 'BinaryExpression',
            left: this.decompileReporter(
              (operand.inputs.OPERAND1 as AnyInput).value
            ),
            operator: '!=',
            right: this.decompileReporter(
              (operand.inputs.OPERAND2 as AnyInput).value
            ),
            line: 0,
            column: 0
          }
          return innerExpr
        } else if (
          typeof operand !== 'string' &&
          operand.opcode === 'operator_gt'
        ) {
          // This is <= (NOT greater than)
          const innerExpr: BinaryExpression = {
            type: 'BinaryExpression',
            left: this.decompileReporter(
              (operand.inputs.OPERAND1 as AnyInput).value
            ),
            operator: '<=',
            right: this.decompileReporter(
              (operand.inputs.OPERAND2 as AnyInput).value
            ),
            line: 0,
            column: 0
          }
          return innerExpr
        } else if (
          typeof operand !== 'string' &&
          operand.opcode === 'operator_lt'
        ) {
          // This is >= (NOT less than)
          const innerExpr: BinaryExpression = {
            type: 'BinaryExpression',
            left: this.decompileReporter(
              (operand.inputs.OPERAND1 as AnyInput).value
            ),
            operator: '>=',
            right: this.decompileReporter(
              (operand.inputs.OPERAND2 as AnyInput).value
            ),
            line: 0,
            column: 0
          }
          return innerExpr
        }

        // Regular NOT operator
        const expr: UnaryExpression = {
          type: 'UnaryExpression',
          operator: '!',
          operand: this.decompileReporter(operand),
          line: 0,
          column: 0
        }
        return expr
      }

      // List operations as member expressions
      case 'data_itemoflist': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.resolveListVariable(fields.LIST),
          property: this.decompileReporter((inputs.INDEX as AnyInput).value),
          computed: true,
          line: 0,
          column: 0
        }
        return memberExpr
      }

      case 'data_lengthoflist': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.resolveListVariable(fields.LIST),
          property: makeIdentifier('length'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [],
          line: 0,
          column: 0
        }
        return callExpr
      }

      case 'data_listcontainsitem': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.resolveListVariable(fields.LIST),
          property: makeIdentifier('includes'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [this.decompileReporter((inputs.ITEM as AnyInput).value)],
          line: 0,
          column: 0
        }
        return callExpr
      }

      case 'data_itemnumoflist': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.resolveListVariable(fields.LIST),
          property: makeIdentifier('indexOf'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [this.decompileReporter((inputs.ITEM as AnyInput).value)],
          line: 0,
          column: 0
        }
        return callExpr
      }

      // String methods
      case 'operator_letter_of': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.decompileReporter((inputs.STRING as AnyInput).value),
          property: this.decompileReporter((inputs.LETTER as AnyInput).value),
          computed: true,
          line: 0,
          column: 0
        }
        return memberExpr
      }

      case 'operator_length': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.decompileReporter((inputs.STRING as AnyInput).value),
          property: makeIdentifier('length'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [],
          line: 0,
          column: 0
        }
        return callExpr
      }

      case 'operator_contains': {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.decompileReporter((inputs.STRING1 as AnyInput).value),
          property: makeIdentifier('includes'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [
            this.decompileReporter((inputs.STRING2 as AnyInput).value)
          ],
          line: 0,
          column: 0
        }
        return callExpr
      }
    }

    // Handle procedure calls as reporters
    if (opcode === 'procedures_call') {
      const result = this.decompileProcedureCall(reporter)
      if (!result) return makeLiteral(0) // Fallback
      return result
    }

    // Try to match namespace entry (with fallback)
    const namespaceMatch = this.tryMatchNamespace(reporter)
    if (namespaceMatch) {
      return namespaceMatch.value
    }

    // If still no match, throw error
    throw new DecompilerError(
      `Cannot decompile reporter with opcode: ${opcode}. ` +
        `Unable to parse as namespace call.`
    )
  }

  /**
   * Resolve a list variable by its export name or name
   */
  private resolveListVariable(name: string): Expression {
    for (const [_, variable] of this.context.globalScope.variables) {
      if (
        (variable.exportName && variable.exportName === name) ||
        variable.name === name
      ) {
        const generatedName = this.context.nameManager.getVariableName(variable)
        if (!generatedName) {
          throw new Error(`Variable name not assigned for: ${name}`)
        }
        return makeIdentifier(generatedName)
      }
    }
    throw new Error(`Unable to resolve ${name}`)
  }

  /**
   * Common logic to match a block/reporter to a namespace entry from provided namespaces
   */
  private tryMatchNamespaceFromMap(
    block: Block | Reporter,
    namespaces: Map<string, Namespace>,
    supportSubstack: boolean
  ): NamespaceMatch | null {
    for (const [nsName, namespace] of namespaces) {
      for (const [memberName, entry] of namespace) {
        if (entry.opcode === block.opcode) {
          // Try to match fields and inputs
          if (supportSubstack) {
            const matched = this.tryMatchEntryWithSubstack(
              block as Block,
              entry
            )
            if (matched) {
              const memberExpr: MemberExpression = {
                type: 'MemberExpression',
                object: makeIdentifier(nsName),
                property: makeIdentifier(memberName),
                computed: false,
                line: 0,
                column: 0
              }
              const callExpr: CallExpression = {
                type: 'CallExpression',
                callee: memberExpr,
                arguments: matched.args,
                then: matched.then,
                line: 0,
                column: 0
              }
              return {
                type: entry.type,
                value: callExpr
              }
            }
          } else {
            const matched = this.tryMatchEntry(block, entry)
            if (matched) {
              const memberExpr: MemberExpression = {
                type: 'MemberExpression',
                object: makeIdentifier(nsName),
                property: makeIdentifier(memberName),
                computed: false,
                line: 0,
                column: 0
              }
              const callExpr: CallExpression = {
                type: 'CallExpression',
                callee: memberExpr,
                arguments: matched,
                line: 0,
                column: 0
              }
              return {
                type: entry.type,
                value: callExpr
              }
            }
          }
        }
      }
    }
    return null
  }

  /**
   * Generate a dynamic namespace call from opcode and save to generatedNamespaces
   */
  private generateDynamicNamespaceCall(
    block: Block | Reporter,
    entryType: 'any' | 'bool' | 'void' | 'hat'
  ): NamespaceMatch | null {
    const parsed = parseOpcodeNamespace(block.opcode)
    if (!parsed) return null

    const args: Expression[] = []
    let thenBlock: BlockStatement | undefined
    const generatedArgs: (
      | NamespaceEntryArgumentAny
      | NamespaceEntryArgumentBoolean
      | NamespaceEntryArgumentSubstack
      | NamespaceEntryArgumentField
    )[] = []

    // Add all fields as arguments
    for (const [fieldName, fieldValue] of Object.entries(block.fields)) {
      args.push(makeLiteral(fieldValue))
      generatedArgs.push({ type: 'field', name: fieldName, menu: null })
    }

    // Add all inputs as arguments
    for (const [name, input] of Object.entries(block.inputs)) {
      if (input.type === 'any') {
        args.push(this.decompileReporter((input as AnyInput).value))
        generatedArgs.push({ type: 'any', name })
      } else if (input.type === 'bool') {
        args.push(this.decompileReporter((input as BooleanInput).value))
        generatedArgs.push({ type: 'bool', name })
      } else if (input.type === 'substack') {
        thenBlock = {
          type: 'BlockStatement',
          body: this.tryConvertToForLoop(
            (input as SubstackInput).value.map(b => this.decompileBlock(b))
          ),
          line: 0,
          column: 0
        }
        generatedArgs.push({ type: 'substack', name })
      }
    }

    // Save to generatedNamespaces
    if (!this.context.generatedNamespaces.has(parsed.namespace)) {
      this.context.generatedNamespaces.set(parsed.namespace, new Map())
    }
    const ns = this.context.generatedNamespaces.get(parsed.namespace)!
    if (!ns.has(parsed.member)) {
      // Create a namespace entry for this generated call
      const entry: NamespaceEntry = {
        type: entryType,
        opcode: block.opcode,
        args: generatedArgs
      }
      ns.set(parsed.member, entry)
    }

    const memberExpr: MemberExpression = {
      type: 'MemberExpression',
      object: makeIdentifier(parsed.namespace),
      property: makeIdentifier(parsed.member),
      computed: false,
      line: 0,
      column: 0
    }

    const callExpr: CallExpression = {
      type: 'CallExpression',
      callee: memberExpr,
      arguments: args,
      then: thenBlock,
      line: 0,
      column: 0
    }

    return {
      type: entryType,
      value: callExpr
    }
  }

  /**
   * Try to match a block/reporter to a namespace entry (for reporters)
   */
  private tryMatchNamespace(block: Block | Reporter): NamespaceMatch | null {
    // First try to match from user-provided namespaces
    const matched = this.tryMatchNamespaceFromMap(
      block,
      this.context.namespaces,
      false
    )
    if (matched) return matched

    // Fallback: try to parse opcode and create dynamic call
    return this.generateDynamicNamespaceCall(block, 'any')
  }

  /**
   * Try to match block inputs/fields to namespace entry args
   */
  private tryMatchEntry(
    block: Block | Reporter,
    entry: NamespaceEntry
  ): Expression[] | null {
    const args: Expression[] = []

    if (entry.inputs) {
      for (const [name, input] of Object.entries(entry.inputs)) {
        if (typeof input.value === 'object') {
          if (
            JSON.stringify(block.inputs[name]) !== JSON.stringify(input.value)
          ) {
            return null
          }
        } else {
          if (block.inputs[name].type !== input.type) return null
          if (
            block.inputs[name].type === 'any' &&
            input.type === 'any' &&
            block.inputs[name].value === input.value
          ) {
            // Match any with specific value
            continue
          }
          return null
        }
      }
    }
    if (entry.fields) {
      for (const [name, value] of Object.entries(entry.fields)) {
        if (block.fields[name] !== value) return null
      }
    }

    for (const arg of entry.args) {
      if (arg.type === 'field') {
        // Match field
        const fieldValue = block.fields[arg.name]
        if (fieldValue === undefined) return null
        const menu = arg.menu
        if (menu) {
          // Translate to key
          const menuEntry = Object.entries(menu).find(e => e[1] === fieldValue)
          if (!menuEntry) return null
          args.push(makeLiteral(menuEntry[0]))
        } else args.push(makeLiteral(fieldValue))
      } else if (arg.type === 'any') {
        // Match any input
        const input = block.inputs[arg.name] as AnyInput | undefined
        if (!input) return null
        args.push(this.decompileReporter(input.value))
      } else if (arg.type === 'bool') {
        // Match bool input
        const input = block.inputs[arg.name] as BooleanInput | undefined
        if (!input) return null
        args.push(this.decompileReporter(input.value))
      } else if (arg.type === 'substack') {
        // Can't match substack in reporter
        return null
      }
    }

    return args
  }

  /**
   * Decompile a block to Statement
   */
  decompileBlock(block: Block): Statement {
    const { opcode, fields, inputs } = block

    // Handle variable assignment
    if (opcode === 'data_setvariableto') {
      const varName = fields.VARIABLE
      const value = this.decompileReporter((inputs.VALUE as AnyInput).value)
      const stmt: AssignmentStatement = {
        type: 'AssignmentStatement',
        left: this.resolveListVariable(varName),
        operator: '=',
        right: value,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'data_changevariableby') {
      const varName = fields.VARIABLE
      const value = this.decompileReporter((inputs.VALUE as AnyInput).value)

      // Check if value is literal 1, if so use increment operator
      if (
        value.type === 'Literal' &&
        (value as LiteralExpression).value === 1
      ) {
        const stmt: IncrementStatement = {
          type: 'IncrementStatement',
          operator: '++',
          target: this.resolveListVariable(varName),
          line: 0,
          column: 0
        }
        return stmt
      }

      const stmt: AssignmentStatement = {
        type: 'AssignmentStatement',
        left: this.resolveListVariable(varName),
        operator: '+=',
        right: value,
        line: 0,
        column: 0
      }
      return stmt
    }

    // Handle control flow
    if (opcode === 'control_if') {
      const condition = inputs.CONDITION
        ? this.decompileReporter((inputs.CONDITION as BooleanInput).value)
        : makeLiteral(false)
      const thenBlocks: Block[] | undefined = (inputs.SUBSTACK as SubstackInput)
        ?.value
      const thenBody: BlockStatement | undefined = thenBlocks
        ? {
            type: 'BlockStatement',
            body: this.tryConvertToForLoop(
              thenBlocks.map(b => this.decompileBlock(b))
            ),
            line: 0,
            column: 0
          }
        : undefined

      const stmt: IfStatement = {
        type: 'IfStatement',
        condition,
        then: thenBody ?? {
          type: 'NoopStatement',
          line: 0,
          column: 0
        },
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'control_if_else') {
      const condition = inputs.CONDITION
        ? this.decompileReporter((inputs.CONDITION as BooleanInput).value)
        : makeLiteral(false)
      const thenBlocks = (inputs.SUBSTACK as SubstackInput | undefined)?.value
      const elseBlocks = (inputs.SUBSTACK2 as SubstackInput | undefined)?.value

      const thenBody: BlockStatement | undefined = thenBlocks
        ? {
            type: 'BlockStatement',
            body: this.tryConvertToForLoop(
              thenBlocks.map(b => this.decompileBlock(b))
            ),
            line: 0,
            column: 0
          }
        : undefined

      const elseBody: BlockStatement | undefined = elseBlocks
        ? {
            type: 'BlockStatement',
            body: this.tryConvertToForLoop(
              elseBlocks.map(b => this.decompileBlock(b))
            ),
            line: 0,
            column: 0
          }
        : undefined

      const stmt: IfStatement = {
        type: 'IfStatement',
        condition,
        then: thenBody ?? {
          type: 'NoopStatement',
          line: 0,
          column: 0
        },
        else: elseBody,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'control_forever') {
      const body = (inputs.SUBSTACK as SubstackInput).value
      const blockStmt: BlockStatement = {
        type: 'BlockStatement',
        body: this.tryConvertToForLoop(body.map(b => this.decompileBlock(b))),
        line: 0,
        column: 0
      }
      const stmt: LoopStatement = {
        type: 'LoopStatement',
        body: blockStmt,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'control_while') {
      const condition = this.decompileReporter(
        (inputs.CONDITION as BooleanInput).value
      )
      const body = (inputs.SUBSTACK as SubstackInput).value

      const blockStmt: BlockStatement = {
        type: 'BlockStatement',
        body: this.tryConvertToForLoop(body.map(b => this.decompileBlock(b))),
        line: 0,
        column: 0
      }
      const stmt: WhileStatement = {
        type: 'WhileStatement',
        condition,
        body: blockStmt,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'control_stop') {
      const stopOption = fields.STOP_OPTION
      if (stopOption === 'this script') {
        const stmt: ReturnStatement = {
          type: 'ReturnStatement',
          line: 0,
          column: 0
        }
        return stmt
      }
      // For other stop options, generate namespace call
    }

    // List operations
    if (opcode === 'data_addtolist') {
      const listName = fields.LIST
      const item = this.decompileReporter((inputs.ITEM as AnyInput).value)
      const memberExpr: MemberExpression = {
        type: 'MemberExpression',
        object: this.resolveListVariable(listName),
        property: makeIdentifier('push'),
        computed: false,
        line: 0,
        column: 0
      }
      const callExpr: CallExpression = {
        type: 'CallExpression',
        callee: memberExpr,
        arguments: [item],
        line: 0,
        column: 0
      }
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'data_deleteoflist') {
      const listName = fields.LIST
      const index = inputs.INDEX
        ? this.decompileReporter((inputs.INDEX as AnyInput).value)
        : makeLiteral('last')

      // Check if it's pop (last)
      if (
        typeof (inputs.INDEX as AnyInput).value === 'string' &&
        (inputs.INDEX as AnyInput).value === 'last'
      ) {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.resolveListVariable(listName),
          property: makeIdentifier('pop'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [],
          line: 0,
          column: 0
        }
        const stmt: ExpressionStatement = {
          type: 'ExpressionStatement',
          expression: callExpr,
          line: 0,
          column: 0
        }
        return stmt
      } else {
        const memberExpr: MemberExpression = {
          type: 'MemberExpression',
          object: this.resolveListVariable(listName),
          property: makeIdentifier('remove'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [index],
          line: 0,
          column: 0
        }
        const stmt: ExpressionStatement = {
          type: 'ExpressionStatement',
          expression: callExpr,
          line: 0,
          column: 0
        }
        return stmt
      }
    }

    if (opcode === 'data_insertatlist') {
      const listName = fields.LIST
      const index = this.decompileReporter((inputs.INDEX as AnyInput).value)
      const item = this.decompileReporter((inputs.ITEM as AnyInput).value)
      const memberExpr: MemberExpression = {
        type: 'MemberExpression',
        object: this.resolveListVariable(listName),
        property: makeIdentifier('insert'),
        computed: false,
        line: 0,
        column: 0
      }
      const callExpr: CallExpression = {
        type: 'CallExpression',
        callee: memberExpr,
        arguments: [index, item],
        line: 0,
        column: 0
      }
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'data_replaceitemoflist') {
      const listName = fields.LIST
      const index = this.decompileReporter((inputs.INDEX as AnyInput).value)
      const item = this.decompileReporter((inputs.ITEM as AnyInput).value)
      const memberExpr: MemberExpression = {
        type: 'MemberExpression',
        object: this.resolveListVariable(listName),
        property: index,
        computed: true,
        line: 0,
        column: 0
      }
      const stmt: AssignmentStatement = {
        type: 'AssignmentStatement',
        left: memberExpr,
        operator: '=',
        right: item,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'data_deletealloflist') {
      const listName = fields.LIST
      const memberExpr: MemberExpression = {
        type: 'MemberExpression',
        object: this.resolveListVariable(listName),
        property: makeIdentifier('clear'),
        computed: false,
        line: 0,
        column: 0
      }
      const callExpr: CallExpression = {
        type: 'CallExpression',
        callee: memberExpr,
        arguments: [],
        line: 0,
        column: 0
      }
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'data_showlist' || opcode === 'data_showvariable') {
      const varName = fields.LIST || fields.VARIABLE
      const memberExpr: MemberExpression = {
        type: 'MemberExpression',
        object: this.resolveListVariable(varName),
        property: makeIdentifier('show'),
        computed: false,
        line: 0,
        column: 0
      }
      const callExpr: CallExpression = {
        type: 'CallExpression',
        callee: memberExpr,
        arguments: [],
        line: 0,
        column: 0
      }
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'data_hidelist' || opcode === 'data_hidevariable') {
      const varName = fields.LIST || fields.VARIABLE
      const memberExpr: MemberExpression = {
        type: 'MemberExpression',
        object: this.resolveListVariable(varName),
        property: makeIdentifier('hide'),
        computed: false,
        line: 0,
        column: 0
      }
      const callExpr: CallExpression = {
        type: 'CallExpression',
        callee: memberExpr,
        arguments: [],
        line: 0,
        column: 0
      }
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }
    if (opcode === 'procedures_return') {
      const returnValue = this.decompileReporter(
        (inputs.VALUE as AnyInput | BooleanInput).value
      )
      const stmt: ReturnStatement = {
        type: 'ReturnStatement',
        value: returnValue,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'procedures_call') {
      const callExpr = this.decompileProcedureCall(block)
      if (!callExpr)
        return {
          type: 'NoopStatement',
          line: 0,
          column: 0
        }
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }

    // Try to match namespace (with fallback)
    const namespaceMatch = this.tryMatchNamespaceBlock(block)
    if (namespaceMatch) {
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: namespaceMatch.value,
        line: 0,
        column: 0
      }
      return stmt
    }

    // If still no match, throw error
    throw new DecompilerError(
      `Cannot decompile block with opcode: ${opcode}. ` +
        `Unable to parse as namespace call.`
    )
  }

  /**
   * Try to match a command block to namespace (for command blocks)
   */
  private tryMatchNamespaceBlock(block: Block): NamespaceMatch | null {
    // First try to match from user-provided namespaces
    const matched = this.tryMatchNamespaceFromMap(
      block,
      this.context.namespaces,
      true
    )
    if (matched) return matched

    // Fallback: try to parse opcode and create dynamic call
    return this.generateDynamicNamespaceCall(block, 'void')
  }

  /**
   * Match entry with substack support
   */
  private tryMatchEntryWithSubstack(
    block: Block,
    entry: NamespaceEntry
  ): { args: Expression[]; then?: BlockStatement } | null {
    const args: Expression[] = []
    let thenBlock: BlockStatement | undefined

    for (const arg of entry.args) {
      if (arg.type === 'field') {
        // Match field
        const fieldValue = block.fields[arg.name]
        if (fieldValue === undefined) return null
        const menu = arg.menu
        if (menu) {
          // Translate to key
          const menuEntry = Object.entries(menu).find(e => e[1] === fieldValue)
          if (!menuEntry) return null
          args.push(makeLiteral(menuEntry[0]))
        } else args.push(makeLiteral(fieldValue))
      } else if (arg.type === 'any') {
        const input = block.inputs[arg.name] as AnyInput | undefined
        if (!input) return null
        args.push(this.decompileReporter(input.value))
      } else if (arg.type === 'bool') {
        const input = block.inputs[arg.name] as BooleanInput | undefined
        if (!input) return null
        args.push(this.decompileReporter(input.value))
      } else if (arg.type === 'substack') {
        const input = block.inputs[arg.name] as SubstackInput | undefined
        if (!input) return null
        thenBlock = {
          type: 'BlockStatement',
          body: this.tryConvertToForLoop(
            input.value.map(b => this.decompileBlock(b))
          ),
          line: 0,
          column: 0
        }
      }
    }

    return { args, then: thenBlock }
  }

  /**
   * Try to detect for loop pattern in consecutive statements
   * Pattern: init statement + while loop with increment at end
   */
  private tryConvertToForLoop(statements: Statement[]): Statement[] {
    const result: Statement[] = []
    let i = 0

    while (i < statements.length) {
      // Check if we have at least 2 statements
      if (i + 1 < statements.length) {
        const initStmt = statements[i]
        const nextStmt = statements[i + 1]

        // Check if init is an assignment and next is a while loop
        if (
          (initStmt.type === 'AssignmentStatement' ||
            initStmt.type === 'IncrementStatement') &&
          nextStmt.type === 'WhileStatement'
        ) {
          const assignment = initStmt as AssignmentStatement
          const whileStmt = nextStmt as WhileStatement

          // Init must be = assignment to a simple identifier
          const body =
            whileStmt.body.type === 'BlockStatement'
              ? (whileStmt.body as BlockStatement).body
              : [whileStmt.body]

          // Check if last statement of while body is increment of the same variable
          if (body.length > 0) {
            const lastStmt = body[body.length - 1]

            if (
              lastStmt.type === 'IncrementStatement' ||
              lastStmt.type === 'AssignmentStatement'
            ) {
              // We have a for loop pattern!
              // Create for statement
              const forStmt: ForStatement = {
                type: 'ForStatement',
                init: assignment,
                condition: whileStmt.condition,
                increment: lastStmt as AssignmentStatement | IncrementStatement,
                body:
                  body.length === 1
                    ? {
                        type: 'NoopStatement',
                        line: 0,
                        column: 0
                      }
                    : ({
                        type: 'BlockStatement',
                        body: body.slice(0, -1), // Remove the last increment statement
                        line: 0,
                        column: 0
                      } as BlockStatement),
                line: 0,
                column: 0
              }
              result.push(forStmt)
              i += 2 // Skip both init and while statements
              continue
            }
          }
        }
      }

      // No for loop pattern, just add the statement as-is
      result.push(statements[i])
      i++
    }

    return result
  }

  /**
   * Try to match a hat block to a namespace entry
   */
  private tryMatchHatBlock(hat: Block): CallExpression | null {
    for (const [nsName, namespace] of this.context.namespaces) {
      for (const [memberName, entry] of namespace) {
        if (entry.opcode === hat.opcode && entry.type === 'hat') {
          const matched = this.tryMatchEntry(hat, entry)
          if (matched) {
            const memberExpr: MemberExpression = {
              type: 'MemberExpression',
              object: makeIdentifier(nsName),
              property: makeIdentifier(memberName),
              computed: false,
              line: 0,
              column: 0
            }
            const callExpr: CallExpression = {
              type: 'CallExpression',
              callee: memberExpr,
              arguments: matched,
              line: 0,
              column: 0
            }
            return callExpr
          }
        }
      }
    }
    return null
  }

  /**
   * Decompile a script
   */
  decompileScript(script: Script): Statement[] {
    const statements = script.blocks.map(block => this.decompileBlock(block))
    // Try to detect and convert for loop patterns
    const processedStatements = this.tryConvertToForLoop(statements)

    // Check if script has a hat block
    if (script.hat) {
      const hatCall = this.tryMatchHatBlock(script.hat)
      if (hatCall) {
        // Wrap statements in the hat block's then clause
        hatCall.then = {
          type: 'BlockStatement',
          body: processedStatements,
          line: 0,
          column: 0
        }
        // Return the hat call as the only statement
        const hatStmt: ExpressionStatement = {
          type: 'ExpressionStatement',
          expression: hatCall,
          line: 0,
          column: 0
        }
        return [hatStmt]
      }
    }
    return []
  }

  /**
   * Decompile a procedure call (function call)
   */
  private decompileProcedureCall(
    block: Block | Reporter
  ): CallExpression | null {
    const mutation = block.mutation
    if (!mutation) {
      throw new DecompilerError('procedures_call block missing mutation')
    }

    const proccode = mutation.proccode as string
    if (!proccode) {
      throw new DecompilerError('procedures_call block missing proccode')
    }

    // Parse argumentnames from mutation
    const argumentidsStr = mutation.argumentids as string
    const argumentids: string[] = argumentidsStr
      ? JSON.parse(argumentidsStr)
      : []

    // Parse argument types from proccode
    const argumentTypes = parseProccodeArgumentTypes(proccode)

    const compiledFunction = this.context.functionRegistry.get(proccode)

    if (!compiledFunction) {
      // Function not yet decompiled or not found - throw error
      console.warn(
        `Cannot resolve export format procedure call: ${proccode}. ` +
          `Function must be decompiled before its calls can be resolved.`
      )
      return null
    }

    // Decompile arguments from inputs
    const args: Expression[] = []
    for (let i = 0; i < argumentids.length; i++) {
      const argId = argumentids[i]
      const input = block.inputs[argId]
      const expectedType = argumentTypes[i] || 'any'

      if (!input) {
        // Missing input - add default value based on default
        let defaultValue = compiledFunction.defaultValues[i]
        if (defaultValue !== undefined) {
          let defaultValueType = ['true', 'false'].includes(defaultValue)
            ? 'bool'
            : 'any'
          if (expectedType === 'bool' && defaultValueType !== 'bool') {
            // throw new DecompilerError(
            //   `Type mismatch for default value of argument ${argId} in procedure call ${proccode}. ` +
            //     `Expected ${expectedType}, got ${defaultValueType} (${JSON.stringify(defaultValue)})`
            // )
            defaultValueType = 'bool'
            defaultValue = 'false'
          }
          console.warn(
            `Missing input for argument ${argId} in procedure call ${proccode}. ` +
              `Using default value: ${defaultValue}`
          )
          if (defaultValueType === 'bool') {
            args.push(makeLiteral(defaultValue === 'true'))
          } else {
            args.push(makeLiteral(defaultValue))
          }
        } else {
          throw new DecompilerError(
            `Missing input for argument ${argId} in procedure call ${proccode}`
          )
        }
        continue
      }

      if (input.type === 'bool') {
        args.push(this.decompileReporter((input as BooleanInput).value))
      } else if (input.type === 'any') {
        args.push(this.decompileReporter((input as AnyInput).value))
      } else {
        throw new DecompilerError(`Unexpected input type for argument ${argId}`)
      }
    }

    // Found the function - use the assigned name from decl
    const functionName = compiledFunction.decl.name.name
    const callExpr: CallExpression = {
      type: 'CallExpression',
      callee: makeIdentifier(functionName),
      arguments: args,
      line: 0,
      column: 0
    }
    return callExpr
  }

  /**
   * Infer return type from procedures_return blocks in the function body
   */
  private inferReturnType(blocks: Block[]): 'void' | 'bool' | 'any' {
    const returnTypes = new Set<'bool' | 'any'>()

    const analyzeBlock = (block: Block) => {
      if (block.opcode === 'procedures_return') {
        const returnValue = (block.inputs.VALUE as AnyInput | BooleanInput)
          .value

        // Check if the return value is a boolean expression
        if (typeof returnValue !== 'string') {
          const isBooleanExpr = this.isBooleanExpression(returnValue)
          returnTypes.add(isBooleanExpr ? 'bool' : 'any')
        } else {
          // String literal - treat as any
          returnTypes.add('any')
        }
      }

      // Recursively check substack inputs
      for (const inputKey in block.inputs) {
        const input = block.inputs[inputKey]
        if (input && 'type' in input && input.type === 'substack') {
          const substackInput = input as SubstackInput
          substackInput.value.forEach(analyzeBlock)
        }
      }
    }

    blocks.forEach(analyzeBlock)

    // Determine final return type
    if (returnTypes.size === 0) {
      return 'void'
    } else if (returnTypes.size === 1 && returnTypes.has('bool')) {
      return 'bool'
    } else {
      return 'any'
    }
  }

  /**
   * Check if a reporter is a boolean expression
   */
  private isBooleanExpression(reporter: Reporter): boolean {
    const matched = this.tryMatchNamespaceFromMap(
      reporter,
      this.context.namespaces,
      false
    )
    return matched?.type === 'bool'
  }

  /**
   * Decompile function from proccode and blocks
   */
  decompileFunction(
    raw: CompiledFunction
  ): FunctionDeclaration | DecoratorStatement {
    let name: string
    let params: Parameter[]
    let generatedExportName: string | undefined

    // Get the function from registry - name should already be assigned
    const registeredFunction = this.context.functionRegistry.get(raw.proccode)
    if (!registeredFunction) {
      throw new DecompilerError(
        `Function not found in registry for proccode: ${raw.proccode}. ` +
          `Did you pass compiledFunctions to createDecompiler?`
      )
    }

    name = registeredFunction.decl.name.name

    // Try to parse as compiler format first
    const parsed = parseProccode(raw.proccode)
    const argumentNames = raw.decl.parameters.map(p => p.name.name)

    // Collect all variable names that should be avoided
    // This includes global and local variables accessible in this function
    const usedVarNames = new Set<string>()
    for (const [_, variable] of this.context.globalScope.variables) {
      const generatedName = this.context.nameManager.getVariableName(variable)
      if (generatedName) {
        usedVarNames.add(generatedName)
      }
    }

    if (parsed) {
      // Use original compiler format: functionName(param1 = %s, param2 = %b)

      const paramNames = generateParameterNames(parsed.params, usedVarNames)
      params = paramNames.map(p => ({
        name: makeIdentifier(p.name),
        type: makeIdentifier(p.type)
      }))
    } else if (argumentNames) {
      // Use export format: func arg %s argbool %b with argumentNames
      const exportParsed = parseExportProccode(raw.proccode, argumentNames)

      if (!exportParsed) {
        throw new DecompilerError(
          `Cannot parse export format proccode: ${raw.proccode} with ${argumentNames.length} arguments`
        )
      }

      // Function name already determined from pre-registration
      // (name is already set from preRegisteredName above)

      const paramNames = generateParameterNames(
        exportParsed.params,
        usedVarNames
      )
      params = paramNames.map(p => ({
        name: makeIdentifier(p.name),
        type: makeIdentifier(p.type)
      }))

      // Construct export name with generated parameter names in brackets
      // Example: "func arg [arg1] argbool [arg2]"
      let exportNameBuilder = ''
      for (const part of exportParsed.templateParts) {
        if (part.paramIndex === null) {
          exportNameBuilder += part.text
        } else {
          // Use the generated parameter name
          const generatedParamName = paramNames[part.paramIndex].name
          exportNameBuilder += `[${generatedParamName}]`
        }
      }
      // Clean up extra spaces: replace multiple spaces with a single space
      generatedExportName = exportNameBuilder
    } else {
      throw new DecompilerError(
        'Invalid decompileFunction call: must provide either compiler format proccode or argumentNames'
      )
    }

    // Infer return type from the function body
    const returnType = this.inferReturnType(raw.impl)

    // Set up parameter context for decompiling function body
    const oldParams = this.context.currentFunctionParams
    this.context.currentFunctionParams = new Map<string, string>()

    // Map argument IDs to generated parameter names
    for (let i = 0; i < argumentNames.length; i++) {
      const argId = argumentNames[i]
      const paramName = params[i].name.name
      this.context.currentFunctionParams.set(argId, paramName)
    }

    const body: BlockStatement = {
      type: 'BlockStatement',
      body: this.tryConvertToForLoop(
        raw.impl.map(block => this.decompileBlock(block))
      ),
      line: 0,
      column: 0
    }

    // Restore previous parameter context
    this.context.currentFunctionParams = oldParams

    const funcDecl: FunctionDeclaration = {
      type: 'FunctionDeclaration',
      name: makeIdentifier(name),
      parameters: params,
      returnType: makeIdentifier(returnType),
      once: false,
      body,
      line: 0,
      column: 0
    }

    // Add export decorator if needed
    if (generatedExportName) {
      // Note: The FunctionDeclaration type should have an exportName property
      // If it doesn't exist in the type definition, this will need to be added
      // For now, we'll use type assertion or add it as needed
      // (funcDecl as any).exportName = generatedExportName
      return {
        type: 'DecoratorStatement',
        name: makeIdentifier('export'),
        arguments: [makeLiteral(generatedExportName)],
        target: funcDecl,
        line: 0,
        column: 0
      }
    }

    return funcDecl
  }
}

/**
 * Helper to create a decompiler with proper context
 * @param globalScope The global scope
 * @param namespaces The namespace definitions
 * @param compiledFunctions The compiled functions
 * @param sharedNameManager Optional shared name manager for global variables (for multiple sprites)
 */
export function createDecompiler(
  globalScope: Scope,
  namespaces: Map<string, Namespace>,
  compiledFunctions: CompiledFunctionWithDefault[],
  sharedNameManager?: VariableNameManager
): Decompiler {
  const functionRegistry = new Map<string, CompiledFunctionWithDefault>()

  // Pre-register all functions and assign their names
  let functionCounter = 0
  for (const func of compiledFunctions) {
    const parsed = parseProccode(func.proccode)

    let assignedName: string
    if (parsed) {
      // Compiler format: use the parsed name
      assignedName = parsed.name
    } else {
      // Export format: generate func1, func2, etc.
      functionCounter++
      assignedName = `func${functionCounter}`
    }

    // Assign the name to the function declaration
    func.decl.name.name = assignedName

    // Register the function with assigned name
    functionRegistry.set(func.proccode, func)
  }

  // Use shared name manager or create a new one
  const nameManager = sharedNameManager
    ? sharedNameManager.createLocalScope()
    : new VariableNameManager()

  // Pre-process all variables from globalScope to generate their names
  for (const [_, variable] of globalScope.variables) {
    if (variable.name !== variable.exportName && variable.exportName) {
      nameManager.assignVariableName(variable, variable.name, variable.isGlobal)
      continue
    }
    const preferredName = variable.name
    const isGlobal = variable.isGlobal

    // Use the name manager to assign a unique name
    const generatedName = nameManager.assignVariableName(
      variable,
      preferredName,
      isGlobal
    )

    variable.name = generatedName // Update variable name to generated name
    if (preferredName !== generatedName) {
      variable.exportName = preferredName
    } else {
      variable.exportName = null
    }
  }

  const context: DecompilerContext = {
    globalScope,
    namespaces,
    generatedNamespaces: new Map<string, Namespace>(),
    nameManager,
    functionRegistry,
    currentFunctionParams: new Map()
  }
  return new Decompiler(context)
}
