/**
 * Decompiler for converting IR (Intermediate Representation) back to AST
 *
 * The decompiler takes compiled IR blocks and converts them back to FUSE AST nodes.
 *
 * Key features:
 * - Handles variable name conflicts and generates valid identifiers
 * - Detects compiler-generated function formats vs export formats
 * - Reconstructs complex operators (!=, <=, >=) from operator_not patterns
 * - Matches namespace entries for unknown blocks
 * - All generated tokens have line=0, column=0
 *
 * Usage:
 * ```typescript
 * import { createDecompiler, Scope, Namespace } from '@scratch-fuse/compiler'
 *
 * // Setup global scope and namespaces
 * const globalScope = new Scope(globalVariables)
 * const namespaces = new Map<string, Namespace>()
 *
 * // Create decompiler
 * const decompiler = createDecompiler(globalScope, namespaces)
 *
 * // Decompile a variable
 * const varDecl = decompiler.decompileVariable(variable, initialValue)
 *
 * // Decompile blocks to statements
 * const statements = blocks.map(block => decompiler.decompileBlock(block))
 *
 * // Decompile a function
 * const funcDecl = decompiler.decompileFunction(proccode, blocks, exportName)
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
  DecoratorStatement
} from '@scratch-fuse/core'

import { Scope, Namespace, NamespaceEntry, ProcedureArgument } from './compiler'

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
    raw: raw ?? String(value),
    line: 0,
    column: 0
  }
}

/**
 * Decompiler context for tracking variable names and namespaces
 */
export interface DecompilerContext {
  globalScope: Scope
  namespaces: Map<string, Namespace>
  // Track generated variable names to avoid conflicts
  usedNames: Set<string>
  // Map from Variable to generated name
  variableNames: Map<Variable, string>
  // Map from proccode to function name (for resolving procedure calls)
  proccodeToFunctionName: Map<string, string>
}

/**
 * Generate a valid variable name based on the variable info
 */
function generateVariableName(
  variable: Variable,
  context: DecompilerContext
): string {
  // Check if we already generated a name for this variable
  const existing = context.variableNames.get(variable)
  if (existing) return existing

  const preferredName = variable.name

  // Check if the name is a valid identifier
  if (!isValidIdentifier(preferredName)) {
    // Generate var1, var2, etc.
    let counter = 1
    let generatedName = `var${counter}`
    while (context.usedNames.has(generatedName)) {
      counter++
      generatedName = `var${counter}`
    }
    context.usedNames.add(generatedName)
    context.variableNames.set(variable, generatedName)
    return generatedName
  }

  // Check if there's a global variable with the same name
  if (variable.isGlobal) {
    // Global variable - use its name if not taken
    if (!context.usedNames.has(preferredName)) {
      context.usedNames.add(preferredName)
      context.variableNames.set(variable, preferredName)
      return preferredName
    }
  } else {
    // Local variable - check if global has this name
    const hasGlobalConflict = Array.from(
      context.globalScope.variables.values()
    ).some(v => v.name === preferredName && v.isGlobal)

    if (!hasGlobalConflict && !context.usedNames.has(preferredName)) {
      context.usedNames.add(preferredName)
      context.variableNames.set(variable, preferredName)
      return preferredName
    }
  }

  // Name conflict - generate var1, var2, etc.
  let counter = 1
  let generatedName = `var${counter}`
  while (context.usedNames.has(generatedName)) {
    counter++
    generatedName = `var${counter}`
  }
  context.usedNames.add(generatedName)
  context.variableNames.set(variable, generatedName)
  return generatedName
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
  // First unescape %% to %
  const unescaped = proccode.replace(/%%/g, '%')

  const match = unescaped.match(/^([a-zA-Z_][a-zA-Z0-9_]*)\((.*)\)$/)
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
    const paramMatch = trimmed.match(/^(.+?)\s*=\s*%([sb])$/)
    if (!paramMatch) return null

    const paramName = paramMatch[1].trim()
    const paramType = paramMatch[2] === 'b' ? 'bool' : 'any'
    params.push({ name: paramName, type: paramType })
  }

  return { name, params }
}

/**
 * Generate parameter names avoiding conflicts
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

    // Check if name is valid identifier
    if (!isValidIdentifier(originalName)) {
      // Use arg1, arg2, etc.
      let counter = 1
      let generatedName = `arg${counter}`
      while (usedNames.has(generatedName)) {
        counter++
        generatedName = `arg${counter}`
      }
      usedNames.add(generatedName)
      result.push({ name: generatedName, originalName, type: param.type })
      continue
    }

    // Check for conflicts
    if (usedNames.has(originalName)) {
      // Add number suffix
      let counter = 1
      let generatedName = `${originalName}${counter}`
      while (usedNames.has(generatedName)) {
        counter++
        generatedName = `${originalName}${counter}`
      }
      usedNames.add(generatedName)
      result.push({ name: generatedName, originalName, type: param.type })
    } else {
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
  // First unescape %% to %
  const unescaped = proccode.replace(/%%/g, '%')

  const params: Array<{ name: string; type: 'any' | 'bool' }> = []
  const templateParts: Array<{ text: string; paramIndex: number | null }> = []

  let currentPos = 0
  let paramIndex = 0
  const paramRegex = /%([sb])/g
  let match: RegExpExecArray | null

  while ((match = paramRegex.exec(unescaped)) !== null) {
    // Add text before this parameter
    if (match.index > currentPos) {
      templateParts.push({
        text: unescaped.slice(currentPos, match.index),
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
  if (currentPos < unescaped.length) {
    templateParts.push({
      text: unescaped.slice(currentPos),
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
 * Decompiler class
 */
export class Decompiler {
  private functionCounter = 0

  constructor(private context: DecompilerContext) {}

  /**
   * Decompile a variable to VariableDeclaration
   * Also handles @export decorator if exportName differs from name
   */
  decompileVariable(
    variable: Variable,
    value: string | number | boolean | (string | number | boolean)[]
  ): VariableDeclaration {
    const name = generateVariableName(variable, this.context)

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
   * Check if an exportName should be used (i.e., it differs from the internal name)
   */
  private shouldUseExportName(
    variable: Variable,
    generatedName: string
  ): string | null {
    // If no exportName, return null
    if (!variable.exportName) return null
    // If exportName equals the generated name, return null (redundant)
    if (variable.exportName === generatedName) return null
    // Otherwise return the exportName
    return variable.exportName
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
      for (const [variable, generatedName] of this.context.variableNames) {
        if (
          (variable.exportName && variable.exportName === varName) ||
          variable.name === varName
        ) {
          return makeIdentifier(generatedName)
        }
      }
      // Fallback to using the field name directly
      return makeIdentifier(varName)
    }

    // Handle list contents
    if (opcode === 'data_listcontents') {
      const listName = fields.LIST
      for (const [variable, generatedName] of this.context.variableNames) {
        if (
          (variable.exportName && variable.exportName === listName) ||
          variable.name === listName
        ) {
          return makeIdentifier(generatedName)
        }
      }
      return makeIdentifier(listName)
    }

    // Handle procedure arguments
    if (opcode === 'argument_reporter_string_number') {
      return makeIdentifier(fields.VALUE)
    }

    if (opcode === 'argument_reporter_boolean') {
      return makeIdentifier(fields.VALUE)
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
          left: this.decompileReporter((inputs.OPERAND1 as BooleanInput).value),
          operator: '&&',
          right: this.decompileReporter(
            (inputs.OPERAND2 as BooleanInput).value
          ),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_or': {
        const expr: BinaryExpression = {
          type: 'BinaryExpression',
          left: this.decompileReporter((inputs.OPERAND1 as BooleanInput).value),
          operator: '||',
          right: this.decompileReporter(
            (inputs.OPERAND2 as BooleanInput).value
          ),
          line: 0,
          column: 0
        }
        return expr
      }
      case 'operator_not': {
        const operand = (inputs.OPERAND as BooleanInput).value

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

      case 'data_indexoflist': {
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
          property: makeIdentifier('at'),
          computed: false,
          line: 0,
          column: 0
        }
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: memberExpr,
          arguments: [
            this.decompileReporter((inputs.LETTER as AnyInput).value)
          ],
          line: 0,
          column: 0
        }
        return callExpr
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
      return this.decompileProcedureCall(reporter)
    }

    // Try to match namespace entry
    const namespaceMatch = this.tryMatchNamespace(reporter)
    if (namespaceMatch) {
      return namespaceMatch
    }

    // Fallback: create namespace call
    throw new DecompilerError(
      `Cannot decompile reporter with opcode: ${opcode}`
    )
  }

  /**
   * Resolve a list variable by its export name or name
   */
  private resolveListVariable(name: string): Expression {
    for (const [variable, generatedName] of this.context.variableNames) {
      if (
        (variable.exportName && variable.exportName === name) ||
        variable.name === name
      ) {
        return makeIdentifier(generatedName)
      }
    }
    return makeIdentifier(name)
  }

  /**
   * Try to match a block/reporter to a namespace entry
   */
  private tryMatchNamespace(block: Block | Reporter): CallExpression | null {
    for (const [nsName, namespace] of this.context.namespaces) {
      for (const [memberName, entry] of namespace) {
        if (entry.opcode === block.opcode) {
          // Try to match fields and inputs
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
            return callExpr
          }
        }
      }
    }
    return null
  }

  /**
   * Try to match block inputs/fields to namespace entry args
   */
  private tryMatchEntry(
    block: Block | Reporter,
    entry: NamespaceEntry
  ): Expression[] | null {
    const args: Expression[] = []

    for (const arg of entry.args) {
      if (arg.type === 'field') {
        // Match field
        const fieldValue = block.fields[arg.name]
        if (fieldValue === undefined) return null
        args.push(makeLiteral(fieldValue))
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
      const condition = this.decompileReporter(
        (inputs.CONDITION as BooleanInput).value
      )
      const thenBlocks = (inputs.SUBSTACK as SubstackInput).value
      const thenBody: BlockStatement = {
        type: 'BlockStatement',
        body: this.tryConvertToForLoop(
          thenBlocks.map(b => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      }

      const stmt: IfStatement = {
        type: 'IfStatement',
        condition,
        then: thenBody,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'control_if_else') {
      const condition = this.decompileReporter(
        (inputs.CONDITION as BooleanInput).value
      )
      const thenBlocks = (inputs.SUBSTACK as SubstackInput).value
      const elseBlocks = (inputs.SUBSTACK2 as SubstackInput).value

      const thenBody: BlockStatement = {
        type: 'BlockStatement',
        body: this.tryConvertToForLoop(
          thenBlocks.map(b => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      }

      const elseBody: BlockStatement = {
        type: 'BlockStatement',
        body: this.tryConvertToForLoop(
          elseBlocks.map(b => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      }

      const stmt: IfStatement = {
        type: 'IfStatement',
        condition,
        then: thenBody,
        else: elseBody,
        line: 0,
        column: 0
      }
      return stmt
    }

    if (opcode === 'control_repeat') {
      const times = this.decompileReporter((inputs.TIMES as AnyInput).value)
      const body = (inputs.SUBSTACK as SubstackInput).value

      // Decompile as for loop: for (let i = 0; i < times; i++)
      // But we need a simpler representation - use while
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
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: callExpr,
        line: 0,
        column: 0
      }
      return stmt
    }

    // Try to match namespace
    const namespaceMatch = this.tryMatchNamespaceBlock(block)
    if (namespaceMatch) {
      const stmt: ExpressionStatement = {
        type: 'ExpressionStatement',
        expression: namespaceMatch,
        line: 0,
        column: 0
      }
      return stmt
    }

    // Fallback: create generic namespace call
    throw new DecompilerError(`Cannot decompile block with opcode: ${opcode}`)
  }

  /**
   * Try to match a command block to namespace
   */
  private tryMatchNamespaceBlock(block: Block): CallExpression | null {
    for (const [nsName, namespace] of this.context.namespaces) {
      for (const [memberName, entry] of namespace) {
        if (entry.opcode === block.opcode && entry.type === 'void') {
          const matched = this.tryMatchEntryWithSubstack(block, entry)
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
            return callExpr
          }
        }
      }
    }
    return null
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
        const fieldValue = block.fields[arg.name]
        if (fieldValue === undefined) return null
        args.push(makeLiteral(fieldValue))
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
   * Decompile a script
   */
  decompileScript(script: Script): Statement[] {
    const statements = script.blocks.map(block => this.decompileBlock(block))
    // Try to detect and convert for loop patterns
    return this.tryConvertToForLoop(statements)
  }

  /**
   * Decompile a procedure call (function call)
   */
  private decompileProcedureCall(block: Block | Reporter): CallExpression {
    const mutation = block.mutation
    if (!mutation) {
      throw new DecompilerError('procedures_call block missing mutation')
    }

    const proccode = mutation.proccode as string
    if (!proccode) {
      throw new DecompilerError('procedures_call block missing proccode')
    }

    // Parse argumentnames from mutation
    const argumentnamesStr = mutation.argumentnames as string
    const argumentnames: string[] = argumentnamesStr
      ? JSON.parse(argumentnamesStr)
      : []

    // Decompile arguments from inputs
    const args: Expression[] = []
    for (const argName of argumentnames) {
      const input = block.inputs[argName]
      if (!input) {
        throw new DecompilerError(`Missing input for argument ${argName}`)
      }

      if (input.type === 'bool') {
        args.push(this.decompileReporter((input as BooleanInput).value))
      } else if (input.type === 'any') {
        args.push(this.decompileReporter((input as AnyInput).value))
      } else {
        throw new DecompilerError(
          `Unexpected input type for argument ${argName}`
        )
      }
    }

    // Try to determine the function name
    // First check if it matches compiler format
    const parsed = parseProccode(proccode)

    if (parsed) {
      // Compiler format: functionName(param1 = %s, param2 = %b)
      const callExpr: CallExpression = {
        type: 'CallExpression',
        callee: makeIdentifier(parsed.name),
        arguments: args,
        line: 0,
        column: 0
      }
      return callExpr
    } else {
      // Export format: look up the function name from proccode mapping
      const functionName = this.context.proccodeToFunctionName.get(proccode)

      if (functionName) {
        // Found the function name in the mapping
        const callExpr: CallExpression = {
          type: 'CallExpression',
          callee: makeIdentifier(functionName),
          arguments: args,
          line: 0,
          column: 0
        }
        return callExpr
      } else {
        // Function not yet decompiled or not found - throw error
        throw new DecompilerError(
          `Cannot resolve export format procedure call: ${proccode}. ` +
            `Function must be decompiled before its calls can be resolved.`
        )
      }
    }
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
    const booleanOpcodes = [
      'operator_equals',
      'operator_lt',
      'operator_gt',
      'operator_and',
      'operator_or',
      'operator_not',
      'operator_contains',
      'data_listcontainsitem'
    ]

    return booleanOpcodes.includes(reporter.opcode)
  }

  /**
   * Decompile function from proccode and blocks
   */
  decompileFunction(
    proccode: string,
    impl: Block[],
    argumentNames: string[]
  ): FunctionDeclaration | DecoratorStatement {
    let name: string
    let params: Parameter[]
    let generatedExportName: string | undefined

    // Try to parse as compiler format first
    const parsed = parseProccode(proccode)

    if (parsed) {
      // Use original compiler format: functionName(param1 = %s, param2 = %b)
      name = parsed.name

      // Collect variable names used in function
      const usedVarNames = new Set<string>()
      // This is simplified - in reality we'd need to analyze the blocks

      const paramNames = generateParameterNames(parsed.params, usedVarNames)
      params = paramNames.map(p => ({
        name: makeIdentifier(p.name),
        type: makeIdentifier(p.type)
      }))
    } else if (argumentNames) {
      // Use export format: func arg %s argbool %b with argumentNames
      const exportParsed = parseExportProccode(proccode, argumentNames)

      if (!exportParsed) {
        throw new DecompilerError(
          `Cannot parse export format proccode: ${proccode} with ${argumentNames.length} arguments`
        )
      }

      // Generate function name: func1, func2, etc.
      this.functionCounter++
      name = `func${this.functionCounter}`

      // Collect variable names used in function
      const usedVarNames = new Set<string>()

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
    const returnType = this.inferReturnType(impl)

    const body: BlockStatement = {
      type: 'BlockStatement',
      body: this.tryConvertToForLoop(
        impl.map(block => this.decompileBlock(block))
      ),
      line: 0,
      column: 0
    }

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

    // Register proccode to function name mapping for procedure call resolution
    this.context.proccodeToFunctionName.set(proccode, name)

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
 */
export function createDecompiler(
  globalScope: Scope,
  namespaces: Map<string, Namespace>
): Decompiler {
  const context: DecompilerContext = {
    globalScope,
    namespaces,
    usedNames: new Set(),
    variableNames: new Map(),
    proccodeToFunctionName: new Map()
  }
  return new Decompiler(context)
}
