import {
  Variable,
  Reporter,
  Block,
  Script,
  BooleanInput,
  AnyInput,
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
  DecoratorStatement,
  IncrementStatement,
  ForStatement,
  ModuleDeclaration,
  ExternDeclaration,
  ImportStatement
} from '@scratch-fuse/core'

import { ErrorList } from '@scratch-fuse/utility'

export interface ResolvedResult {
  program: Program
  filename: string // Absolute path of the resolved program
}
/**
 * ImportResolver is responsible for resolving import paths to Programs
 * This allows the compiler to handle import statements by loading and parsing external modules
 */
export interface ImportResolver {
  /**
   * Resolve an import path to a Program
   * @param path The import path (from ImportStatement.path.value)
   * @param currentFile Optional context about the file doing the importing
   * @returns The resolved Program, or throws an error if the import cannot be resolved
   */
  resolve(
    path: string,
    currentFile?: string
  ): Promise<ResolvedResult> | ResolvedResult
}

export interface ContextOptions {
  importResolver?: ImportResolver
}

export interface ProcedureArgument {
  name: string
  type: 'any' | 'bool'
}

export type ListCommand = (v: Variable, args: (Reporter | string)[]) => Block[]
export type ListReporter = (
  v: Variable,
  args: (Reporter | string)[]
) => TypedValue
export type VarCommand = (v: Variable, args: (Reporter | string)[]) => Block[]
// Var methods have no side effects, so VarCommand is not needed
export type VarReporter = (
  v: Reporter | string,
  args: (Reporter | string)[]
) => TypedValue

export class CompilerError extends Error {
  constructor(
    message: string,
    public line: number,
    public column: number
  ) {
    super(`${message} at ${line}:${column}`)
    this.name = 'CompilerError'
  }
}

export interface CompiledResult {
  scripts: Script[]
  functions: CompiledFunction[]
  variables: [
    Variable,
    string | boolean | number | (string | boolean | number)[]
  ][]
  externs: External[]
  errors?: ErrorList
}

// Method call maps
const listCmds = new Map<string, ListCommand>([
  [
    'push',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1) throw new Error('push expects exactly one argument')
      return [
        {
          opcode: 'data_addtolist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            ITEM: { type: 'any', value: rhs[0] }
          }
        }
      ]
    }
  ],
  [
    'pop',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('pop expects no arguments')
      return [
        {
          opcode: 'data_deleteoflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            INDEX: { type: 'any', value: 'last' }
          }
        }
      ]
    }
  ],
  [
    'insert',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 2)
        throw new Error('insert expects exactly two arguments')
      return [
        {
          opcode: 'data_insertatlist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            INDEX: { type: 'any', value: rhs[0] },
            ITEM: { type: 'any', value: rhs[1] }
          }
        }
      ]
    }
  ],
  [
    'remove',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1)
        throw new Error('remove expects exactly one argument')
      return [
        {
          opcode: 'data_deleteoflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            INDEX: { type: 'any', value: rhs[0] }
          }
        }
      ]
    }
  ],
  [
    'replace',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 2)
        throw new Error('replace expects exactly two arguments')
      return [
        {
          opcode: 'data_replaceitemoflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            INDEX: { type: 'any', value: rhs[0] },
            ITEM: { type: 'any', value: rhs[1] }
          }
        }
      ]
    }
  ],
  [
    'show',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('show expects no arguments')
      return [
        {
          opcode: 'data_showlist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {}
        }
      ]
    }
  ],
  [
    'hide',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('hide expects no arguments')
      return [
        {
          opcode: 'data_hidelist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {}
        }
      ]
    }
  ],
  [
    'clear',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('clear expects no arguments')
      return [
        {
          opcode: 'data_deletealloflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {}
        }
      ]
    }
  ]
])

const listReps = new Map<string, ListReporter>([
  [
    'includes',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1)
        throw new Error('includes expects exactly one argument')
      return {
        type: 'bool',
        value: {
          opcode: 'data_listcontainsitem',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            ITEM: { type: 'any', value: rhs[0] }
          }
        }
      }
    }
  ],
  [
    'at',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1) throw new Error('at expects exactly one argument')
      return {
        type: 'any',
        value: {
          opcode: 'data_itemoflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            INDEX: { type: 'any', value: rhs[0] }
          }
        }
      }
    }
  ],
  [
    'indexOf',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1)
        throw new Error('indexOf expects exactly one argument')
      return {
        type: 'any',
        value: {
          opcode: 'data_itemnumoflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {
            ITEM: { type: 'any', value: rhs[0] }
          }
        }
      }
    }
  ],
  [
    'length',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('length expects no arguments')
      return {
        type: 'any',
        value: {
          opcode: 'data_lengthoflist',
          fields: {
            LIST: v.exportName ?? v.name
          },
          inputs: {}
        }
      }
    }
  ]
])

const varCmds = new Map<string, VarCommand>([
  [
    'show',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('show expects no arguments')
      return [
        {
          opcode: 'data_showvariable',
          fields: {
            VARIABLE: v.exportName ?? v.name
          },
          inputs: {}
        }
      ]
    }
  ],
  [
    'hide',
    (v: Variable, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('hide expects no arguments')
      return [
        {
          opcode: 'data_hidevariable',
          fields: {
            VARIABLE: v.exportName ?? v.name
          },
          inputs: {}
        }
      ]
    }
  ]
])

const varReps = new Map<string, VarReporter>([
  [
    'at',
    (v: Reporter | string, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1) throw new Error('at expects exactly one argument')
      return {
        type: 'any',
        value: {
          opcode: 'operator_letter_of',
          fields: {},
          inputs: {
            STRING: { type: 'any', value: v },
            LETTER: { type: 'any', value: rhs[0] }
          }
        }
      }
    }
  ],
  [
    'includes',
    (v: Reporter | string, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 1)
        throw new Error('includes expects exactly one argument')
      return {
        type: 'bool',
        value: {
          opcode: 'operator_contains',
          fields: {},
          inputs: {
            STRING1: { type: 'any', value: v },
            STRING2: { type: 'any', value: rhs[0] }
          }
        }
      }
    }
  ],
  [
    'length',
    (v: Reporter | string, rhs: (Reporter | string)[]) => {
      if (rhs.length !== 0) throw new Error('length expects no arguments')
      return {
        type: 'any',
        value: {
          opcode: 'operator_length',
          fields: {},
          inputs: {
            STRING: { type: 'any', value: v }
          }
        }
      }
    }
  ]
])

/**
 * Call a method on a Variable or Reporter value
 * @param target Variable object or Reporter value
 * @param method Method name to call
 * @param args Arguments to pass to the method
 * @returns Block array for commands, or TypedValue for reporters
 */
export function callMethod(
  target: Variable | Reporter | string,
  method: string,
  args: (Reporter | string)[]
): Block[] | TypedValue {
  // If target is a Variable, determine which map to use
  if (
    typeof target === 'object' &&
    'type' in target &&
    typeof target.type === 'string' &&
    (target.type === 'list' || target.type === 'scalar')
  ) {
    const variable = target as Variable
    if (variable.type === 'list') {
      // Try list command first
      const listCmd = listCmds.get(method)
      if (listCmd) return listCmd(variable, args)

      // Try list reporter
      const listRep = listReps.get(method)
      if (listRep) return listRep(variable, args)

      throw new Error(`Method ${method} not found on list variable`)
    } else {
      // Scalar variable
      // Try var command first
      const varCmd = varCmds.get(method)
      if (varCmd) return varCmd(variable, args)

      // For reporters on scalar variables, we need the variable value as Reporter
      // This should not happen in practice as scalar variables are accessed differently
      throw new Error(`Method ${method} not found on scalar variable`)
    }
  } else {
    // Target is a Reporter or string value
    const varRep = varReps.get(method)
    if (varRep) return varRep(target as Reporter | string, args)

    throw new Error(`Method ${method} not found for value`)
  }
}

export class Scope {
  constructor(private args?: Map<string, ProcedureArgument>) {}

  getArg(name: string): Reporter | null {
    const arg = this.args?.get(name)
    if (!arg) return null

    switch (arg.type) {
      case 'any':
        return {
          opcode: 'argument_reporter_string_number',
          fields: {
            VALUE: arg.name
          },
          inputs: {}
        }
      case 'bool':
        return {
          opcode: 'argument_reporter_boolean',
          fields: {
            VALUE: arg.name
          },
          inputs: {}
        }
    }
  }

  getArgType(name: string): 'arg_any' | 'arg_bool' | null {
    const arg = this.args?.get(name)
    if (!arg) return null
    return arg.type === 'bool' ? 'arg_bool' : 'arg_any'
  }
}

export interface CompiledFunction {
  decl: FunctionDeclaration
  proccode: string
  impl: Block[]
}

export class ScratchFunction {
  public scope: Scope
  public module: Module
  constructor(
    public decl: FunctionDeclaration,
    private exportName: string | null,
    module: Module
  ) {
    const args = new Map<string, ProcedureArgument>()
    for (const arg of decl.parameters) {
      if (['bool', 'any'].indexOf(arg.type.name) === -1) {
        throw new CompilerError(
          `Invalid argument type ${arg.type}, at function ${decl.name.name}`,
          decl.name.line,
          decl.name.column
        )
      }
      args.set(arg.name.name, {
        type: arg.type.name === 'bool' ? 'bool' : 'any',
        name: arg.name.name
      })
    }
    // Create function scope with function arguments
    this.scope = new Scope(args)
    this.module = module
    if (this.exportName) {
      // Validate exportName
      ScratchFunction.getProccode(decl, this.exportName)
    }
  }
  static escape(str: string) {
    // replace % with %%
    return str.replace(/%/g, '%%')
  }
  // TODO: 移动到其它位置，解耦合
  static getProccode(
    decl: FunctionDeclaration,
    exportName: string | null,
    modulePath: string[] = []
  ) {
    if (exportName) {
      return ScratchFunction.parseTemplateName(exportName, decl)
    }

    // Generate proccode with module path prefix if in a module
    const fullName =
      modulePath.length > 0
        ? [...modulePath, decl.name.name].join('.')
        : decl.name.name

    return `${fullName}(${decl.parameters
      .map(
        p =>
          `${ScratchFunction.escape(p.name.name)} = %${
            p.type.name === 'bool' ? 'b' : 's'
          }`
      )
      .join(', ')})`
  }
  get proccode() {
    const modulePath: string[] = []
    let module: Module = this.module
    while (module) {
      modulePath.unshift(module.name)
      if (module.parent) {
        module = followAlias(module.parent)
      } else {
        break
      }
    }
    modulePath.splice(0, 1) // Remove root module name
    return ScratchFunction.getProccode(this.decl, this.exportName, modulePath)
  }
  static parseTemplateName(
    template: string,
    decl: FunctionDeclaration
  ): string {
    // Use regex to match parameter placeholders and escaped brackets
    // Match: [paramName] but not [[...]] or ...]]
    // Use negative lookbehind (?<!\[) and negative lookahead (?!\])
    const paramRegex = /(?<!\[)\[([^\]]+)\](?!\])/g

    let result = ''
    let lastIndex = 0
    let paramIndex = 0
    let match: RegExpExecArray | null

    while ((match = paramRegex.exec(template)) !== null) {
      // Add text before this parameter (escape it)
      if (match.index > lastIndex) {
        const textBefore = template.slice(lastIndex, match.index)
        // Replace escaped brackets [[ -> [ and ]] -> ]
        const unescapedText = textBefore
          .replace(/\[\[/g, '[')
          .replace(/\]\]/g, ']')
        // Escape % to %%
        result += ScratchFunction.escape(unescapedText)
      }

      // Process parameter placeholder
      const paramName = match[1]

      if (paramIndex >= decl.parameters.length) {
        throw new CompilerError(
          `Too many parameter placeholders in template`,
          decl.name.line,
          decl.name.column
        )
      }

      const param = decl.parameters[paramIndex]
      if (paramName !== param.name.name) {
        throw new CompilerError(
          `Parameter placeholder [${paramName}] does not match parameter ${param.name.name}`,
          decl.name.line,
          decl.name.column
        )
      }

      result += `%${param.type.name === 'bool' ? 'b' : 's'}`
      paramIndex++
      lastIndex = match.index + match[0].length
    }

    // Add remaining text after last parameter
    if (lastIndex < template.length) {
      const textAfter = template.slice(lastIndex)
      // Replace escaped brackets [[ -> [ and ]] -> ]
      const unescapedText = textAfter
        .replace(/\[\[/g, '[')
        .replace(/\]\]/g, ']')
      // Escape % to %%
      result += ScratchFunction.escape(unescapedText)
    }

    if (paramIndex < decl.parameters.length) {
      throw new CompilerError(
        `Missing parameter placeholders for: ${decl.parameters
          .slice(paramIndex)
          .map(p => p.name.name)
          .join(', ')}`,
        decl.name.line,
        decl.name.column
      )
    }

    return result.trim()
  }
}

export type TypedValue =
  | {
      type: 'any'
      value: Reporter | string
    }
  | { type: 'bool'; value: Reporter }

export type ResolvedValue =
  | ScratchFunction
  | External
  | (TypedValue & {
      computed?: {
        target: Variable // only list
        index: TypedValue
      }
    })
  | Variable
  | {
      target: Variable | Reporter | string
      method: (args: (Reporter | string)[]) => Block[] | TypedValue
    }
  | Block[]
  | Module

export interface External {
  opcode: string
  type: 'void' | 'any' | 'bool' | 'hat'
  inputs?: Record<string, BooleanInput | AnyInput | SubstackInput> // preset inputs
  fields?: Record<string, string> // preset fields
  args: (
    | ExternalArgumentAny
    | ExternalArgumentBoolean
    | ExternalArgumentSubstack
    | ExternalArgumentField
  )[]
}
export interface ExternalArgumentBase {
  name: string
}

export interface ExternalArgumentAny extends ExternalArgumentBase {
  type: 'any'
}

export interface ExternalArgumentBoolean extends ExternalArgumentBase {
  type: 'bool'
}
export interface ExternalArgumentSubstack extends ExternalArgumentBase {
  type: 'substack'
}
export interface ExternalArgumentField extends ExternalArgumentBase {
  type: 'field'
  menu: Record<string, string> | null // null means as-is
}

export class Context {
  public currentModule: Module
  private importedModules: Map<string, Module>
  private currentScope: Scope | null = null // Current function scope (for parameters)

  constructor(
    currentModule: Module,
    public options?: ContextOptions,
    importedModules?: Map<string, Module>
  ) {
    this.currentModule = currentModule
    this.importedModules = importedModules ?? new Map()
  }

  /**
   * Get variable type by name from current module or current scope
   */
  private getVariableType(
    name: string
  ): 'list' | 'scalar' | 'arg_any' | 'arg_bool' | null {
    // Check current scope first (for function parameters)
    if (this.currentScope) {
      const argType = this.currentScope.getArgType(name)
      if (argType) return argType
    }

    let module: ModuleInfo | null = this.currentModule

    while (module) {
      const resolved = followAlias(module)
      const [varInfo] = resolved.variables.get(name) ?? []
      if (varInfo) {
        return varInfo.type
      }
      module = resolved.parent
    }

    return null
  }

  /**
   * Get variable Reporter by name from current module or current scope
   */
  private getVariableReporter(name: string): Reporter | null {
    // Check current scope first (for function parameters)
    if (this.currentScope) {
      const argReporter = this.currentScope.getArg(name)
      if (argReporter) return argReporter
    }

    // Then check current module
    const currentResolved = followAlias(this.currentModule)
    const [variable] = currentResolved.variables.get(name) ?? []
    if (!variable) return null

    if (variable.type === 'scalar') {
      return {
        opcode: 'data_variable',
        fields: {
          VARIABLE: variable.exportName ?? variable.name
        },
        inputs: {}
      }
    } else {
      return {
        opcode: 'data_listcontents',
        fields: {
          LIST: variable.exportName ?? variable.name
        },
        inputs: {}
      }
    }
  }

  /**
   * Get Variable object by name from current module
   */
  private getVariable(name: string): Variable | null {
    let module: ModuleInfo | null = this.currentModule
    while (module) {
      const resolved = followAlias(module)
      const [variable] = resolved.variables.get(name) ?? []
      if (variable) return variable
      module = resolved.parent
    }
    return null
  }

  compile(stmt: Program): Promise<CompiledResult> | CompiledResult
  compile(
    stmt: Statement,
    functionReturnType?: 'bool' | 'any' | 'void'
  ): Block[]
  compile(
    stmt: Statement | Program,
    functionReturnType?: 'bool' | 'any' | 'void'
  ): Block[] | Promise<CompiledResult> | CompiledResult {
    if (stmt.type === 'Program') {
      return this.parseProgram(stmt as Program)
    } else {
      return this.parseStatement(stmt as Statement, functionReturnType ?? null)
    }
  }

  /**
   * Process import statements in a program
   * Compiles each import in isolation and returns their modules and results
   */
  private async processImports(
    program: Program,
    importChain: Set<string> = new Set()
  ): Promise<{ modules: Module[]; results: CompiledResult[] }> {
    const importModules: Module[] = []
    const importResults: CompiledResult[] = []

    for (const stmt of program.body) {
      if (stmt.type === 'ImportStatement') {
        const importStmt = stmt as ImportStatement
        const importPath = importStmt.path.value as string

        // Check for circular imports
        if (importChain.has(importPath)) {
          throw new CompilerError(
            `Circular import detected: ${importPath}`,
            importStmt.line,
            importStmt.column
          )
        }

        try {
          // Resolve the import
          const importedProgram = await this.options!.importResolver!.resolve(
            importPath,
            this.currentModule.filename
          )

          if (this.importedModules.has(importedProgram.filename)) {
            importModules.push(
              this.importedModules.get(importedProgram.filename)!
            )
            continue
          }

          // Create a new import chain for this path
          const newChain = new Set(importChain)
          newChain.add(importedProgram.filename)

          function getTopModule(module: Module): Module {
            let current: Module = module
            while (current.parent) {
              current = current.parent
            }
            return current
          }

          // Create a new module for the imported file
          const importedModule: Module = {
            name: '',
            filename: importedProgram.filename,
            parent: getTopModule(this.currentModule),
            functions: new Map(),
            variables: new Map(),
            externs: new Map(),
            children: new Map()
          }

          this.importedModules.set(importedProgram.filename, importedModule)

          // Create a new context for the imported module
          const importContext = new Context(
            importedModule,
            this.options,
            this.importedModules
          )

          // Compile the imported module
          const importedResult = await importContext.compile(
            importedProgram.program
          )

          // // Recursively process any imports in the imported module
          // const nestedImports = await importContext.processImports(
          //   importedProgram.program,
          //   newChain
          // )

          // Collect this import's module and result
          importModules.push(importedModule)
          importResults.push(importedResult)

          // Collect nested imports
          // importModules.push(...nestedImports.modules)
          // importResults.push(...nestedImports.results)
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error
          }
          throw new CompilerError(
            `Failed to resolve import '${importPath}': ${(error as Error).message}`,
            importStmt.line,
            importStmt.column
          )
        }
      }
    }

    return { modules: importModules, results: importResults }
  }

  /**
   * Merge compiled results from imports into the main result
   * Checks for variable name conflicts
   */
  private mergeImportResults(
    mainResult: CompiledResult,
    importResults: CompiledResult[]
  ): CompiledResult {
    const mergedScripts = [...mainResult.scripts]
    const mergedFunctions = [...mainResult.functions]
    const mergedVariables = [...mainResult.variables]
    const mergedExterns = [...mainResult.externs]

    // Track variable sources to detect conflicts
    const variableModules = new Map<string, string>()

    // Record main module variables
    for (const [variable, _] of mainResult.variables) {
      const varName = variable.exportName ?? variable.name
      variableModules.set(varName, this.currentModule.filename || 'main')
    }

    // Merge import results
    for (const importResult of importResults) {
      // Merge scripts
      mergedScripts.push(...importResult.scripts)

      // Merge functions
      mergedFunctions.push(...importResult.functions)

      // Merge variables with conflict detection
      for (const [variable, value] of importResult.variables) {
        const varName = variable.exportName ?? variable.name
        const existingModule = variableModules.get(varName)

        if (existingModule) {
          // Variable with same name already exists
          // This is an error unless both are global variables from the same module
          const currentModule = variable.name.split('.')[0] || 'main'

          if (existingModule !== currentModule || !variable.isGlobal) {
            throw new CompilerError(
              `Variable name conflict: '${varName}' is defined in both '${existingModule}' and '${currentModule}'`,
              0,
              0
            )
          }
          // Skip duplicate global variable from same module
          continue
        }

        variableModules.set(varName, variable.name.split('.')[0] || 'main')
        mergedVariables.push([variable, value])
      }

      // Merge externs
      mergedExterns.push(...importResult.externs)
    }

    return {
      scripts: mergedScripts,
      functions: mergedFunctions,
      variables: mergedVariables,
      externs: mergedExterns
    }
  }

  /**
   * Call a ScratchFunction with arguments and return the result as a reporter
   */
  private callFunction(
    func: ScratchFunction,
    args: TypedValue[],
    line: number,
    column: number
  ): TypedValue {
    if (func.decl.returnType.name === 'void') {
      throw new CompilerError(
        `Function ${func.decl.name.name} returns void and cannot be used as a reporter`,
        line,
        column
      )
    }

    // Check argument count
    if (args.length !== func.decl.parameters.length) {
      throw new CompilerError(
        `Function ${func.decl.name.name} expects ${func.decl.parameters.length} arguments, got ${args.length}`,
        line,
        column
      )
    }

    // Check argument types
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      if (paramType === 'bool' && args[i].type !== 'bool') {
        throw new CompilerError(
          `Argument ${i + 1} of function ${func.decl.name.name} must be boolean`,
          line,
          column
        )
      }
    }

    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      {}
    for (let i = 0; i < args.length; i++) {
      const paramName = func.decl.parameters[i].name.name
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      inputs[paramName] =
        paramType === 'bool'
          ? { type: 'bool', value: args[i].value as Reporter }
          : { type: 'any', value: args[i].value }
    }

    const names = func.decl.parameters.map(p => p.name.name)

    return {
      type: func.decl.returnType.name === 'bool' ? 'bool' : 'any',
      value: {
        opcode: 'procedures_call',
        fields: {},
        inputs,
        mutation: {
          tagName: 'mutation',
          proccode: func.proccode,
          children: [],
          return: '1',
          warp: func.decl.once ? 'true' : 'false',
          argumentids: JSON.stringify(names),
          argumentnames: JSON.stringify(names),
          argumentdefaults: JSON.stringify(
            func.decl.parameters.map(p =>
              p.type.name === 'any' ? '' : 'false'
            )
          )
        }
      }
    }
  }

  /**
   * Call an External with arguments and return the result as a reporter
   */
  private callExternal(
    external: External,
    args: TypedValue[],
    line: number,
    column: number
  ): TypedValue {
    if (external.type !== 'any' && external.type !== 'bool') {
      throw new CompilerError(
        `External ${external.opcode} cannot be used as a reporter (type: ${external.type})`,
        line,
        column
      )
    }

    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      Object.assign({}, external.inputs)
    const fields: { [key: string]: string } = Object.assign({}, external.fields)

    // Match arguments with external arguments (non-substack)
    const nonSubstackArgs = external.args.filter(arg => arg.type !== 'substack')
    if (args.length !== nonSubstackArgs.length) {
      throw new CompilerError(
        `External ${external.opcode} expects ${nonSubstackArgs.length} arguments, got ${args.length}`,
        line,
        column
      )
    }

    for (let i = 0; i < nonSubstackArgs.length; i++) {
      const argDef = nonSubstackArgs[i]
      const argValue = args[i]

      if (argDef.type === 'bool' && argValue.type !== 'bool') {
        throw new CompilerError(
          `Argument ${argDef.name} must be boolean`,
          line,
          column
        )
      }

      if (argDef.type === 'field') {
        // Must be literal
        const rawArgValue = args[i]
        if (
          rawArgValue.type !== 'any' ||
          typeof rawArgValue.value !== 'string'
        ) {
          throw new CompilerError(
            `Argument ${argDef.name} must be a string literal for field`,
            line,
            column
          )
        }
        const fieldValue = String(rawArgValue.value)
        if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
          throw new CompilerError(
            `Argument ${argDef.name} has invalid value ${fieldValue}`,
            line,
            column
          )
        }
        fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue
      } else {
        inputs[argDef.name] =
          argValue.type === 'bool'
            ? { type: 'bool', value: argValue.value as Reporter }
            : { type: 'any', value: argValue.value }
      }
    }

    return {
      type: external.type,
      value: {
        opcode: external.opcode,
        fields,
        inputs
      }
    }
  }

  /**
   * Internal implementation for expression parsing
   * Handles recursive lookup with optional current object for member expressions
   */
  private parseExprImpl(
    expr: Expression,
    currentObject?: ResolvedValue
  ): ResolvedValue {
    if (expr.type === 'Literal') {
      return this.parseLiteralExpression(expr as LiteralExpression)
    }

    if (expr.type === 'BinaryExpression') {
      return this.parseBinaryExpression(expr as BinaryExpression)
    }

    if (expr.type === 'UnaryExpression') {
      return this.parseUnaryExpression(expr as UnaryExpression)
    }

    if (expr.type === 'ArrayExpression') {
      throw new CompilerError(
        'ArrayExpression is only allowed in variable declarations',
        expr.line,
        expr.column
      )
    }

    if (expr.type === 'Identifier') {
      const name = (expr as IdentifierExpression).name

      // If we have a currentObject, we're accessing a property/method
      if (currentObject !== undefined) {
        // Check if currentObject is a Variable
        if (
          'name' in currentObject &&
          'type' in currentObject &&
          'isGlobal' in currentObject &&
          'exportName' in currentObject
        ) {
          const variable = currentObject as Variable

          // Check if it's a list method or variable method
          if (variable.type === 'list') {
            const listCmd = listCmds.get(name)
            const listRep = listReps.get(name)
            if (listCmd) {
              return {
                target: variable,
                method: listCmd.bind(null, variable)
              }
            }
            if (listRep) {
              return {
                target: variable,
                method: listRep.bind(null, variable)
              }
            }
          } else {
            // scalar variable
            const varCmd = varCmds.get(name)
            const varRep = varReps.get(name)
            if (varCmd) {
              return {
                target: variable,
                method: varCmd.bind(null, variable)
              }
            }
            if (varRep) {
              return {
                target: variable,
                method: varRep.bind(null, {
                  opcode: 'data_variable',
                  fields: {
                    VARIABLE: variable.exportName ?? variable.name
                  },
                  inputs: {}
                })
              }
            }
          }

          throw new CompilerError(
            `Method ${name} not found on variable ${variable.name}`,
            expr.line,
            expr.column
          )
        }

        // Check if currentObject is a ModuleInfo
        if (
          'name' in currentObject &&
          'parent' in currentObject &&
          'functions' in currentObject &&
          'variables' in currentObject &&
          'externs' in currentObject &&
          'children' in currentObject
        ) {
          let module = currentObject as ModuleInfo

          // Follow alias if this is an aliased module
          module = followAlias(module)

          // Check for function in module
          const func = module.functions.get(name)
          if (func) return func

          // Check for variable in module
          const varInfo = module.variables.get(name)
          if (varInfo) {
            const [variable] = varInfo
            return variable
          }

          // Check for extern in module
          const extern = module.externs.get(name)
          if (extern) return extern

          // Check for nested module
          const childModule = module.children.get(name)
          if (childModule) return followAlias(childModule)

          throw new CompilerError(
            `Member ${name} not found in module ${module.name}`,
            expr.line,
            expr.column
          )
        }

        if ('type' in currentObject && 'value' in currentObject) {
          // currentObject is a TypedValue - treat as string and use varReps
          const varRep = varReps.get(name)
          if (varRep) {
            return {
              target: currentObject.value,
              method: varRep.bind(null, currentObject.value)
            }
          }
          throw new CompilerError(
            `Method ${name} not found for this value`,
            expr.line,
            expr.column
          )
        }

        // Other cases for currentObject
        throw new CompilerError(
          `Cannot access property ${name} on this value`,
          expr.line,
          expr.column
        )
      }

      // No currentObject: resolve identifier from scope
      // Search from current module up to parent modules
      let searchModule: ModuleInfo | null = this.currentModule
      while (searchModule) {
        const resolved = followAlias(searchModule)

        // Try function in current module
        const func = resolved.functions.get(name)
        if (func) return func

        // Try variable in current module
        const varInfo = resolved.variables.get(name)
        if (varInfo) {
          const [variable] = varInfo
          const varType = this.getVariableType(variable.name)

          // For list/scalar variables, return Variable
          if (varType === 'list' || varType === 'scalar') {
            return variable
          }

          // For function arguments, return TypedValue
          const reporter = this.getVariableReporter(variable.name)
          if (reporter) {
            if (varType === 'arg_bool') {
              return { type: 'bool', value: reporter }
            } else if (varType === 'arg_any') {
              return { type: 'any', value: reporter }
            }
          }
        }

        // Try extern in current module
        const extern = resolved.externs.get(name)
        if (extern) return extern

        // Try child module
        const childModule = resolved.children.get(name)
        if (childModule) return followAlias(childModule)

        // Move to parent module
        searchModule = resolved.parent
      }

      // Variable/argument lookup using current module
      const varType = this.getVariableType(name)
      if (varType) {
        if (varType === 'list' || varType === 'scalar') {
          // Return Variable for list/scalar
          const variable = this.getVariable(name)
          if (variable) return variable
        } else {
          // For function arguments, return TypedValue
          const reporter = this.getVariableReporter(name)
          if (reporter) {
            if (varType === 'arg_bool') {
              return { type: 'bool', value: reporter }
            } else if (varType === 'arg_any') {
              return { type: 'any', value: reporter }
            }
          }
        }
      }

      throw new CompilerError(
        `Identifier ${name} not found`,
        expr.line,
        expr.column
      )
    }

    if (expr.type === 'MemberExpression') {
      const memberExpr = expr as MemberExpression

      // First resolve the object part using recursion
      const objectValue = this.parseExprImpl(memberExpr.object, currentObject)

      if (memberExpr.computed) {
        // Computed access like obj[index]
        // objectValue should be a Variable or TypedValue
        if (
          'name' in objectValue &&
          'type' in objectValue &&
          'isGlobal' in objectValue
        ) {
          // It's a Variable
          const variable = objectValue as Variable
          const index = this.parseExpr(memberExpr.property)

          if (variable.type === 'list') {
            // For lists, use data_itemoflist directly
            const listAtMethod = listReps.get('at')
            if (!listAtMethod) {
              throw new CompilerError(
                `Internal error: list 'at' method not found`,
                memberExpr.line,
                memberExpr.column
              )
            }
            const result = listAtMethod(variable, [index.value])
            return {
              type: 'any',
              value: result.value,
              computed: { target: variable, index }
            }
          } else {
            // scalar variable - use operator_letter_of
            const varValue = this.getVariableReporter(variable.name)
            if (!varValue) {
              throw new CompilerError(
                `Variable ${variable.name} not found`,
                memberExpr.line,
                memberExpr.column
              )
            }
            const result = this.callMethodOnValue(
              varValue,
              'at',
              [index.value],
              memberExpr.line,
              memberExpr.column
            )
            return {
              type: 'any',
              value: result.value
            }
          }
        } else if ('value' in objectValue && 'type' in objectValue) {
          // It's a TypedValue - treat as string and use operator_letter_of
          const index = this.parseExpr(memberExpr.property)
          const result = this.callMethodOnValue(
            objectValue.value,
            'at',
            [index.value],
            memberExpr.line,
            memberExpr.column
          )
          return {
            type: 'any',
            value: result.value
          }
        } else {
          throw new CompilerError(
            `Cannot use computed access on this value`,
            memberExpr.line,
            memberExpr.column
          )
        }
      } else {
        // Dot notation like obj.prop
        if (memberExpr.property.type !== 'Identifier') {
          throw new CompilerError(
            'Property must be an identifier in dot notation',
            memberExpr.line,
            memberExpr.column
          )
        }

        // Recursively parse the property with objectValue as currentObject
        return this.parseExprImpl(memberExpr.property, objectValue)
      }
    }

    if (expr.type === 'CallExpression') {
      const callExpr = expr as CallExpression

      if (callExpr.then) {
        throw new CompilerError(
          'Call expressions with then blocks cannot be used as values',
          callExpr.line,
          callExpr.column
        )
      }

      // Resolve the callee
      const callee = this.parseExprImpl(callExpr.callee)

      // Parse arguments
      const parsedArgs = callExpr.arguments.map(arg => this.parseExpr(arg))

      // Handle different callee types
      if (callee instanceof ScratchFunction) {
        // Function call
        return this.callFunction(
          callee,
          parsedArgs,
          callExpr.line,
          callExpr.column
        )
      } else if ('opcode' in callee && 'type' in callee && 'args' in callee) {
        // External call
        return this.callExternal(
          callee,
          parsedArgs,
          callExpr.line,
          callExpr.column
        )
      } else if ('target' in callee && 'method' in callee) {
        // Method call on a variable
        const methodObj = callee
        const args = parsedArgs.map(arg => arg.value)
        return methodObj.method(args)
      } else {
        throw new CompilerError(
          'Cannot call this value as a function',
          callExpr.line,
          callExpr.column
        )
      }
    }

    throw new CompilerError(
      `Unsupported expression type: ${expr.type}`,
      expr.line,
      expr.column
    )
  }

  parseExpr(stmt: Expression): TypedValue {
    const resolved = this.parseExprImpl(stmt)

    // parseExpr returns TypedValue (including with computed property)
    // Everything else should be an error

    // Check for Block[] - should not happen in expression context
    if (Array.isArray(resolved)) {
      throw new CompilerError(
        `Statement cannot be used as an expression`,
        stmt.line,
        stmt.column
      )
    }

    // Check for TypedValue (including with computed property)
    if (
      'type' in resolved &&
      'value' in resolved &&
      !('opcode' in resolved) &&
      !('name' in resolved)
    ) {
      return resolved as
        | TypedValue
        | (TypedValue & { computed: { target: Variable; index: TypedValue } })
    }

    // Check for Variable - convert scalar to TypedValue, error for list
    if (
      'name' in resolved &&
      'exportName' in resolved &&
      'isGlobal' in resolved
    ) {
      const variable = resolved as Variable

      // For scalar variables, construct reporter directly from Variable object
      if (variable.type === 'scalar') {
        const reporter: Reporter = {
          opcode: 'data_variable',
          fields: {
            VARIABLE: variable.exportName ?? variable.name
          },
          inputs: {}
        }
        return { type: 'any', value: reporter }
      }

      return {
        type: 'any',
        value: {
          opcode: 'data_listcontents',
          fields: {
            LIST: variable.exportName ?? variable.name
          },
          inputs: {}
        }
      }
    }

    // Check for method object - auto-call with no arguments (省略括号的方法调用)
    if ('target' in resolved && 'method' in resolved) {
      const methodObj = resolved as {
        target: Variable | Reporter | string
        method: (args: (Reporter | string)[]) => Block[] | TypedValue
      }

      // Try to call the method with no arguments
      const result: Block[] | TypedValue = methodObj.method([])

      // Method should return TypedValue in expression context
      if (Array.isArray(result)) {
        throw new CompilerError(
          `This method performs an action and cannot be used as a value`,
          stmt.line,
          stmt.column
        )
      }

      return result
    }

    // Check for ScratchFunction - should error
    if (resolved instanceof ScratchFunction) {
      throw new CompilerError(
        `Function ${resolved.decl.name.name} must be called with ()`,
        stmt.line,
        stmt.column
      )
    }

    // Check for External - should error
    if ('opcode' in resolved && 'type' in resolved && 'args' in resolved) {
      throw new CompilerError(
        `External ${(resolved as External).opcode} must be called with ()`,
        stmt.line,
        stmt.column
      )
    }

    // Check for ModuleInfo - should error
    if (
      'path' in resolved &&
      'functions' in resolved &&
      'children' in resolved
    ) {
      const module = resolved as ModuleInfo
      throw new CompilerError(
        `Module ${module.name} cannot be used as a value`,
        stmt.line,
        stmt.column
      )
    }

    // Unknown type
    throw new CompilerError(
      `This expression cannot be used as a value`,
      stmt.line,
      stmt.column
    )
  }

  // Helper methods for parseExpr
  private parseLiteralExpression(expr: LiteralExpression): TypedValue {
    const value = expr.value
    if (typeof value === 'boolean') {
      return this.getBooleanLiteral(value)
    } else {
      return {
        type: 'any',
        value: String(value)
      }
    }
  }

  private parseBinaryExpression(expr: BinaryExpression): TypedValue {
    const left = this.parseExpr(expr.left)
    const right = this.parseExpr(expr.right)

    switch (expr.operator) {
      case '+':
        return {
          type: 'any',
          value: {
            opcode: 'operator_add',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '-':
        return {
          type: 'any',
          value: {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '*':
        return {
          type: 'any',
          value: {
            opcode: 'operator_multiply',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '/':
        return {
          type: 'any',
          value: {
            opcode: 'operator_divide',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '%':
        return {
          type: 'any',
          value: {
            opcode: 'operator_mod',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '..':
        return {
          type: 'any',
          value: {
            opcode: 'operator_join',
            fields: {},
            inputs: {
              STRING1: { type: 'any', value: left.value },
              STRING2: { type: 'any', value: right.value }
            }
          }
        }
      case '==':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_equals',
            fields: {},
            inputs: {
              OPERAND1: { type: 'any', value: left.value },
              OPERAND2: { type: 'any', value: right.value }
            }
          }
        }
      case '!=':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                type: 'bool',
                value: {
                  opcode: 'operator_equals',
                  fields: {},
                  inputs: {
                    OPERAND1: { type: 'any', value: left.value },
                    OPERAND2: { type: 'any', value: right.value }
                  }
                }
              }
            }
          }
        }
      case '<':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_lt',
            fields: {},
            inputs: {
              OPERAND1: { type: 'any', value: left.value },
              OPERAND2: { type: 'any', value: right.value }
            }
          }
        }
      case '>':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_gt',
            fields: {},
            inputs: {
              OPERAND1: { type: 'any', value: left.value },
              OPERAND2: { type: 'any', value: right.value }
            }
          }
        }
      case '<=':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                type: 'bool',
                value: {
                  opcode: 'operator_gt',
                  fields: {},
                  inputs: {
                    OPERAND1: { type: 'any', value: left.value },
                    OPERAND2: { type: 'any', value: right.value }
                  }
                }
              }
            }
          }
        }
      case '>=':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                type: 'bool',
                value: {
                  opcode: 'operator_lt',
                  fields: {},
                  inputs: {
                    OPERAND1: { type: 'any', value: left.value },
                    OPERAND2: { type: 'any', value: right.value }
                  }
                }
              }
            }
          }
        }
      case '&&':
        this.ensureBooleanType(
          left,
          'Left operand of && must be boolean',
          expr.line,
          expr.column
        )
        this.ensureBooleanType(
          right,
          'Right operand of && must be boolean',
          expr.line,
          expr.column
        )
        return {
          type: 'bool',
          value: {
            opcode: 'operator_and',
            fields: {},
            inputs: {
              OPERAND1: { type: 'bool', value: left.value as Reporter },
              OPERAND2: { type: 'bool', value: right.value as Reporter }
            }
          }
        }
      case '||':
        this.ensureBooleanType(
          left,
          'Left operand of || must be boolean',
          expr.line,
          expr.column
        )
        this.ensureBooleanType(
          right,
          'Right operand of || must be boolean',
          expr.line,
          expr.column
        )
        return {
          type: 'bool',
          value: {
            opcode: 'operator_or',
            fields: {},
            inputs: {
              OPERAND1: { type: 'bool', value: left.value as Reporter },
              OPERAND2: { type: 'bool', value: right.value as Reporter }
            }
          }
        }
      default:
        throw new CompilerError(
          `Unsupported binary operator: ${expr.operator}`,
          expr.line,
          expr.column
        )
    }
  }

  private parseUnaryExpression(expr: UnaryExpression): TypedValue {
    const operand = this.parseExpr(expr.operand)

    switch (expr.operator) {
      case '!':
        this.ensureBooleanType(
          operand,
          'Operand of ! must be boolean',
          expr.line,
          expr.column
        )
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: { type: 'bool', value: operand.value as Reporter }
            }
          }
        }
      case '-':
        return {
          type: 'any',
          value: {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: '0' },
              NUM2: { type: 'any', value: operand.value }
            }
          }
        }
      case '+':
        return {
          type: 'any',
          value: {
            opcode: 'operator_add',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: '0' },
              NUM2: { type: 'any', value: operand.value }
            }
          }
        }
      default:
        throw new CompilerError(
          `Unsupported unary operator: ${expr.operator}`,
          expr.line,
          expr.column
        )
    }
  }

  // Helper method to call a method on a value (used for chained calls)
  private callMethodOnValue(
    value: Reporter | string,
    methodName: string,
    args: (Reporter | string)[],
    line: number,
    column: number
  ): TypedValue {
    // Use callMethod for calling methods on values
    try {
      const result = callMethod(value, methodName, args)
      // callMethod should return TypedValue for reporters
      if ('type' in result && 'value' in result) {
        return result as TypedValue
      }
      throw new Error(`Method ${methodName} is not a reporter method`)
    } catch (error) {
      throw new CompilerError((error as Error).message, line, column)
    }
  }

  // Type checking utility
  private ensureBooleanType(
    value: TypedValue,
    message: string,
    line: number,
    column: number
  ): void {
    if (value.type !== 'bool') {
      throw new CompilerError(message, line, column)
    }
  }

  // Handle computed access (test[index])
  private parseComputedAccess(
    objectName: string,
    index: TypedValue,
    line: number,
    column: number
  ): TypedValue {
    const varType = this.getVariableType(objectName)
    if (!varType) {
      throw new CompilerError(`Variable ${objectName} not found`, line, column)
    }

    if (varType === 'list') {
      // For lists, use list.at(index)
      try {
        const varValue = this.getVariableReporter(objectName)
        if (!varValue) {
          throw new Error(`Variable ${objectName} not found`)
        }
        const methodResult = this.callMethodOnValue(
          varValue,
          'at',
          [index.value],
          line,
          column
        )
        return methodResult
      } catch (error) {
        if (error instanceof CompilerError) {
          throw error
        }
        throw new CompilerError((error as Error).message, line, column)
      }
    } else if (varType === 'scalar' || varType === 'arg_any') {
      // For any/scalar, use string.at(index)
      try {
        const varValue = this.getVariableReporter(objectName)
        if (!varValue) {
          throw new Error(`Variable ${objectName} not found`)
        }
        const methodResult = this.callMethodOnValue(
          varValue,
          'at',
          [index.value],
          line,
          column
        )
        return methodResult
      } catch (error) {
        if (error instanceof CompilerError) {
          throw error
        }
        throw new CompilerError((error as Error).message, line, column)
      }
    } else {
      throw new CompilerError(
        `Cannot use computed access on ${varType}`,
        line,
        column
      )
    }
  }

  // Parse ScratchFunction
  private parseScratchFunction(func: ScratchFunction): CompiledFunction {
    // 使用函数的作用域来编译函数体
    const oldScope = this.currentScope

    this.currentScope = func.scope

    try {
      // 确定函数返回类型
      const returnType: 'bool' | 'any' | 'void' =
        func.decl.returnType.name === 'bool'
          ? 'bool'
          : func.decl.returnType.name === 'void'
            ? 'void'
            : 'any'

      // 编译函数体
      const impl = this.parseBlockStatement(func.decl.body, returnType)

      // if (func.decl.returnType.name !== 'void') {
      //   // 确保所有代码路径都返回值
      //   const allPathsReturn = (blocks: Block[]): boolean => {
      //     for (const block of blocks) {
      //       if (block.opcode === 'control_if_else') {
      //         const substack1: Block[] =
      //           (block.inputs.SUBSTACK as SubstackInput)?.value || []
      //         const substack2: Block[] =
      //           (block.inputs.SUBSTACK2 as SubstackInput)?.value || []
      //         if (!allPathsReturn(substack1) || !allPathsReturn(substack2)) {
      //           return false
      //         }
      //       } else if (block.opcode === 'control_if') {
      //         const substack: Block[] =
      //           (block.inputs.SUBSTACK as SubstackInput)?.value || []
      //         if (!allPathsReturn(substack)) {
      //           return false
      //         }
      //       } else if (block.opcode === 'procedures_return') {
      //         return true
      //       }
      //     }
      //     return false
      //   }

      //   if (!allPathsReturn(impl)) {
      //     throw new CompilerError(
      //       `Not all code paths return a value in function ${func.decl.name.name}`,
      //       func.decl.name.line,
      //       func.decl.name.column
      //     )
      //   }
      // }

      return {
        decl: func.decl,
        proccode: func.proccode,
        impl: impl
      }
    } finally {
      // 恢复原始作用域
      this.currentScope = oldScope
    }
  }

  // Parse Program (top-level)
  private parseProgram(
    program: Program,
    imported?: boolean
  ): CompiledResult | Promise<CompiledResult> {
    const scripts: Script[] = []
    const errors: Error[] = []

    // Step 1: Extract variables, externs, and modules from Program
    // Store them in temporary maps with qualified names
    const variableMap = new Map<
      string,
      [Variable, string | boolean | number | (string | boolean | number)[]]
    >()
    const externMap = new Map<string, External>()

    // Process imports first if importResolver is provided
    if (this.options?.importResolver && !imported) {
      return this.processImports(program).then(({ modules, results }) => {
        // Merge imported modules into current module
        for (const importedModule of modules) {
          mergeModule(
            importedModule,
            this.currentModule,
            0, // line number not available here
            0 // column number not available here
          )
        }

        // Compile the main program
        const mainResult = this.parseProgram(program, true) as CompiledResult

        // Merge import results with main result
        return this.mergeImportResults(mainResult, results)
      })
    }

    // Collect pending aliases across all modules
    type PendingAlias = {
      module: Module
      aliasPath: string[]
      parentModule: ModuleInfo
      line: number
      column: number
    }
    const pendingAliases: PendingAlias[] = []

    this.extractDeclarations(
      program.body,
      [],
      variableMap,
      externMap,
      this.currentModule,
      pendingAliases
    )

    // Resolve all aliases after all modules have been declared
    for (const pending of pendingAliases) {
      const targetModule = this.resolveModuleAlias(
        pending.aliasPath,
        pending.parentModule,
        pending.line,
        pending.column,
        new Set()
      )
      // Replace the temporary Module with a ModuleAlias
      const moduleAlias: ModuleAlias = {
        name: pending.module.name,
        alias: targetModule
      }
      const resolvedParent = followAlias(pending.parentModule)
      resolvedParent.children.set(pending.module.name, moduleAlias)
    }

    // Step 2: Compile functions and hat blocks in Program and child modules
    // Store them in temporary functionMap and scripts array
    const functionMap = new Map<string, CompiledFunction>()

    this.compileModule(
      functionMap,
      scripts,
      errors,
      this.currentModule,
      program.body,
      []
    )

    // If there are errors, throw them all at once using ErrorList
    if (errors.length > 0) {
      throw new ErrorList(errors)
    }

    // Convert maps to arrays for the result
    const compiledFunctions = Array.from(functionMap.values())
    const compiledVariables = Array.from(variableMap.values())

    return {
      scripts,
      functions: compiledFunctions,
      variables: compiledVariables,
      externs: Array.from(externMap.values())
    }
  }

  /**
   * Step 1: Extract variables, externs, and modules from statements
   * Recursively process child modules
   *
   * Two-phase approach for alias resolution:
   * Phase 1: Collect all module declarations, store alias paths without resolving
   * Phase 2: Resolve all aliases, following chains and detecting cycles (done at top level)
   */
  private extractDeclarations(
    statements: Statement[],
    parentPath: string[],
    variableMap: Map<
      string,
      [Variable, string | boolean | number | (string | boolean | number)[]]
    >,
    externMap: Map<string, External>,
    parentModule: Module,
    pendingAliases: Array<{
      module: ModuleInfo
      aliasPath: string[]
      parentModule: ModuleInfo
      line: number
      column: number
    }>
  ): void {
    // Phase 1: Collect all declarations

    for (const stmt of statements) {
      if (stmt.type === 'ModuleDeclaration') {
        const modDecl = stmt as ModuleDeclaration
        const moduleName = modDecl.name.name
        const modulePath = [...parentPath, moduleName]

        if (modDecl.alias) {
          // This is an alias: module B = C.D
          // Parse the alias path but don't resolve it yet
          let aliasPath: string[] = []
          if (modDecl.alias.type === 'Identifier') {
            aliasPath = [(modDecl.alias as IdentifierExpression).name]
          } else if (modDecl.alias.type === 'MemberExpression') {
            const parsePath = (expr: Expression): string[] => {
              if (expr.type === 'Identifier') {
                return [(expr as IdentifierExpression).name]
              } else if (expr.type === 'MemberExpression') {
                const memberExpr = expr as MemberExpression
                const objectPath = parsePath(memberExpr.object)
                const propertyName = (
                  memberExpr.property as IdentifierExpression
                ).name
                return [...objectPath, propertyName]
              }
              throw new CompilerError(
                `Invalid alias expression`,
                expr.line,
                expr.column
              )
            }
            aliasPath = parsePath(modDecl.alias)
          } else {
            throw new CompilerError(
              `Invalid alias expression`,
              modDecl.alias.line,
              modDecl.alias.column
            )
          }

          // Create module alias placeholder (will be resolved in phase 2)
          const moduleInfo: Module = {
            name: moduleName,
            parent: parentModule,
            functions: new Map(),
            variables: new Map(),
            externs: new Map(),
            children: new Map()
          }

          // Store for phase 2 resolution
          pendingAliases.push({
            module: moduleInfo,
            aliasPath,
            parentModule,
            line: modDecl.line,
            column: modDecl.column
          })

          const resolved = followAlias(parentModule)
          resolved.children.set(moduleName, moduleInfo)
        } else if (modDecl.body) {
          // Create module info
          const moduleInfo: Module = {
            name: moduleName,
            parent: parentModule,
            functions: new Map(),
            variables: new Map(),
            externs: new Map(),
            children: new Map()
          }

          // Recursively extract from module body
          this.extractDeclarations(
            modDecl.body,
            modulePath,
            variableMap,
            externMap,
            moduleInfo,
            pendingAliases
          )

          const resolved = followAlias(parentModule)
          // Merge or add module
          if (resolved.children.has(moduleName)) {
            const existing = resolved.children.get(moduleName)!
            const existingResolved = followAlias(existing)
            mergeModule(
              moduleInfo,
              existingResolved,
              modDecl.line,
              modDecl.column
            )
          } else {
            resolved.children.set(moduleName, moduleInfo)
          }
        }
      } else if (stmt.type === 'ExternDeclaration') {
        const externDecl = stmt as ExternDeclaration
        const externName = externDecl.name.name
        const externValue = externDecl.value as External

        if (!externValue.opcode || !externValue.type || !externValue.args) {
          throw new CompilerError(
            `Invalid extern declaration: must have opcode, type, and args`,
            externDecl.line,
            externDecl.column
          )
        }

        if (!['void', 'any', 'bool', 'hat'].includes(externValue.type)) {
          throw new CompilerError(
            `Invalid extern type: ${externValue.type}, must be one of: void, any, bool, hat`,
            externDecl.line,
            externDecl.column
          )
        }

        const qualifiedName =
          parentPath.length > 0
            ? [...parentPath, externName].join('.')
            : externName

        if (externMap.has(qualifiedName)) {
          throw new CompilerError(
            `Extern ${qualifiedName} is already declared`,
            externDecl.line,
            externDecl.column
          )
        }

        const resolved = followAlias(parentModule)
        resolved.externs.set(externName, externValue)
        externMap.set(qualifiedName, externValue)
      } else if (stmt.type === 'VariableDeclaration') {
        this.processVariableDeclaration(
          stmt as VariableDeclaration,
          parentPath,
          variableMap,
          parentModule,
          null
        )
      } else if (stmt.type === 'FunctionDeclaration') {
        // Only create ScratchFunction and add to module, don't compile yet
        const funcDecl = stmt as FunctionDeclaration
        const func = new ScratchFunction(funcDecl, null, parentModule)

        const resolved = followAlias(parentModule)
        if (resolved.functions.has(funcDecl.name.name)) {
          throw new CompilerError(
            `Function ${funcDecl.name.name} is already declared in module`,
            funcDecl.line,
            funcDecl.column
          )
        }

        resolved.functions.set(funcDecl.name.name, func)
      } else if (stmt.type === 'DecoratorStatement') {
        const decorator = stmt as DecoratorStatement
        if (decorator.name.name === 'export') {
          if (decorator.target.type === 'VariableDeclaration') {
            const arg = decorator.arguments[0]
            if (
              decorator.arguments.length !== 1 ||
              typeof arg.value !== 'string'
            ) {
              throw new CompilerError(
                '@export requires exactly one string argument',
                decorator.line,
                decorator.column
              )
            }
            this.processVariableDeclaration(
              decorator.target as VariableDeclaration,
              parentPath,
              variableMap,
              parentModule,
              arg.value as string
            )
          } else if (decorator.target.type === 'FunctionDeclaration') {
            // Only create ScratchFunction and add to module, don't compile yet
            const funcDecl = decorator.target as FunctionDeclaration
            const arg = decorator.arguments[0]

            if (
              decorator.arguments.length !== 1 ||
              typeof arg.value !== 'string'
            ) {
              throw new CompilerError(
                '@export requires exactly one string argument',
                decorator.line,
                decorator.column
              )
            }

            const exportName = arg.value as string
            const func = new ScratchFunction(funcDecl, exportName, parentModule)

            const resolved = followAlias(parentModule)
            if (resolved.functions.has(funcDecl.name.name)) {
              throw new CompilerError(
                `Function ${funcDecl.name.name} is already declared in module`,
                funcDecl.line,
                funcDecl.column
              )
            }

            resolved.functions.set(funcDecl.name.name, func)
          }
        }
      }
    }
  }

  /**
   * Resolve a module alias path to the actual module, following alias chains
   * and detecting circular references
   */
  private resolveModuleAlias(
    path: string[],
    contextModule: ModuleInfo,
    line: number,
    column: number,
    visited: Set<ModuleInfo>
  ): Module {
    if (path.length === 0) {
      throw new CompilerError('Module alias path cannot be empty', line, column)
    }

    // Start from root module
    let current: ModuleInfo = contextModule

    // Find the root module
    while (true) {
      const resolved = followAlias(current)
      if (resolved.parent === null) {
        current = resolved
        break
      }
      current = resolved.parent
    }

    // Navigate to the target module
    for (let i = 0; i < path.length; i++) {
      const segment = path[i]
      const resolved = followAlias(current)
      const child: ModuleInfo | undefined = resolved.children.get(segment)

      if (!child) {
        throw new CompilerError(
          `Module ${path.slice(0, i + 1).join('.')} not found`,
          line,
          column
        )
      }

      current = child
    }

    // Follow alias chain
    try {
      return followAlias(current)
    } catch (e) {
      if (e instanceof Error) {
        throw new CompilerError(e.message, line, column)
      }
      throw e
    }
  }

  /**
   * Process a variable declaration
   */
  private processVariableDeclaration(
    varDecl: VariableDeclaration,
    parentPath: string[],
    variableMap: Map<
      string,
      [Variable, string | boolean | number | (string | boolean | number)[]]
    >,
    parentModule: Module,
    exportName: string | null
  ): void {
    const varName = varDecl.name
    let value: string | number | boolean | (string | number | boolean)[]

    if (varDecl.initializer.type === 'ArrayExpression') {
      value = (varDecl.initializer as ArrayExpression).elements.map(elem => {
        if (elem.type === 'Literal') {
          return (elem as LiteralExpression).value
        } else {
          throw new CompilerError(
            'Array elements must be literals',
            elem.line,
            elem.column
          )
        }
      })
    } else if (varDecl.initializer.type === 'Literal') {
      value = (varDecl.initializer as LiteralExpression).value
    } else {
      throw new CompilerError(
        `Variable initializer must be a literal or array, got ${varDecl.initializer.type}`,
        varDecl.initializer.line,
        varDecl.initializer.column
      )
    }

    const variable: Variable = {
      name:
        parentPath.length > 0 ? [...parentPath, varName].join('.') : varName,
      exportName: exportName,
      type: Array.isArray(value) ? 'list' : 'scalar',
      isGlobal: varDecl.isGlobal
    }

    const qualifiedName = variable.name

    if (variableMap.has(qualifiedName)) {
      throw new CompilerError(
        `Variable ${qualifiedName} is already declared`,
        varDecl.line,
        varDecl.column
      )
    }

    const resolved = parentModule
    resolved.variables.set(varName, [variable, value])
    variableMap.set(qualifiedName, [variable, value])
  }

  /**
   * Step 2: Compile all functions and hat blocks in a module
   * Recursively compile functions and hat blocks in child modules
   */
  private compileModule(
    functionMap: Map<string, CompiledFunction>,
    scripts: Script[],
    errors: Error[],
    module: ModuleInfo,
    statements: Statement[],
    parentPath: string[] = []
  ): void {
    const resolved = followAlias(module)

    // Compile all functions in this module
    for (const [funcName, func] of resolved.functions) {
      try {
        const qualifiedName =
          parentPath.length > 0 ? [...parentPath, funcName].join('.') : funcName

        // Compile the function using a separate context
        const funcContext = new Context(
          resolved,
          this.options,
          this.importedModules
        )
        const compiled = funcContext.parseScratchFunction(func)
        functionMap.set(qualifiedName, compiled)
      } catch (error) {
        errors.push(error as Error)
      }
    }

    // Process hat blocks in this module
    for (const stmt of statements) {
      try {
        if (stmt.type === 'ExpressionStatement') {
          const exprStmt = stmt as ExpressionStatement
          if (exprStmt.expression.type === 'CallExpression') {
            const callExpr = exprStmt.expression as CallExpression
            if (callExpr.then) {
              // Create a context for this module to parse hat blocks
              const moduleContext = new Context(
                resolved,
                this.options,
                this.importedModules
              )
              const hatScript = moduleContext.parseHatCall(callExpr)
              if (hatScript) {
                scripts.push(hatScript)
              }
            }
          }
        } else if (stmt.type === 'ModuleDeclaration') {
          const modDecl = stmt as ModuleDeclaration
          const moduleName = modDecl.name.name

          // Process child module if it has a body
          if (modDecl.body && !modDecl.alias) {
            const childModule = resolved.children.get(moduleName)
            if (childModule) {
              const childPath = [...parentPath, moduleName]
              this.compileModule(
                functionMap,
                scripts,
                errors,
                childModule,
                modDecl.body,
                childPath
              )
            }
          }
        }
        // Skip other statement types (they are declarations already processed)
      } catch (error) {
        errors.push(error as Error)
      }
    }

    // Note: We don't need to recursively process children here anymore
    // because we handle ModuleDeclaration statements above with their bodies
  }

  // Parse Statement (in function/block context)
  private parseStatement(
    stmt: Statement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    switch (stmt.type) {
      case 'NoopStatement':
        return []
      case 'ExpressionStatement':
        return this.parseExpressionStatement(
          stmt as ExpressionStatement,
          functionReturnType
        )
      case 'AssignmentStatement':
        return this.parseAssignmentStatement(stmt as AssignmentStatement)
      case 'IncrementStatement':
        return this.parseIncrementStatement(stmt as IncrementStatement)
      case 'IfStatement':
        return this.parseIfStatement(stmt as IfStatement, functionReturnType)
      case 'WhileStatement':
        return this.parseWhileStatement(
          stmt as WhileStatement,
          functionReturnType
        )
      case 'ForStatement':
        return this.parseForStatement(stmt as ForStatement, functionReturnType)
      case 'LoopStatement':
        return this.parseLoopStatement(
          stmt as LoopStatement,
          functionReturnType
        )
      case 'ReturnStatement':
        return this.parseReturnStatement(
          stmt as ReturnStatement,
          functionReturnType
        )
      case 'BlockStatement':
        return this.parseBlockStatement(
          stmt as BlockStatement,
          functionReturnType
        )
      case 'ModuleDeclaration':
        throw new CompilerError(
          'Module declarations are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      case 'ExternDeclaration':
        throw new CompilerError(
          'Extern declarations are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      case 'VariableDeclaration':
        throw new CompilerError(
          'Variable declarations are not allowed in function bodies (use assignment instead)',
          stmt.line,
          stmt.column
        )
      case 'FunctionDeclaration':
        throw new CompilerError(
          'Function declarations are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      case 'DecoratorStatement':
        throw new CompilerError(
          'Decorators are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      default:
        throw new CompilerError(
          `Unsupported statement type: ${stmt.type}`,
          stmt.line,
          stmt.column
        )
    }
  }

  // Parse hat block calls
  private parseHatCall(callExpr: CallExpression): Script | null {
    if (!callExpr.then) return null

    if (callExpr.callee.type === 'MemberExpression') {
      const memberExpr = callExpr.callee as MemberExpression
      if (
        memberExpr.object.type === 'Identifier' &&
        memberExpr.property.type === 'Identifier'
      ) {
        const objectName = (memberExpr.object as IdentifierExpression).name
        const functionName = (memberExpr.property as IdentifierExpression).name

        // Try to find extern: search from current module up to parent modules
        const qualifiedExternName = `${objectName}.${functionName}`
        let entry: External | undefined
        let searchModule: ModuleInfo | null = this.currentModule

        while (searchModule && !entry) {
          const resolved = followAlias(searchModule)

          // Check qualified name in current module
          entry = resolved.externs.get(qualifiedExternName)

          if (!entry) {
            // Check in child modules
            const module = resolved.children.get(objectName)
            if (module) {
              const moduleResolved = followAlias(module)
              entry = moduleResolved.externs.get(functionName)
            }
          }

          // Move to parent module if not found
          if (!entry) {
            searchModule = resolved.parent
          }
        }

        if (!entry || entry.type !== 'hat') return null

        const parsedArgs = callExpr.arguments.map(arg => this.parseExpr(arg))
        const inputs: {
          [key: string]: BooleanInput | AnyInput | SubstackInput
        } = Object.assign({}, entry.inputs)
        const fields: { [key: string]: string } = Object.assign(
          {},
          entry.fields
        )

        // Match arguments with entry arguments
        if (parsedArgs.length !== entry.args.length) {
          throw new CompilerError(
            `${qualifiedExternName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`,
            callExpr.line,
            callExpr.column
          )
        }

        for (let i = 0; i < entry.args.length; i++) {
          const argDef = entry.args[i]
          const argValue = parsedArgs[i]

          if (argDef.type === 'field') {
            // Must be literal
            const rawArgValue = callExpr.arguments[i]
            if (rawArgValue.type !== 'Literal') {
              throw new CompilerError(
                `Argument ${argDef.name} must be a literal for field`,
                callExpr.line,
                callExpr.column
              )
            }
            const literal = rawArgValue as LiteralExpression
            if (typeof literal.value !== 'string') {
              throw new CompilerError(
                `Argument ${argDef.name} must be a string literal`,
                callExpr.line,
                callExpr.column
              )
            }
            const fieldValue = String(literal.value)
            if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
              throw new CompilerError(
                `Argument ${argDef.name} has invalid value ${fieldValue}`,
                callExpr.line,
                callExpr.column
              )
            }
            fields[argDef.name] = argDef.menu
              ? argDef.menu[fieldValue]
              : fieldValue
          }

          if (argDef.type === 'bool' && argValue.type !== 'bool') {
            throw new CompilerError(
              `Argument ${argDef.name} must be boolean`,
              callExpr.line,
              callExpr.column
            )
          }

          inputs[argDef.name] =
            argValue.type === 'bool'
              ? { type: 'bool', value: argValue.value as Reporter }
              : { type: 'any', value: argValue.value }
        }

        const hat = {
          opcode: entry.opcode,
          fields,
          inputs
        }

        const blocks = this.parseBlockStatement(callExpr.then, null)

        return { hat, blocks }
      }
    } else if (callExpr.callee.type === 'Identifier') {
      // Direct extern call (e.g., greenflag {} when extern is in current or parent module)
      const funcName = (callExpr.callee as IdentifierExpression).name

      // Search from current module up to parent modules
      let entry: External | undefined
      let searchModule: ModuleInfo | null = this.currentModule

      while (searchModule && !entry) {
        const resolved = followAlias(searchModule)
        entry = resolved.externs.get(funcName)

        // Move to parent module if not found
        if (!entry) {
          searchModule = resolved.parent
        }
      }

      if (!entry || entry.type !== 'hat') return null

      // Parse arguments
      const parsedArgs = callExpr.arguments.map(arg => this.parseExpr(arg))
      const inputs: {
        [key: string]: BooleanInput | AnyInput | SubstackInput
      } = Object.assign({}, entry.inputs)
      const fields: { [key: string]: string } = Object.assign({}, entry.fields)

      // Match arguments with entry arguments
      if (parsedArgs.length !== entry.args.length) {
        throw new CompilerError(
          `${funcName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`,
          callExpr.line,
          callExpr.column
        )
      }

      for (let i = 0; i < entry.args.length; i++) {
        const argDef = entry.args[i]
        const argValue = parsedArgs[i]

        if (argDef.type === 'field') {
          // Must be literal
          const rawArgValue = callExpr.arguments[i]
          if (rawArgValue.type !== 'Literal') {
            throw new CompilerError(
              `Argument ${argDef.name} must be a literal for field`,
              callExpr.line,
              callExpr.column
            )
          }
          const literal = rawArgValue as LiteralExpression
          if (typeof literal.value !== 'string') {
            throw new CompilerError(
              `Argument ${argDef.name} must be a string literal`,
              callExpr.line,
              callExpr.column
            )
          }
          const fieldValue = String(literal.value)
          if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
            throw new CompilerError(
              `Argument ${argDef.name} has invalid value ${fieldValue}`,
              callExpr.line,
              callExpr.column
            )
          }
          fields[argDef.name] = argDef.menu
            ? argDef.menu[fieldValue]
            : fieldValue
        }

        if (argDef.type === 'bool' && argValue.type !== 'bool') {
          throw new CompilerError(
            `Argument ${argDef.name} must be boolean`,
            callExpr.line,
            callExpr.column
          )
        }

        inputs[argDef.name] =
          argValue.type === 'bool'
            ? { type: 'bool', value: argValue.value as Reporter }
            : { type: 'any', value: argValue.value }
      }

      const hat = {
        opcode: entry.opcode,
        fields,
        inputs
      }

      const blocks = this.parseBlockStatement(callExpr.then, null)

      return { hat, blocks }
    }

    return null
  }

  // Parse hat member calls (like event.greenflag {})
  // This method is no longer needed as parser wraps member expressions with then blocks into CallExpressions

  // Statement parsing methods
  private parseExpressionStatement(
    stmt: ExpressionStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const expr = stmt.expression

    if (expr.type === 'CallExpression') {
      const callExpr = expr as CallExpression

      // Resolve the callee
      try {
        const callee = this.parseExprImpl(callExpr.callee)

        // Check if this is a method call on a variable
        if ('target' in callee && 'method' in callee) {
          // This is a command/reporter call on a variable
          const methodObj = callee
          const args = callExpr.arguments.map(arg => this.parseExpr(arg).value)

          const result: Block[] | TypedValue = methodObj.method(args)

          // In statement context, we expect Block[]
          if (!Array.isArray(result)) {
            throw new Error(
              `This method returns a value and cannot be used as a statement`
            )
          }
          return result
        }

        const parsedArgs = callExpr.arguments.map(arg => this.parseExpr(arg))

        // Handle different callee types
        if (callee instanceof ScratchFunction) {
          // Function call as command
          return this.callFunctionAsCommand(
            callee,
            parsedArgs,
            callExpr.line,
            callExpr.column
          )
        } else if ('opcode' in callee && 'type' in callee) {
          // External call as command
          return this.callExternalAsCommand(
            callee,
            parsedArgs,
            callExpr.then,
            functionReturnType,
            callExpr.line,
            callExpr.column
          )
        } else {
          throw new CompilerError(
            'Expression cannot be called as a command',
            callExpr.line,
            callExpr.column
          )
        }
      } catch (error) {
        // If resolveValue fails, check if this is a method call on a variable
        if (callExpr.callee.type === 'MemberExpression') {
          const memberExpr = callExpr.callee as MemberExpression
          if (
            memberExpr.object.type === 'Identifier' &&
            memberExpr.property.type === 'Identifier'
          ) {
            const objectName = (memberExpr.object as IdentifierExpression).name
            const propertyName = (memberExpr.property as IdentifierExpression)
              .name

            // Check if this is a method call on a variable
            const varType = this.getVariableType(objectName)
            if (varType) {
              const args = callExpr.arguments.map(
                arg => this.parseExpr(arg).value
              )
              try {
                // Get the variable
                const variable = this.getVariable(objectName)
                if (!variable) {
                  throw new Error(`Variable ${objectName} not found`)
                }

                // Use callMethod to invoke the method
                const methodResult = callMethod(variable, propertyName, args)

                // Should return Block[] for statement methods
                if (!Array.isArray(methodResult)) {
                  throw new Error(
                    `Method ${propertyName} is not a command method`
                  )
                }

                return methodResult
              } catch (methodError) {
                if (methodError instanceof CompilerError) {
                  throw methodError
                }
                throw new CompilerError(
                  (methodError as Error).message,
                  callExpr.line,
                  callExpr.column
                )
              }
            }
          }
        }

        // Re-throw the original error
        if (error instanceof CompilerError) {
          throw error
        }
        throw new CompilerError(
          (error as Error).message,
          callExpr.line,
          callExpr.column
        )
      }
    }

    throw new CompilerError(
      'Expression statements must be function calls or method calls',
      stmt.line,
      stmt.column
    )
  }

  /**
   * Call a ScratchFunction as a command
   */
  private callFunctionAsCommand(
    func: ScratchFunction,
    args: TypedValue[],
    line: number,
    column: number
  ): Block[] {
    if (func.decl.returnType.name !== 'void') {
      throw new CompilerError(
        `Function ${func.decl.name.name} returns a value and cannot be used as a command`,
        line,
        column
      )
    }

    // Check argument count
    if (args.length !== func.decl.parameters.length) {
      throw new CompilerError(
        `Function ${func.decl.name.name} expects ${func.decl.parameters.length} arguments, got ${args.length}`,
        line,
        column
      )
    }

    // Check argument types
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      if (paramType === 'bool' && args[i].type !== 'bool') {
        throw new CompilerError(
          `Argument ${i + 1} of function ${func.decl.name.name} must be boolean`,
          line,
          column
        )
      }
    }

    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      {}
    for (let i = 0; i < args.length; i++) {
      const paramName = func.decl.parameters[i].name.name
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      inputs[paramName] =
        paramType === 'bool'
          ? { type: 'bool', value: args[i].value as Reporter }
          : { type: 'any', value: args[i].value }
    }

    const names = func.decl.parameters.map(p => p.name.name)

    return [
      {
        opcode: 'procedures_call',
        fields: {},
        inputs,
        mutation: {
          tagName: 'mutation',
          proccode: func.proccode,
          children: [],
          warp: func.decl.once ? 'true' : 'false',
          argumentids: JSON.stringify(names),
          argumentnames: JSON.stringify(names),
          argumentdefaults: JSON.stringify(
            func.decl.parameters.map(p =>
              p.type.name === 'any' ? '' : 'false'
            )
          )
        }
      }
    ]
  }

  /**
   * Call an External as a command
   */
  private callExternalAsCommand(
    external: External,
    args: TypedValue[],
    thenBlock: BlockStatement | undefined,
    functionReturnType: 'bool' | 'any' | 'void' | null,
    line: number,
    column: number
  ): Block[] {
    if (external.type !== 'void') {
      throw new CompilerError(
        `External ${external.opcode} cannot be used as a command (type: ${external.type})`,
        line,
        column
      )
    }

    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      Object.assign({}, external.inputs)
    const fields: { [key: string]: string } = Object.assign({}, external.fields)

    // Find substack argument if any
    let substackArgDef = external.args.find(
      (
        arg:
          | ExternalArgumentAny
          | ExternalArgumentBoolean
          | ExternalArgumentSubstack
          | ExternalArgumentField
      ) => arg.type === 'substack'
    )

    // Match arguments with external arguments (non-substack arguments)
    const nonSubstackArgs = external.args.filter(
      (
        arg:
          | ExternalArgumentAny
          | ExternalArgumentBoolean
          | ExternalArgumentSubstack
          | ExternalArgumentField
      ) => arg.type !== 'substack'
    )
    if (args.length !== nonSubstackArgs.length) {
      throw new CompilerError(
        `External ${external.opcode} expects ${nonSubstackArgs.length} non-substack arguments, got ${args.length}`,
        line,
        column
      )
    }

    for (let i = 0; i < nonSubstackArgs.length; i++) {
      const argDef = nonSubstackArgs[i]
      const argValue = args[i]

      if (argDef.type === 'bool' && argValue.type !== 'bool') {
        throw new CompilerError(
          `Argument ${argDef.name} must be boolean`,
          line,
          column
        )
      }

      if (argDef.type === 'field') {
        // Must be literal
        const rawArgValue = args[i]
        if (
          rawArgValue.type !== 'any' ||
          typeof rawArgValue.value !== 'string'
        ) {
          throw new CompilerError(
            `Argument ${argDef.name} must be a string literal for field`,
            line,
            column
          )
        }
        const fieldValue = String(rawArgValue.value)
        if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
          throw new CompilerError(
            `Argument ${argDef.name} has invalid value ${fieldValue}`,
            line,
            column
          )
        }
        fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue
      } else {
        inputs[argDef.name] =
          argValue.type === 'bool'
            ? { type: 'bool', value: argValue.value as Reporter }
            : { type: 'any', value: argValue.value }
      }
    }

    // Handle substack
    if (substackArgDef) {
      if (thenBlock) {
        inputs[substackArgDef.name] = {
          type: 'substack',
          value: this.parseBlockStatement(thenBlock, functionReturnType)
        }
      } else {
        inputs[substackArgDef.name] = { type: 'substack', value: [] }
      }
    } else if (thenBlock) {
      throw new CompilerError(
        `External ${external.opcode} does not accept a then block`,
        line,
        column
      )
    }

    return [
      {
        opcode: external.opcode,
        fields,
        inputs
      }
    ]
  }

  private operateVar(
    varName: string,
    operator: string,
    value: TypedValue,
    line: number,
    column: number
  ): Block[] {
    // Get the variable first
    const variable = this.getVariable(varName)
    if (!variable) {
      throw new CompilerError(`Variable ${varName} not found`, line, column)
    }

    if (variable.type !== 'scalar') {
      throw new CompilerError(
        `Cannot perform ${operator} operation on list variable ${varName}`,
        line,
        column
      )
    }

    try {
      switch (operator) {
        case '=':
          return [
            {
              opcode: 'data_setvariableto',
              fields: {
                VARIABLE: variable.exportName ?? variable.name
              },
              inputs: {
                VALUE: { type: 'any', value: value.value }
              }
            }
          ]
        case '+=':
          return [
            {
              opcode: 'data_changevariableby',
              fields: {
                VARIABLE: variable.exportName ?? variable.name
              },
              inputs: {
                VALUE: { type: 'any', value: value.value }
              }
            }
          ]
        case '-=':
          return [
            {
              opcode: 'data_changevariableby',
              fields: {
                VARIABLE: variable.exportName ?? variable.name
              },
              inputs: {
                VALUE: {
                  type: 'any',
                  value: {
                    opcode: 'operator_subtract',
                    fields: {},
                    inputs: {
                      NUM1: { type: 'any', value: '0' },
                      NUM2: { type: 'any', value: value.value }
                    }
                  }
                }
              }
            }
          ]
        // *= -> = var * value, /= -> = var / value, etc.
        case '*=':
        case '/=':
        case '%=':
        case '..=': {
          const operatorMap: Record<string, string> = {
            '*': 'operator_multiply',
            '/': 'operator_divide',
            '%': 'operator_mod',
            '.': 'operator_join'
          }
          const op = operator[0] // Get the operator character
          const varValue = this.getVariableReporter(varName)
          if (!varValue) {
            throw new Error(`Variable ${varName} not found for operation`)
          }

          const operationBlock = {
            opcode: operatorMap[op],
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: varValue },
              NUM2: { type: 'any', value: value.value }
            }
          } satisfies Reporter

          return [
            {
              opcode: 'data_setvariableto',
              fields: {
                VARIABLE: variable.exportName ?? variable.name
              },
              inputs: {
                VALUE: { type: 'any', value: operationBlock }
              }
            }
          ]
        }
        default:
          throw new CompilerError(
            `Unsupported operator: ${operator}`,
            line,
            column
          )
      }
    } catch (error) {
      if (error instanceof CompilerError) {
        throw error
      }
      throw new CompilerError((error as Error).message, line, column)
    }
  }

  private operateComputed(
    memberExpr: MemberExpression,
    operator: string,
    value: TypedValue,
    line: number,
    column: number
  ): Block[] {
    if (!memberExpr.computed) {
      throw new CompilerError(
        'Dot notation assignments are not supported',
        line,
        column
      )
    }

    if (memberExpr.object.type !== 'Identifier') {
      throw new CompilerError(
        'Only simple computed assignments are supported',
        line,
        column
      )
    }

    const objectName = (memberExpr.object as IdentifierExpression).name
    const varType = this.getVariableType(objectName)

    if (varType !== 'list') {
      throw new CompilerError(
        'Computed assignment is only supported for lists',
        line,
        column
      )
    }

    switch (operator) {
      case '=': {
        const index = this.parseExpr(memberExpr.property)

        // Use list.replace(index, value)
        try {
          const variable = this.getVariable(objectName)
          if (!variable) {
            throw new Error(`Variable ${objectName} not found`)
          }

          const methodResult = callMethod(variable, 'replace', [
            index.value,
            value.value
          ])
          if (!Array.isArray(methodResult)) {
            throw new Error(`Method 'replace' is not a command method`)
          }

          return methodResult
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error
          }
          throw new CompilerError((error as Error).message, line, column)
        }
      }
      case '+=':
      case '-=':
      case '*=':
      case '/=':
      case '%=':
      case '..=': {
        // Use list.replace(list.get(index) op value)
        const operatorMap: Record<string, string> = {
          '*': 'operator_multiply',
          '/': 'operator_divide',
          '%': 'operator_mod',
          '.': 'operator_join'
        }
        const op = operator[0] // Get the operator character
        const index = this.parseExpr(memberExpr.property)
        let currentValue: TypedValue
        try {
          currentValue = this.parseComputedAccess(
            objectName,
            index,
            line,
            column
          )
        } catch (error) {
          throw new CompilerError((error as Error).message, line, column)
        }
        const additionBlock = {
          opcode: operatorMap[op],
          fields: {},
          inputs: {
            NUM1: { type: 'any', value: currentValue.value },
            NUM2: { type: 'any', value: value.value }
          }
        } satisfies Reporter
        try {
          const variable = this.getVariable(objectName)
          if (!variable) {
            throw new Error(`Variable ${objectName} not found`)
          }

          const methodResult = callMethod(variable, 'replace', [
            index.value,
            additionBlock
          ])
          if (!Array.isArray(methodResult)) {
            throw new Error(`Method 'replace' is not a command method`)
          }

          return methodResult
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error
          }
          throw new CompilerError((error as Error).message, line, column)
        }
      }
      default:
        throw new CompilerError(
          `Unsupported operator: ${operator}`,
          line,
          column
        )
    }
  }

  private parseIncrementStatement(stmt: IncrementStatement): Block[] {
    if (stmt.target.type === 'Identifier') {
      // Simple variable assignment
      const varName = (stmt.target as IdentifierExpression).name

      switch (stmt.operator) {
        case '++':
          return this.operateVar(
            varName,
            '+=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        case '--':
          return this.operateVar(
            varName,
            '-=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        default:
          throw new CompilerError(
            `Unsupported increment operator: ${stmt.operator}`,
            stmt.line,
            stmt.column
          )
      }
    } else if (stmt.target.type === 'MemberExpression') {
      // Computed member assignment like test[1]++
      const memberExpr = stmt.target as MemberExpression

      switch (stmt.operator) {
        case '++':
          return this.operateComputed(
            memberExpr,
            '+=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        case '--':
          return this.operateComputed(
            memberExpr,
            '-=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        default:
          throw new CompilerError(
            `Unsupported increment operator: ${stmt.operator}`,
            stmt.line,
            stmt.column
          )
      }
    } else {
      throw new CompilerError(
        'Unsupported increment target',
        stmt.line,
        stmt.column
      )
    }
  }
  private parseAssignmentStatement(stmt: AssignmentStatement): Block[] {
    const value = this.parseExpr(stmt.right)

    if (stmt.left.type === 'Identifier') {
      // Simple variable assignment
      const varName = (stmt.left as IdentifierExpression).name
      return this.operateVar(
        varName,
        stmt.operator,
        value,
        stmt.line,
        stmt.column
      )
    } else if (stmt.left.type === 'MemberExpression') {
      // Computed member assignment like test[1] = value
      const memberExpr = stmt.left as MemberExpression
      return this.operateComputed(
        memberExpr,
        stmt.operator,
        value,
        stmt.line,
        stmt.column
      )
    } else {
      throw new CompilerError(
        'Unsupported assignment target',
        stmt.line,
        stmt.column
      )
    }
  }

  private parseIfStatement(
    stmt: IfStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = this.parseExpr(stmt.condition)
    this.ensureBooleanType(
      condition,
      'If condition must be boolean',
      stmt.line,
      stmt.column
    )

    const thenBlocks: Block[] =
      stmt.then.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.then as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.then, functionReturnType)
    const elseBlocks = stmt.else
      ? stmt.else.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.else as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.else as Statement, functionReturnType)
      : []

    return [
      {
        opcode: elseBlocks.length > 0 ? 'control_if_else' : 'control_if',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: { type: 'substack', value: thenBlocks },
          ...(elseBlocks.length > 0 && {
            SUBSTACK2: { type: 'substack', value: elseBlocks }
          })
        }
      }
    ]
  }

  private parseWhileStatement(
    stmt: WhileStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = this.parseExpr(stmt.condition)
    this.ensureBooleanType(
      condition,
      'While condition must be boolean',
      stmt.line,
      stmt.column
    )

    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    return [
      {
        opcode: 'control_while',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: { type: 'substack', value: bodyBlocks }
        }
      }
    ]
  }

  private getBooleanLiteral(value: boolean): TypedValue {
    if (value)
      return {
        type: 'bool',
        value: {
          opcode: 'operator_not',
          fields: {},
          inputs: {}
        }
      }
    return {
      type: 'bool',
      value: {
        opcode: 'operator_not',
        fields: {},
        inputs: {
          OPERAND: {
            type: 'bool',
            value: {
              opcode: 'operator_not',
              fields: {},
              inputs: {}
            }
          }
        }
      }
    }
  }

  private parseForStatement(
    stmt: ForStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = stmt.condition
      ? this.parseExpr(stmt.condition)
      : this.getBooleanLiteral(true)
    this.ensureBooleanType(
      condition,
      'While condition must be boolean',
      stmt.line,
      stmt.column
    )

    const init = stmt.init
      ? this.parseStatement(stmt.init, functionReturnType)
      : []
    const increment = stmt.increment
      ? this.parseStatement(stmt.increment, functionReturnType)
      : []

    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    // Convert into while loop
    return [
      ...init,
      {
        opcode: 'control_while',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: {
            type: 'substack',
            value: [...bodyBlocks, ...increment]
          }
        }
      }
    ]
  }

  private parseLoopStatement(
    stmt: LoopStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    return [
      {
        opcode: 'control_forever',
        fields: {},
        inputs: {
          SUBSTACK: { type: 'substack', value: bodyBlocks }
        }
      }
    ]
  }

  private parseReturnStatement(
    stmt: ReturnStatement,
    functionReturnType: 'bool' | 'void' | 'any' | null
  ): Block[] {
    if (stmt.value) {
      if (functionReturnType === 'void' || !functionReturnType) {
        throw new CompilerError(
          `Cannot return a value from a void function or a non-function context`,
          stmt.line,
          stmt.column
        )
      }
      const returnValue = this.parseExpr(stmt.value)
      if (functionReturnType === 'bool') {
        this.ensureBooleanType(
          returnValue,
          'Return value must be boolean',
          stmt.line,
          stmt.column
        )
      }
      return [
        {
          opcode: 'procedures_return',
          fields: {},
          inputs: {
            VALUE: { type: 'any', value: returnValue.value }
          }
        }
      ]
    } else {
      if (functionReturnType !== 'void' && functionReturnType !== null) {
        throw new CompilerError(
          `Must return a value from a non-void function`,
          stmt.line,
          stmt.column
        )
      }
      return [
        {
          opcode: 'control_stop',
          fields: {
            STOP_OPTION: 'this script'
          },
          inputs: {}
        }
      ]
    }
  }

  private parseBlockStatement(
    stmt: BlockStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const blocks: Block[] = []
    for (const statement of stmt.body) {
      blocks.push(...this.parseStatement(statement, functionReturnType))
    }
    return blocks
  }
}

// Module system data structures
export type Module = {
  name: string
  parent: Module | null // Parent module for hierarchical lookup
  functions: Map<string, ScratchFunction>
  variables: Map<
    string,
    [Variable, string | boolean | number | (string | boolean | number)[]]
  >
  externs: Map<string, External>
  children: Map<string, ModuleInfo> // Nested modules
  filename?: string
}
export type ModuleAlias = {
  name: string
  alias: Module // If this is an alias, points to the target module
}
export type ModuleInfo = Module | ModuleAlias

export function followAlias(module: ModuleInfo): Module {
  let currentModule = module
  const visitedModules = new Set<ModuleInfo>()

  while ('alias' in currentModule) {
    if (visitedModules.has(currentModule)) {
      throw new CompilerError(
        `Cyclic module alias detected at module ${currentModule.name}`,
        0,
        0
      )
    }
    visitedModules.add(currentModule)
    currentModule = currentModule.alias
  }

  return currentModule
}

export function mergeModule(
  src: Module,
  dest: Module,
  line: number,
  column: number
) {
  const srcResolved = followAlias(src)
  const destResolved = followAlias(dest)

  if (srcResolved === destResolved) {
    return
  }

  // Merge functions
  for (const [name, func] of srcResolved.functions) {
    if (destResolved.functions.has(name)) {
      throw new CompilerError(
        `Duplicate function ${name} in module ${destResolved.name}`,
        line,
        column
      )
    }
    destResolved.functions.set(name, func)
  }

  // Merge variables
  for (const [name, varInfo] of srcResolved.variables) {
    if (destResolved.variables.has(name)) {
      throw new CompilerError(
        `Duplicate variable ${name} in module ${destResolved.name}`,
        line,
        column
      )
    }
    destResolved.variables.set(name, varInfo)
  }

  // Merge externs
  for (const [name, ext] of srcResolved.externs) {
    if (destResolved.externs.has(name)) {
      throw new CompilerError(
        `Duplicate extern ${name} in module ${destResolved.name}`,
        line,
        column
      )
    }
    destResolved.externs.set(name, ext)
  }

  // Merge child modules recursively
  for (const [name, child] of srcResolved.children) {
    if (destResolved.children.has(name)) {
      if ('alias' in child || 'alias' in destResolved.children.get(name)!) {
        throw new CompilerError(
          `Cannot merge module aliases for child module ${name} in module ${destResolved.name}`,
          line,
          column
        )
      }
      const dest = destResolved.children.get(name)
      if (!dest || 'alias' in dest) {
        throw new CompilerError(
          `Cannot merge module aliases for child module ${name} in module ${destResolved.name}`,
          line,
          column
        )
      }
      // If child module exists, merge recursively
      mergeModule(child, dest, line, column)
    } else {
      destResolved.children.set(name, child)
    }
  }
}
