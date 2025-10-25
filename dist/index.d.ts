import { Program, FunctionDeclaration, Statement, Expression, VariableDeclaration, DecoratorStatement } from '@scratch-fuse/core';
import { ErrorList } from '@scratch-fuse/utility';

interface BlockBase {
    opcode: string;
    inputs: {
        [key: string]: BooleanInput | AnyInput | SubstackInput;
    };
    fields: {
        [key: string]: string;
    };
    mutation?: BlockMutation;
}
interface BaseInput {
    type: 'bool' | 'any' | 'substack';
}
interface BooleanInput extends BaseInput {
    type: 'bool';
    value: Reporter;
}
interface AnyInput extends BaseInput {
    type: 'any';
    value: string | Reporter;
}
interface SubstackInput extends BaseInput {
    type: 'substack';
    value: Block[];
}
type BlockMutation = Record<string, string | BlockMutation[]>;
interface Block extends BlockBase {
}
interface Reporter extends BlockBase {
}
interface Script {
    hat?: HatBlock;
    blocks: Block[];
}
interface HatBlock extends BlockBase {
}
interface Variable {
    name: string;
    exportName: string | null;
    type: 'scalar' | 'list';
    isGlobal: boolean;
}

interface ResolvedResult {
    program: Program;
    filename: string;
}
/**
 * ImportResolver is responsible for resolving import paths to Programs
 * This allows the compiler to handle import statements by loading and parsing external modules
 */
interface ImportResolver {
    /**
     * Resolve an import path to a Program
     * @param path The import path (from ImportStatement.path.value)
     * @param currentFile Optional context about the file doing the importing
     * @returns The resolved Program, or throws an error if the import cannot be resolved
     */
    resolve(path: string, currentFile?: string): Promise<ResolvedResult> | ResolvedResult;
}
interface ContextOptions {
    importResolver?: ImportResolver;
}
interface ProcedureArgument {
    name: string;
    type: 'any' | 'bool';
}
type ListCommand = (v: Variable, args: (Reporter | string)[]) => Block[];
type ListReporter = (v: Variable, args: (Reporter | string)[]) => TypedValue;
type VarCommand = (v: Variable, args: (Reporter | string)[]) => Block[];
type VarReporter = (v: Reporter | string, args: (Reporter | string)[]) => TypedValue;
declare class CompilerError extends Error {
    line: number;
    column: number;
    constructor(message: string, line: number, column: number);
}
interface CompiledResult {
    scripts: Script[];
    functions: CompiledFunction[];
    variables: [
        Variable,
        string | boolean | number | (string | boolean | number)[]
    ][];
    externs: External[];
    errors?: ErrorList;
}
/**
 * Call a method on a Variable or Reporter value
 * @param target Variable object or Reporter value
 * @param method Method name to call
 * @param args Arguments to pass to the method
 * @returns Block array for commands, or TypedValue for reporters
 */
declare function callMethod(target: Variable | Reporter | string, method: string, args: (Reporter | string)[]): Block[] | TypedValue;
declare class Scope {
    private args?;
    constructor(args?: Map<string, ProcedureArgument> | undefined);
    getArg(name: string): Reporter | null;
    getArgType(name: string): 'arg_any' | 'arg_bool' | null;
}
interface CompiledFunction {
    decl: FunctionDeclaration;
    proccode: string;
    impl: Block[];
}
declare class ScratchFunction {
    decl: FunctionDeclaration;
    private exportName;
    scope: Scope;
    module: Module;
    constructor(decl: FunctionDeclaration, exportName: string | null, module: Module);
    static escape(str: string): string;
    static getProccode(decl: FunctionDeclaration, exportName: string | null, modulePath?: string[]): string;
    get proccode(): string;
    static parseTemplateName(template: string, decl: FunctionDeclaration): string;
}
type TypedValue = {
    type: 'any';
    value: Reporter | string;
} | {
    type: 'bool';
    value: Reporter;
};
type ResolvedValue = ScratchFunction | External | (TypedValue & {
    computed?: {
        target: Variable;
        index: TypedValue;
    };
}) | Variable | {
    target: Variable | Reporter | string;
    method: (args: (Reporter | string)[]) => Block[] | TypedValue;
} | Block[] | Module;
interface External {
    opcode: string;
    type: 'void' | 'any' | 'bool' | 'hat';
    inputs?: Record<string, BooleanInput | AnyInput | SubstackInput>;
    fields?: Record<string, string>;
    args: (ExternalArgumentAny | ExternalArgumentBoolean | ExternalArgumentSubstack | ExternalArgumentField)[];
}
interface ExternalArgumentBase {
    name: string;
}
interface ExternalArgumentAny extends ExternalArgumentBase {
    type: 'any';
}
interface ExternalArgumentBoolean extends ExternalArgumentBase {
    type: 'bool';
}
interface ExternalArgumentSubstack extends ExternalArgumentBase {
    type: 'substack';
}
interface ExternalArgumentField extends ExternalArgumentBase {
    type: 'field';
    menu: Record<string, string> | null;
}
declare class Context {
    options?: ContextOptions | undefined;
    currentModule: Module;
    private importedModules;
    private currentScope;
    constructor(currentModule: Module, options?: ContextOptions | undefined, importedModules?: Map<string, Module>);
    /**
     * Get variable type by name from current module or current scope
     */
    private getVariableType;
    /**
     * Get variable Reporter by name from current module or current scope
     */
    private getVariableReporter;
    /**
     * Get Variable object by name from current module
     */
    private getVariable;
    compile(stmt: Program): Promise<CompiledResult> | CompiledResult;
    compile(stmt: Statement, functionReturnType?: 'bool' | 'any' | 'void'): Block[];
    /**
     * Process import statements in a program
     * Compiles each import in isolation and returns their modules and results
     */
    private processImports;
    /**
     * Merge compiled results from imports into the main result
     * Checks for variable name conflicts
     */
    private mergeImportResults;
    /**
     * Call a ScratchFunction with arguments and return the result as a reporter
     */
    private callFunction;
    /**
     * Call an External with arguments and return the result as a reporter
     */
    private callExternal;
    /**
     * Internal implementation for expression parsing
     * Handles recursive lookup with optional current object for member expressions
     */
    private parseExprImpl;
    parseExpr(stmt: Expression): TypedValue;
    private parseLiteralExpression;
    private parseBinaryExpression;
    private parseUnaryExpression;
    private callMethodOnValue;
    private ensureBooleanType;
    private parseComputedAccess;
    private parseScratchFunction;
    private parseProgram;
    /**
     * Step 1: Extract variables, externs, and modules from statements
     * Recursively process child modules
     *
     * Two-phase approach for alias resolution:
     * Phase 1: Collect all module declarations, store alias paths without resolving
     * Phase 2: Resolve all aliases, following chains and detecting cycles (done at top level)
     */
    private extractDeclarations;
    /**
     * Resolve a module alias path to the actual module, following alias chains
     * and detecting circular references
     */
    private resolveModuleAlias;
    /**
     * Process a variable declaration
     */
    private processVariableDeclaration;
    /**
     * Step 2: Compile all functions and hat blocks in a module
     * Recursively compile functions and hat blocks in child modules
     */
    private compileModule;
    private parseStatement;
    private parseHatCall;
    private parseExpressionStatement;
    /**
     * Call a ScratchFunction as a command
     */
    private callFunctionAsCommand;
    /**
     * Call an External as a command
     */
    private callExternalAsCommand;
    private operateVar;
    private operateComputed;
    private parseIncrementStatement;
    private parseAssignmentStatement;
    private parseIfStatement;
    private parseWhileStatement;
    private getBooleanLiteral;
    private parseForStatement;
    private parseLoopStatement;
    private parseReturnStatement;
    private parseBlockStatement;
}
type Module = {
    name: string;
    parent: Module | null;
    functions: Map<string, ScratchFunction>;
    variables: Map<string, [
        Variable,
        string | boolean | number | (string | boolean | number)[]
    ]>;
    externs: Map<string, External>;
    children: Map<string, ModuleInfo>;
    filename?: string;
};
type ModuleAlias = {
    name: string;
    alias: Module;
};
type ModuleInfo = Module | ModuleAlias;
declare function followAlias(module: ModuleInfo): Module;
declare function mergeModule(src: Module, dest: Module, line: number, column: number): void;

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
 * - Matches module extern entries for unknown blocks
 * - Auto-generates and stores extern entries for unmatched blocks
 * - All generated tokens have line=0, column=0
 *
 * Usage:
 * ```typescript
 * import { createDecompiler, Module } from '@scratch-fuse/compiler'
 *
 * // Setup global module with variables and externs
 * const globalModule: Module = {
 *   name: 'global',
 *   parent: null,
 *   functions: new Map(),
 *   variables: new Map([[varName, [variable, initialValue]]]),
 *   externs: new Map([[externName, externEntry]]),
 *   children: new Map()
 * }
 * const compiledFunctions = [...] // Array of CompiledFunction
 *
 * // Create decompiler - variable names are pre-processed here
 * const decompiler = createDecompiler(globalModule, compiledFunctions)
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
 * // Access auto-generated module (for blocks not in the original modules)
 * const generatedModule = decompiler.generatedModule
 * ```
 */

interface CompiledFunctionWithDefault extends CompiledFunction {
    defaultValues: string[];
}
declare class DecompilerError extends Error {
    constructor(message: string);
}
type DecompilerModuleContext = {
    name: string;
    externs: Map<string, External>;
    children: Map<string, ModuleInfo>;
};
/**
 * Variable name manager for coordinating global and local variable names
 * - Global variables use a shared naming pool with 'global' prefix for invalid identifiers
 * - Local variables use a sprite-specific pool with 'local' prefix for invalid identifiers
 * - Valid identifiers can be used as-is if no conflict exists
 * - Manages variable to name mappings to ensure consistency across contexts
 * - Uses separate Maps for global and local variables to enable name reuse across sprites
 */
declare class VariableNameManager {
    private globalVariableNames;
    private localVariableNames;
    private functionNames;
    private globalCounter;
    private localCounter;
    private funcCounter;
    /**
     * Get all used names in the specified scope
     * @param scope - 'global' | 'local' | 'func'
     */
    private getUsedNames;
    /**
     * Check if a name is already used in the appropriate scope
     */
    isUsed(name: string, scope: 'global' | 'local' | 'func'): boolean;
    /**
     * Generate a unique name
     * @param preferredName - The preferred name
     * @param type - 'global' | 'local' | 'func'
     */
    private generateName;
    /**
     * Assign a name to a variable and store the mapping
     * Returns the assigned name
     */
    assignVariableName(variable: Variable, preferredName: string, isGlobal: boolean): string;
    /**
     * Assign a name to a function and store the mapping
     * Returns the assigned name
     */
    assignFunctionName(proccode: string, preferredName: string): string;
    /**
     * Get the assigned name for a function
     * Returns undefined if the function has not been assigned a name
     */
    getFunctionName(proccode: string): string | undefined;
    /**
     * Get the assigned name for a variable
     * Returns undefined if the variable has not been assigned a name
     */
    getVariableName(variable: Variable): string | undefined;
    /**
     * Find a variable by its exportName or name
     * Returns the variable and its generated name, or null if not found
     * @param searchName - The name to search for (exportName or name)
     * @param expectedType - Optional type filter ('scalar' or 'list')
     */
    findVariable(searchName: string, expectedType?: 'scalar' | 'list'): {
        variable: Variable;
        generatedName: string;
    } | null;
    /**
     * Get all variables (global and local) managed by this name manager
     */
    getAllVariables(): Variable[];
    /**
     * Create a new local scope manager that shares the global scope
     * This allows different sprites to reuse local variable names
     * - global counter and globalVariableNames are shared
     * - local counter, func counter, localVariableNames, and functionNames are not shared
     */
    createLocalScope(): VariableNameManager;
}
/**
 * Decompiler context for tracking variable names and modules
 */
interface DecompilerContext {
    context: DecompilerModuleContext;
    generatedModule: Module;
    nameManager: VariableNameManager;
    functionRegistry: Map<string, CompiledFunctionWithDefault>;
    currentFunctionParams: Map<string, string>;
}
/**
 * Decompiler class
 */
declare class Decompiler {
    private context;
    constructor(context: DecompilerContext);
    get generatedModule(): Module;
    /**
     * Decompile a variable to VariableDeclaration
     * Also handles @export decorator if exportName differs from name
     */
    decompileVariable(variable: Variable, value: string | number | boolean | (string | number | boolean)[]): VariableDeclaration | DecoratorStatement;
    /**
     * Decompile a reporter (value block) to Expression
     * @param reporter - The reporter or string literal to decompile
     * @param expectedType - The expected type based on usage context ('bool' for boolean contexts, 'any' otherwise)
     */
    decompileReporter(reporter: Reporter | string, expectedType?: 'bool' | 'any'): Expression;
    /**
     * Resolve a variable by its export name or name
     * @param name - The variable name to resolve
     * @param expectedType - The expected type of the variable ('scalar' or 'list')
     */
    private resolveVariable;
    /**
     * Common logic to match a block/reporter to an extern entry from a module
     */
    private tryMatchExternFromModule;
    /**
     * Generate a dynamic extern call from opcode and save to generatedModule
     */
    private generateDynamicExternCall;
    /**
     * Try to match a block/reporter to an extern entry (for reporters)
     */
    private tryMatchExtern;
    /**
     * Try to match block inputs/fields to extern entry args
     */
    private tryMatchEntry;
    /**
     * Decompile a block to Statement
     */
    decompileBlock(block: Block): Statement;
    /**
     * Try to match a command block to extern (for command blocks)
     */
    private tryMatchExternBlock;
    /**
     * Match entry with substack support
     */
    private tryMatchEntryWithSubstack;
    /**
     * Try to detect for loop pattern in consecutive statements
     * Pattern: init statement + while loop with increment at end
     */
    private tryConvertToForLoop;
    /**
     * Try to match a hat block to an extern entry
     */
    private tryMatchHatBlock;
    /**
     * Decompile a script
     */
    decompileScript(script: Script): Statement[];
    /**
     * Decompile a procedure call (function call)
     */
    private decompileProcedureCall;
    /**
     * Infer return type from procedures_return blocks in the function body
     */
    private inferReturnType;
    /**
     * Check if a reporter is a boolean expression
     */
    private isBooleanExpression;
    /**
     * Decompile function from proccode and blocks
     */
    decompileFunction(raw: CompiledFunction): FunctionDeclaration | DecoratorStatement;
}
/**
 * Helper to create a decompiler with proper context
 * @param globalModule The global module containing variables and externs
 * @param compiledFunctions The compiled functions
 * @param sharedNameManager Optional shared name manager for global variables (for multiple sprites)
 */
declare function createDecompiler(moduleContext: DecompilerModuleContext, globalVars: Variable[], compiledFunctions: CompiledFunctionWithDefault[], sharedNameManager?: VariableNameManager): Decompiler;

export { type AnyInput, type BaseInput, type Block, type BlockBase, type BlockMutation, type BooleanInput, type CompiledFunction, type CompiledFunctionWithDefault, type CompiledResult, CompilerError, Context, type ContextOptions, Decompiler, type DecompilerContext, DecompilerError, type DecompilerModuleContext, type External, type ExternalArgumentAny, type ExternalArgumentBase, type ExternalArgumentBoolean, type ExternalArgumentField, type ExternalArgumentSubstack, type HatBlock, type ImportResolver, type ListCommand, type ListReporter, type Module, type ModuleAlias, type ModuleInfo, type ProcedureArgument, type Reporter, type ResolvedResult, type ResolvedValue, Scope, ScratchFunction, type Script, type SubstackInput, type TypedValue, type VarCommand, type VarReporter, type Variable, VariableNameManager, callMethod, createDecompiler, followAlias, mergeModule };
