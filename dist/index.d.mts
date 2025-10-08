import { FunctionDeclaration, Program, Statement, Expression, VariableDeclaration, DecoratorStatement } from '@scratch-fuse/core';

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
declare class Scope {
    readonly variables: Map<string, Variable>;
    private args?;
    private static listCmds;
    private static listReps;
    private static varCmds;
    private static varReps;
    constructor(variables: Map<string, Variable>, args?: Map<string, ProcedureArgument> | undefined);
    typeof(name: string): 'list' | 'scalar' | 'arg_any' | 'arg_bool' | null;
    get(name: string): Reporter | null;
    set(name: string, value: Reporter | string): Block[];
    add(name: string, value: Reporter | string): Block[];
    stmtMethod(name: string, func: string, args: (Reporter | string)[]): Block[] | null;
    exprMethod(name: string, func: string, args: (Reporter | string)[]): TypedValue | null;
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
    constructor(globalVars: Map<string, Variable>, decl: FunctionDeclaration, exportName: string | null);
    static escape(str: string): string;
    static getProccode(decl: FunctionDeclaration, exportName: string | null): string;
    get proccode(): string;
    private static parseTemplateName;
}
type TypedValue = {
    type: 'any';
    value: Reporter | string;
} | {
    type: 'bool';
    value: Reporter;
};
type Namespace = Map<string /** name */, NamespaceEntry>;
interface NamespaceEntry {
    opcode: string;
    type: 'void' | 'any' | 'bool' | 'hat';
    inputs?: Record<string, BooleanInput | AnyInput | SubstackInput>;
    fields?: Record<string, string>;
    args: (NamespaceEntryArgumentAny | NamespaceEntryArgumentBoolean | NamespaceEntryArgumentSubstack | NamespaceEntryArgumentField)[];
}
interface NamespaceEntryArgumentBase {
    name: string;
}
interface NamespaceEntryArgumentAny extends NamespaceEntryArgumentBase {
    type: 'any';
}
interface NamespaceEntryArgumentBoolean extends NamespaceEntryArgumentBase {
    type: 'bool';
}
interface NamespaceEntryArgumentSubstack extends NamespaceEntryArgumentBase {
    type: 'substack';
}
interface NamespaceEntryArgumentField extends NamespaceEntryArgumentBase {
    type: 'field';
    menu: Record<string, string> | null;
}
declare class Compiler {
    globalScope: Scope;
    funcs: Map<string, ScratchFunction>;
    namespaces: Map<string, Namespace>;
    constructor(globalScope: Scope, funcs: Map<string, ScratchFunction>, namespaces: Map<string, Namespace>);
    parse(stmt: ScratchFunction): CompiledFunction;
    parse(stmt: Program): Script[];
    parse(stmt: Statement, functionReturnType?: 'bool' | 'any' | 'void'): Block[];
    parseExpr(stmt: Expression): TypedValue;
    private parseLiteralExpression;
    private parseIdentifierExpression;
    private parseBinaryExpression;
    private parseUnaryExpression;
    private parseCallExpressionAsReporter;
    private parseMemberExpression;
    private callMethodOnValue;
    private parseNamespaceCallAsReporter;
    private parseFunctionCallAsReporter;
    private ensureBooleanType;
    private parseComputedAccess;
    private parseScratchFunction;
    private parseProgram;
    private parseStatement;
    private parseHatCall;
    private parseExpressionStatement;
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
    private parseNamespaceCallAsCommand;
    private parseFunctionCallAsCommand;
    static getFunctions(globalScope: Scope, program: Program): Map<string, ScratchFunction>;
}
interface ProgramInfo {
    namespaces: Map<string, Namespace>;
    variables: Map<string, [
        Variable,
        string | boolean | number | (string | boolean | number)[]
    ]>;
}
declare function getProgramInfo(program: Program): ProgramInfo;
declare function mergeNamespace(base: Namespace, additional: Namespace): Namespace;

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

interface CompiledFunctionWithDefault extends CompiledFunction {
    defaultValues: string[];
}
declare class DecompilerError extends Error {
    constructor(message: string);
}
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
 * Decompiler context for tracking variable names and namespaces
 */
interface DecompilerContext {
    namespaces: Map<string, Namespace>;
    generatedNamespaces: Map<string, Namespace>;
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
    get namespaces(): Map<string, Namespace>;
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
     * Common logic to match a block/reporter to a namespace entry from provided namespaces
     */
    private tryMatchNamespaceFromMap;
    /**
     * Generate a dynamic namespace call from opcode and save to generatedNamespaces
     */
    private generateDynamicNamespaceCall;
    /**
     * Try to match a block/reporter to a namespace entry (for reporters)
     */
    private tryMatchNamespace;
    /**
     * Try to match block inputs/fields to namespace entry args
     */
    private tryMatchEntry;
    /**
     * Decompile a block to Statement
     */
    decompileBlock(block: Block): Statement;
    /**
     * Try to match a command block to namespace (for command blocks)
     */
    private tryMatchNamespaceBlock;
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
     * Try to match a hat block to a namespace entry
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
 * @param globalVars The global variables
 * @param namespaces The namespace definitions
 * @param compiledFunctions The compiled functions
 * @param sharedNameManager Optional shared name manager for global variables (for multiple sprites)
 */
declare function createDecompiler(globalVars: Variable[], namespaces: Map<string, Namespace>, compiledFunctions: CompiledFunctionWithDefault[], sharedNameManager?: VariableNameManager): Decompiler;

export { type AnyInput, type BaseInput, type Block, type BlockBase, type BlockMutation, type BooleanInput, type CompiledFunction, type CompiledFunctionWithDefault, Compiler, CompilerError, Decompiler, type DecompilerContext, DecompilerError, type HatBlock, type ListCommand, type ListReporter, type Namespace, type NamespaceEntry, type NamespaceEntryArgumentAny, type NamespaceEntryArgumentBase, type NamespaceEntryArgumentBoolean, type NamespaceEntryArgumentField, type NamespaceEntryArgumentSubstack, type ProcedureArgument, type ProgramInfo, type Reporter, Scope, ScratchFunction, type Script, type SubstackInput, type TypedValue, type VarCommand, type VarReporter, type Variable, VariableNameManager, createDecompiler, getProgramInfo, mergeNamespace };
