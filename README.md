# @scratch-fuse/compiler

A simple compiler to compile FUSE language to intermediate representation (IR). You will need it for many applications (LSP, FUSE to Scratch, etc.).

## Overview

This repository contains the core of a compiler/tooling library written in TypeScript. It includes built-in modules under `src/builtins/` and the compiler implementation under `src/compiler/`. The project uses `tsup` for bundling and build tasks and is intended to be used as a Node.js library or integrated into a larger toolchain.

## Key Features

- Written in TypeScript with type definitions
- Built-in modules (see `src/builtins/`)
- Simple build configuration using `tsup` (`tsup.config.ts`)
- **Decompiler** - Convert IR back to AST for analysis and transformation

## Decompiler

The decompiler allows you to convert compiled IR (Intermediate Representation) back into FUSE AST nodes. This is useful for:

- Code analysis and transformation
- Round-trip compilation testing
- IR optimization and refactoring
- Creating code generation tools

### Key Features

1. **Smart Variable Naming**: Automatically handles variable name conflicts and generates valid identifiers
   - Prefers original variable names when valid
   - Handles conflicts between global and local variables
   - Generates `var1`, `var2`, etc. for invalid or conflicting names

2. **Function Format Detection**: Automatically detects compiler-generated vs export formats
   - Parses `functionName(param1 = %s, param2 = %b)` format
   - Handles parameter name conflicts
   - Generates `arg1`, `arg2`, etc. for invalid parameter names

3. **Operator Reconstruction**: Reconstructs complex operators from IR
   - Detects `!=`, `<=`, `>=` from `operator_not` patterns
   - Preserves operator precedence
   - Handles nested expressions

4. **Namespace Matching**: Attempts to match unknown blocks to namespace entries
   - Matches opcode, fields, and inputs
   - Generates namespace member calls (`namespace.member()`)
   - Falls back gracefully for unmatched blocks

### Usage Example

```typescript
import {
  createDecompiler,
  Scope,
  Namespace,
  Variable
} from '@scratch-fuse/compiler'

// Setup variables
const globalVariables = new Map<string, Variable>()
globalVariables.set('score', {
  name: 'score',
  exportName: null,
  type: 'scalar',
  isGlobal: true
})

// Create scope and decompiler
const globalScope = new Scope(globalVariables)
const namespaces = new Map<string, Namespace>()
const decompiler = createDecompiler(globalScope, namespaces)

// Decompile a variable
const variable: Variable = {
  name: 'myVar',
  exportName: null,
  type: 'scalar',
  isGlobal: false
}
const varDecl = decompiler.decompileVariable(variable, 0)
// Result: { type: 'VariableDeclaration', name: 'myVar', ... }

// Decompile blocks
const block: Block = {
  opcode: 'data_setvariableto',
  fields: { VARIABLE: 'score' },
  inputs: { VALUE: { type: 'any', value: '10' } }
}
const stmt = decompiler.decompileBlock(block)
// Result: { type: 'AssignmentStatement', left: ..., operator: '=', right: ... }

// Decompile a function
const proccode = 'moveBy(x = %s, y = %s)'
const impl: Block[] = [
  /* function blocks */
]
const funcDecl = decompiler.decompileFunction(proccode, impl)
// Result: { type: 'FunctionDeclaration', name: { name: 'moveBy' }, ... }
```

### Notes

- All generated tokens have `line: 0` and `column: 0`
- The `exportName` field is set to `null` when it matches the generated name
- For variables, global variables have priority in name resolution
- For functions, parameter names are resolved to avoid conflicts with function-scoped variables

## Quick Start

1. Clone the repository and open the project folder.

2. Install dependencies:

```bash
npm install
```

3. Build the project (if a build script is present):

```bash
npm run build
```

Note: The repository includes a `tsup.config.ts` file. If you use a custom build flow or tooling, consult that file for configuration details.

## Project Structure

- `src/` — TypeScript source files
  - `builtins/` — built-in features and sb3-related implementation
  - `compiler/` — compiler core implementation
  - `index.ts`, `global.d.ts` — package entry point and global type declarations
- `tsup.config.ts` — bundler/build configuration
- `package.json` — npm configuration and scripts
- `LICENSE` — license file

See the files under `src/` for more implementation details.

## Contributing

Contributions are welcome via issues and pull requests. Please ensure code formatting and tests (if any) pass before opening a PR and describe the purpose of changes in the PR description.

## License

MPL-2.0. See the `LICENSE` file for details.
