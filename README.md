
# @scratch-fuse/compiler

A simple compiler to compile FUSE language to intermediate representation (IR). You will need it for many applications (LSP, FUSE to Scratch, etc.).

## Overview

This repository contains the core of a compiler/tooling library written in TypeScript. It includes built-in modules under `src/builtins/` and the compiler implementation under `src/compiler/`. The project uses `tsup` for bundling and build tasks and is intended to be used as a Node.js library or integrated into a larger toolchain.

## Key Features

- Written in TypeScript with type definitions
- Built-in modules (see `src/builtins/`)
- Simple build configuration using `tsup` (`tsup.config.ts`)

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

