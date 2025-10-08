"use strict";
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

// src/index.ts
var index_exports = {};
__export(index_exports, {
  Compiler: () => Compiler,
  CompilerError: () => CompilerError,
  Decompiler: () => Decompiler,
  DecompilerError: () => DecompilerError,
  Scope: () => Scope,
  ScratchFunction: () => ScratchFunction,
  VariableNameManager: () => VariableNameManager,
  createDecompiler: () => createDecompiler,
  getProgramInfo: () => getProgramInfo,
  mergeNamespace: () => mergeNamespace
});
module.exports = __toCommonJS(index_exports);

// src/compiler.ts
var import_utility = require("@scratch-fuse/utility");
var CompilerError = class extends Error {
  constructor(message, line, column) {
    super(`${message} at ${line}:${column}`);
    this.line = line;
    this.column = column;
    this.name = "CompilerError";
  }
};
var Scope = class _Scope {
  constructor(variables, args) {
    this.variables = variables;
    this.args = args;
  }
  static listCmds = /* @__PURE__ */ new Map([
    [
      "push",
      (v, rhs) => {
        if (rhs.length !== 1)
          throw new Error("push expects exactly one argument");
        return [
          {
            opcode: "data_addtolist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              ITEM: { type: "any", value: rhs[0] }
            }
          }
        ];
      }
    ],
    [
      "pop",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("pop expects no arguments");
        return [
          {
            opcode: "data_deleteoflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: "any", value: "last" }
            }
          }
        ];
      }
    ],
    [
      "insert",
      (v, rhs) => {
        if (rhs.length !== 2)
          throw new Error("insert expects exactly two arguments");
        return [
          {
            opcode: "data_insertatlist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: "any", value: rhs[0] },
              ITEM: { type: "any", value: rhs[1] }
            }
          }
        ];
      }
    ],
    [
      "remove",
      (v, rhs) => {
        if (rhs.length !== 1)
          throw new Error("remove expects exactly one argument");
        return [
          {
            opcode: "data_deleteoflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: "any", value: rhs[0] }
            }
          }
        ];
      }
    ],
    [
      "replace",
      (v, rhs) => {
        if (rhs.length !== 2)
          throw new Error("replace expects exactly two arguments");
        return [
          {
            opcode: "data_replaceitemoflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: "any", value: rhs[0] },
              ITEM: { type: "any", value: rhs[1] }
            }
          }
        ];
      }
    ],
    [
      "show",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("show expects no arguments");
        return [
          {
            opcode: "data_showlist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        ];
      }
    ],
    [
      "hide",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("hide expects no arguments");
        return [
          {
            opcode: "data_hidelist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        ];
      }
    ],
    [
      "clear",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("clear expects no arguments");
        return [
          {
            opcode: "data_deletealloflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        ];
      }
    ]
  ]);
  static listReps = /* @__PURE__ */ new Map([
    [
      "includes",
      (v, rhs) => {
        if (rhs.length !== 1)
          throw new Error("includes expects exactly one argument");
        return {
          type: "bool",
          value: {
            opcode: "data_listcontainsitem",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              ITEM: { type: "any", value: rhs[0] }
            }
          }
        };
      }
    ],
    [
      "at",
      (v, rhs) => {
        if (rhs.length !== 1) throw new Error("at expects exactly one argument");
        return {
          type: "any",
          value: {
            opcode: "data_itemoflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: "any", value: rhs[0] }
            }
          }
        };
      }
    ],
    [
      "indexOf",
      (v, rhs) => {
        if (rhs.length !== 1)
          throw new Error("indexOf expects exactly one argument");
        return {
          type: "any",
          value: {
            opcode: "data_itemnumoflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              ITEM: { type: "any", value: rhs[0] }
            }
          }
        };
      }
    ],
    [
      "length",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("length expects no arguments");
        return {
          type: "any",
          value: {
            opcode: "data_lengthoflist",
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        };
      }
    ]
  ]);
  static varCmds = /* @__PURE__ */ new Map([
    [
      "show",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("show expects no arguments");
        return [
          {
            opcode: "data_showvariable",
            fields: {
              VARIABLE: v.exportName ?? v.name
            },
            inputs: {}
          }
        ];
      }
    ],
    [
      "hide",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("hide expects no arguments");
        return [
          {
            opcode: "data_hidevariable",
            fields: {
              VARIABLE: v.exportName ?? v.name
            },
            inputs: {}
          }
        ];
      }
    ]
  ]);
  static varReps = /* @__PURE__ */ new Map([
    [
      "at",
      (v, rhs) => {
        if (rhs.length !== 1) throw new Error("at expects exactly one argument");
        return {
          type: "any",
          value: {
            opcode: "operator_letter_of",
            fields: {},
            inputs: {
              STRING: { type: "any", value: v },
              LETTER: { type: "any", value: rhs[0] }
            }
          }
        };
      }
    ],
    [
      "includes",
      (v, rhs) => {
        if (rhs.length !== 1)
          throw new Error("includes expects exactly one argument");
        return {
          type: "bool",
          value: {
            opcode: "operator_contains",
            fields: {},
            inputs: {
              STRING1: { type: "any", value: v },
              STRING2: { type: "any", value: rhs[0] }
            }
          }
        };
      }
    ],
    [
      "length",
      (v, rhs) => {
        if (rhs.length !== 0) throw new Error("length expects no arguments");
        return {
          type: "any",
          value: {
            opcode: "operator_length",
            fields: {},
            inputs: {
              STRING: { type: "any", value: v }
            }
          }
        };
      }
    ]
  ]);
  typeof(name) {
    const arg = this.args?.get(name);
    if (arg) return arg.type === "bool" ? "arg_bool" : "arg_any";
    const variable = this.variables.get(name);
    if (variable) return variable.type;
    return null;
  }
  get(name) {
    const arg = this.args?.get(name);
    if (arg) {
      switch (arg.type) {
        case "any":
          return {
            opcode: "argument_reporter_string_number",
            fields: {
              VALUE: arg.name
            },
            inputs: {}
          };
        case "bool":
          return {
            opcode: "argument_reporter_boolean",
            fields: {
              VALUE: arg.name
            },
            inputs: {}
          };
      }
    }
    const variable = this.variables.get(name);
    if (variable) {
      switch (variable.type) {
        case "scalar":
          return {
            opcode: "data_variable",
            fields: {
              VARIABLE: variable.exportName ?? variable.name
            },
            inputs: {}
          };
        case "list":
          return {
            opcode: "data_listcontents",
            fields: {
              LIST: variable.exportName ?? variable.name
            },
            inputs: {}
          };
      }
    }
    return null;
  }
  set(name, value) {
    const arg = this.args?.get(name);
    if (arg) throw new Error(`Cannot assign to argument ${name}`);
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === "scalar") {
        return [
          {
            opcode: "data_setvariableto",
            fields: {
              VARIABLE: variable.exportName ?? variable.name
            },
            inputs: {
              VALUE: { type: "any", value }
            }
          }
        ];
      } else throw new Error(`Cannot assign to list variable ${name}`);
    }
    throw new Error(`Variable ${name} not found`);
  }
  add(name, value) {
    const arg = this.args?.get(name);
    if (arg) throw new Error(`Cannot add to argument ${name}`);
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === "scalar") {
        return [
          {
            opcode: "data_changevariableby",
            fields: {
              VARIABLE: variable.exportName ?? variable.name
            },
            inputs: {
              VALUE: { type: "any", value }
            }
          }
        ];
      } else throw new Error(`Cannot add to list variable ${name}`);
    }
    throw new Error(`Variable ${name} not found`);
  }
  stmtMethod(name, func, args) {
    const arg = this.args?.get(name);
    if (arg) return null;
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === "list") {
        const listCmd = _Scope.listCmds.get(func);
        if (listCmd) return listCmd(variable, args);
        else return null;
      } else {
        const varCmd = _Scope.varCmds.get(func);
        if (varCmd) return varCmd(variable, args);
        else return null;
      }
    }
    throw new Error(`Variable ${name} not found`);
  }
  exprMethod(name, func, args) {
    const arg = this.args?.get(name);
    if (arg) {
      const varRep = _Scope.varReps.get(func);
      if (varRep)
        return varRep(
          arg.type === "bool" ? {
            opcode: "argument_reporter_boolean",
            fields: { VALUE: arg.name },
            inputs: {}
          } : {
            opcode: "argument_reporter_string_number",
            fields: { VALUE: arg.name },
            inputs: {}
          },
          args
        );
    }
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === "list") {
        const listRep = _Scope.listReps.get(func);
        if (listRep) return listRep(variable, args);
        else return null;
      } else {
        const varRep = _Scope.varReps.get(func);
        if (varRep)
          return varRep(
            {
              opcode: "data_variable",
              fields: {
                VARIABLE: variable.exportName ?? variable.name
              },
              inputs: {}
            },
            args
          );
        else return null;
      }
    }
    throw new Error(`Variable ${name} not found`);
  }
};
var ScratchFunction = class _ScratchFunction {
  constructor(globalVars, decl, exportName) {
    this.decl = decl;
    this.exportName = exportName;
    const args = /* @__PURE__ */ new Map();
    for (const arg of decl.parameters) {
      if (["bool", "any"].indexOf(arg.type.name) === -1) {
        throw new CompilerError(
          `Invalid argument type ${arg.type}, at function ${decl.name.name}`,
          decl.name.line,
          decl.name.column
        );
      }
      args.set(arg.name.name, {
        type: arg.type.name === "bool" ? "bool" : "any",
        name: arg.name.name
      });
    }
    this.scope = new Scope(globalVars, args);
    if (this.exportName) {
      _ScratchFunction.getProccode(decl, this.exportName);
    }
  }
  scope;
  static escape(str) {
    return str.replace(/%/g, "%%");
  }
  // TODO: 移动到其它位置，解耦合
  static getProccode(decl, exportName) {
    return exportName ? _ScratchFunction.parseTemplateName(exportName, decl) : `${decl.name.name}(${decl.parameters.map(
      (p) => `${_ScratchFunction.escape(p.name.name)} = %${p.type.name === "bool" ? "b" : "s"}`
    ).join(", ")})`;
  }
  get proccode() {
    return _ScratchFunction.getProccode(this.decl, this.exportName);
  }
  static parseTemplateName(template, decl) {
    const paramRegex = /(?<!\[)\[([^\]]+)\](?!\])/g;
    let result = "";
    let lastIndex = 0;
    let paramIndex = 0;
    let match;
    while ((match = paramRegex.exec(template)) !== null) {
      if (match.index > lastIndex) {
        const textBefore = template.slice(lastIndex, match.index);
        const unescapedText = textBefore.replace(/\[\[/g, "[").replace(/\]\]/g, "]");
        result += _ScratchFunction.escape(unescapedText);
      }
      const paramName = match[1];
      if (paramIndex >= decl.parameters.length) {
        throw new CompilerError(
          `Too many parameter placeholders in template`,
          decl.name.line,
          decl.name.column
        );
      }
      const param = decl.parameters[paramIndex];
      if (paramName !== param.name.name) {
        throw new CompilerError(
          `Parameter placeholder [${paramName}] does not match parameter ${param.name.name}`,
          decl.name.line,
          decl.name.column
        );
      }
      result += `%${param.type.name === "bool" ? "b" : "s"}`;
      paramIndex++;
      lastIndex = match.index + match[0].length;
    }
    if (lastIndex < template.length) {
      const textAfter = template.slice(lastIndex);
      const unescapedText = textAfter.replace(/\[\[/g, "[").replace(/\]\]/g, "]");
      result += _ScratchFunction.escape(unescapedText);
    }
    if (paramIndex < decl.parameters.length) {
      throw new CompilerError(
        `Missing parameter placeholders for: ${decl.parameters.slice(paramIndex).map((p) => p.name.name).join(", ")}`,
        decl.name.line,
        decl.name.column
      );
    }
    return result.trim();
  }
};
var Compiler = class {
  constructor(globalScope, funcs, namespaces) {
    this.globalScope = globalScope;
    this.funcs = funcs;
    this.namespaces = namespaces;
  }
  parse(stmt, functionReturnType) {
    if (stmt instanceof ScratchFunction) {
      return this.parseScratchFunction(stmt);
    } else if (stmt.type === "Program") {
      return this.parseProgram(stmt);
    } else {
      return this.parseStatement(stmt, functionReturnType ?? null);
    }
  }
  parseExpr(stmt) {
    switch (stmt.type) {
      case "Literal":
        return this.parseLiteralExpression(stmt);
      case "Identifier":
        return this.parseIdentifierExpression(stmt);
      case "BinaryExpression":
        return this.parseBinaryExpression(stmt);
      case "UnaryExpression":
        return this.parseUnaryExpression(stmt);
      case "CallExpression":
        return this.parseCallExpressionAsReporter(stmt);
      case "MemberExpression":
        return this.parseMemberExpression(stmt);
      case "ArrayExpression":
        throw new CompilerError(
          "ArrayExpression is only allowed in variable declarations",
          stmt.line,
          stmt.column
        );
      default:
        throw new CompilerError(
          `Unsupported expression type: ${stmt.type}`,
          stmt.line,
          stmt.column
        );
    }
  }
  // Helper methods for parseExpr
  parseLiteralExpression(expr) {
    const value = expr.value;
    if (typeof value === "boolean") {
      return this.getBooleanLiteral(value);
    } else {
      return {
        type: "any",
        value: String(value)
      };
    }
  }
  parseIdentifierExpression(expr) {
    const name = expr.name;
    let reporter;
    try {
      reporter = this.globalScope.get(name);
    } catch (error) {
      throw new CompilerError(error.message, expr.line, expr.column);
    }
    if (reporter) {
      const varType = this.globalScope.typeof(name);
      if (varType === "arg_bool") {
        return { type: "bool", value: reporter };
      } else {
        return { type: "any", value: reporter };
      }
    }
    throw new CompilerError(
      `Variable ${name} not found`,
      expr.line,
      expr.column
    );
  }
  parseBinaryExpression(expr) {
    const left = this.parseExpr(expr.left);
    const right = this.parseExpr(expr.right);
    switch (expr.operator) {
      case "+":
        return {
          type: "any",
          value: {
            opcode: "operator_add",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: left.value },
              NUM2: { type: "any", value: right.value }
            }
          }
        };
      case "-":
        return {
          type: "any",
          value: {
            opcode: "operator_subtract",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: left.value },
              NUM2: { type: "any", value: right.value }
            }
          }
        };
      case "*":
        return {
          type: "any",
          value: {
            opcode: "operator_multiply",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: left.value },
              NUM2: { type: "any", value: right.value }
            }
          }
        };
      case "/":
        return {
          type: "any",
          value: {
            opcode: "operator_divide",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: left.value },
              NUM2: { type: "any", value: right.value }
            }
          }
        };
      case "%":
        return {
          type: "any",
          value: {
            opcode: "operator_mod",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: left.value },
              NUM2: { type: "any", value: right.value }
            }
          }
        };
      case "..":
        return {
          type: "any",
          value: {
            opcode: "operator_join",
            fields: {},
            inputs: {
              STRING1: { type: "any", value: left.value },
              STRING2: { type: "any", value: right.value }
            }
          }
        };
      case "==":
        return {
          type: "bool",
          value: {
            opcode: "operator_equals",
            fields: {},
            inputs: {
              OPERAND1: { type: "any", value: left.value },
              OPERAND2: { type: "any", value: right.value }
            }
          }
        };
      case "!=":
        return {
          type: "bool",
          value: {
            opcode: "operator_not",
            fields: {},
            inputs: {
              OPERAND: {
                type: "bool",
                value: {
                  opcode: "operator_equals",
                  fields: {},
                  inputs: {
                    OPERAND1: { type: "any", value: left.value },
                    OPERAND2: { type: "any", value: right.value }
                  }
                }
              }
            }
          }
        };
      case "<":
        return {
          type: "bool",
          value: {
            opcode: "operator_lt",
            fields: {},
            inputs: {
              OPERAND1: { type: "any", value: left.value },
              OPERAND2: { type: "any", value: right.value }
            }
          }
        };
      case ">":
        return {
          type: "bool",
          value: {
            opcode: "operator_gt",
            fields: {},
            inputs: {
              OPERAND1: { type: "any", value: left.value },
              OPERAND2: { type: "any", value: right.value }
            }
          }
        };
      case "<=":
        return {
          type: "bool",
          value: {
            opcode: "operator_not",
            fields: {},
            inputs: {
              OPERAND: {
                type: "bool",
                value: {
                  opcode: "operator_gt",
                  fields: {},
                  inputs: {
                    OPERAND1: { type: "any", value: left.value },
                    OPERAND2: { type: "any", value: right.value }
                  }
                }
              }
            }
          }
        };
      case ">=":
        return {
          type: "bool",
          value: {
            opcode: "operator_not",
            fields: {},
            inputs: {
              OPERAND: {
                type: "bool",
                value: {
                  opcode: "operator_lt",
                  fields: {},
                  inputs: {
                    OPERAND1: { type: "any", value: left.value },
                    OPERAND2: { type: "any", value: right.value }
                  }
                }
              }
            }
          }
        };
      case "&&":
        this.ensureBooleanType(
          left,
          "Left operand of && must be boolean",
          expr.line,
          expr.column
        );
        this.ensureBooleanType(
          right,
          "Right operand of && must be boolean",
          expr.line,
          expr.column
        );
        return {
          type: "bool",
          value: {
            opcode: "operator_and",
            fields: {},
            inputs: {
              OPERAND1: { type: "bool", value: left.value },
              OPERAND2: { type: "bool", value: right.value }
            }
          }
        };
      case "||":
        this.ensureBooleanType(
          left,
          "Left operand of || must be boolean",
          expr.line,
          expr.column
        );
        this.ensureBooleanType(
          right,
          "Right operand of || must be boolean",
          expr.line,
          expr.column
        );
        return {
          type: "bool",
          value: {
            opcode: "operator_or",
            fields: {},
            inputs: {
              OPERAND1: { type: "bool", value: left.value },
              OPERAND2: { type: "bool", value: right.value }
            }
          }
        };
      default:
        throw new CompilerError(
          `Unsupported binary operator: ${expr.operator}`,
          expr.line,
          expr.column
        );
    }
  }
  parseUnaryExpression(expr) {
    const operand = this.parseExpr(expr.operand);
    switch (expr.operator) {
      case "!":
        this.ensureBooleanType(
          operand,
          "Operand of ! must be boolean",
          expr.line,
          expr.column
        );
        return {
          type: "bool",
          value: {
            opcode: "operator_not",
            fields: {},
            inputs: {
              OPERAND: { type: "bool", value: operand.value }
            }
          }
        };
      case "-":
        return {
          type: "any",
          value: {
            opcode: "operator_subtract",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: "0" },
              NUM2: { type: "any", value: operand.value }
            }
          }
        };
      case "+":
        return {
          type: "any",
          value: {
            opcode: "operator_add",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: "0" },
              NUM2: { type: "any", value: operand.value }
            }
          }
        };
      default:
        throw new CompilerError(
          `Unsupported unary operator: ${expr.operator}`,
          expr.line,
          expr.column
        );
    }
  }
  parseCallExpressionAsReporter(expr) {
    if (expr.then) {
      throw new CompilerError(
        "Call expressions with then blocks cannot be used as reporters",
        expr.line,
        expr.column
      );
    }
    if (expr.callee.type === "MemberExpression") {
      const memberExpr = expr.callee;
      if (memberExpr.object.type !== "Identifier") {
        const objectValue = this.parseExpr(memberExpr.object);
        if (memberExpr.computed) {
          const index = this.parseExpr(memberExpr.property);
          return this.callMethodOnValue(
            objectValue.value,
            "at",
            [index.value],
            expr.line,
            expr.column
          );
        }
        const propertyName = memberExpr.property.name;
        const args = expr.arguments.map((arg) => this.parseExpr(arg).value);
        return this.callMethodOnValue(
          objectValue.value,
          propertyName,
          args,
          expr.line,
          expr.column
        );
      }
      if (memberExpr.property.type === "Identifier") {
        const objectName = memberExpr.object.name;
        const propertyName = memberExpr.property.name;
        const varType = this.globalScope.typeof(objectName);
        if (varType) {
          const args = expr.arguments.map((arg) => this.parseExpr(arg).value);
          try {
            const methodResult = this.globalScope.exprMethod(
              objectName,
              propertyName,
              args
            );
            if (methodResult) {
              return methodResult;
            }
          } catch (error) {
            throw new CompilerError(
              error.message,
              expr.line,
              expr.column
            );
          }
        }
      }
      return this.parseNamespaceCallAsReporter(
        memberExpr,
        expr.arguments,
        expr.line,
        expr.column
      );
    } else if (expr.callee.type === "Identifier") {
      const funcName = expr.callee.name;
      return this.parseFunctionCallAsReporter(
        funcName,
        expr.arguments,
        expr.line,
        expr.column
      );
    } else {
      throw new CompilerError(
        "Unsupported call expression callee type",
        expr.line,
        expr.column
      );
    }
  }
  parseMemberExpression(expr) {
    if (expr.object.type === "Identifier") {
      const objectName = expr.object.name;
      if (expr.computed) {
        const index = this.parseExpr(expr.property);
        return this.parseComputedAccess(
          objectName,
          index,
          expr.line,
          expr.column
        );
      } else {
        const propertyName = expr.property.name;
        const varType = this.globalScope.typeof(objectName);
        if (varType) {
          try {
            const methodResult = this.globalScope.exprMethod(
              objectName,
              propertyName,
              []
            );
            if (methodResult) {
              return methodResult;
            }
          } catch (error) {
            throw new CompilerError(
              error.message,
              expr.line,
              expr.column
            );
          }
        }
        return this.parseNamespaceCallAsReporter(
          expr,
          [],
          expr.line,
          expr.column
        );
      }
    } else {
      const objectValue = this.parseExpr(expr.object);
      if (expr.computed) {
        const index = this.parseExpr(expr.property);
        return this.callMethodOnValue(
          objectValue.value,
          "at",
          [index.value],
          expr.line,
          expr.column
        );
      } else {
        const propertyName = expr.property.name;
        return this.callMethodOnValue(
          objectValue.value,
          propertyName,
          [],
          expr.line,
          expr.column
        );
      }
    }
  }
  // Helper method to call a method on a value (used for chained calls)
  callMethodOnValue(value, methodName, args, line, column) {
    const method = Scope["varReps"].get(methodName);
    if (!method) {
      throw new CompilerError(
        `Method ${methodName} not found for chained call`,
        line,
        column
      );
    }
    try {
      return method(value, args);
    } catch (error) {
      throw new CompilerError(error.message, line, column);
    }
  }
  parseNamespaceCallAsReporter(memberExpr, args, line, column) {
    if (memberExpr.object.type !== "Identifier" || memberExpr.property.type !== "Identifier") {
      throw new CompilerError(
        "Namespace calls must use simple identifiers",
        line,
        column
      );
    }
    const namespaceName = memberExpr.object.name;
    const functionName = memberExpr.property.name;
    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      throw new CompilerError(
        `Namespace ${namespaceName} not found`,
        line,
        column
      );
    }
    const entry = namespace.get(functionName);
    if (!entry) {
      throw new CompilerError(
        `Function ${functionName} not found in namespace ${namespaceName}`,
        line,
        column
      );
    }
    if (entry.type !== "any" && entry.type !== "bool") {
      throw new CompilerError(
        `${namespaceName}.${functionName} cannot be used as a reporter (type: ${entry.type})`,
        line,
        column
      );
    }
    const parsedArgs = args.map((arg) => this.parseExpr(arg));
    const inputs = Object.assign({}, entry.inputs);
    const fields = Object.assign({}, entry.fields);
    if (parsedArgs.length !== entry.args.length) {
      throw new CompilerError(
        `${namespaceName}.${functionName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`,
        line,
        column
      );
    }
    for (let i = 0; i < entry.args.length; i++) {
      const argDef = entry.args[i];
      const argValue = parsedArgs[i];
      if (argDef.type === "bool" && argValue.type !== "bool") {
        throw new CompilerError(
          `Argument ${argDef.name} must be boolean`,
          line,
          column
        );
      }
      if (argDef.type === "substack") {
        throw new CompilerError(
          `Argument ${argDef.name} cannot be a substack`,
          line,
          column
        );
      }
      if (argDef.type === "field") {
        const rawArgValue = args[i];
        if (rawArgValue.type !== "Literal") {
          throw new CompilerError(
            `Argument ${argDef.name} must be a literal for field`,
            line,
            column
          );
        }
        const literal = rawArgValue;
        if (typeof literal.value !== "string") {
          throw new CompilerError(
            `Argument ${argDef.name} must be a string literal`,
            line,
            column
          );
        }
        const fieldValue = String(literal.value);
        if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
          throw new CompilerError(
            `Argument ${argDef.name} has invalid value ${fieldValue}`,
            line,
            column
          );
        }
        fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue;
      } else {
        inputs[argDef.name] = argValue.type === "bool" ? { type: "bool", value: argValue.value } : { type: "any", value: argValue.value };
      }
    }
    return {
      type: entry.type,
      value: {
        opcode: entry.opcode,
        fields,
        inputs
      }
    };
  }
  parseFunctionCallAsReporter(funcName, args, line, column) {
    const func = this.funcs.get(funcName);
    if (!func) {
      throw new CompilerError(`Function ${funcName} not found`, line, column);
    }
    if (func.decl.returnType.name === "void") {
      throw new CompilerError(
        `Function ${funcName} returns void and cannot be used as a reporter`,
        line,
        column
      );
    }
    const parsedArgs = args.map((arg) => this.parseExpr(arg));
    if (parsedArgs.length !== func.decl.parameters.length) {
      throw new CompilerError(
        `Function ${funcName} expects ${func.decl.parameters.length} arguments, got ${parsedArgs.length}`,
        line,
        column
      );
    }
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType = func.decl.parameters[i].type.name === "bool" ? "bool" : "any";
      if (paramType === "bool" && parsedArgs[i].type !== "bool") {
        throw new CompilerError(
          `Parameter ${func.decl.parameters[i].name.name} must be boolean`,
          line,
          column
        );
      }
    }
    const inputs = {};
    for (let i = 0; i < parsedArgs.length; i++) {
      const paramName = func.decl.parameters[i].name.name;
      const paramType = func.decl.parameters[i].type.name === "bool" ? "bool" : "any";
      inputs[paramName] = paramType === "bool" ? { type: "bool", value: parsedArgs[i].value } : { type: "any", value: parsedArgs[i].value };
    }
    const names = func.decl.parameters.map((p) => p.name.name);
    return {
      type: func.decl.returnType.name === "bool" ? "bool" : "any",
      value: {
        opcode: "procedures_call",
        fields: {},
        inputs,
        mutation: {
          tagName: "mutation",
          proccode: func.proccode,
          children: [],
          return: "1",
          warp: func.decl.once ? "true" : "false",
          argumentids: JSON.stringify(names),
          // FIXME: use stable IDs
          argumentnames: JSON.stringify(names),
          argumentdefaults: JSON.stringify(
            func.decl.parameters.map(
              (p) => p.type.name === "any" ? "" : "false"
            )
          )
        }
      }
    };
  }
  // Type checking utility
  ensureBooleanType(value, message, line, column) {
    if (value.type !== "bool") {
      throw new CompilerError(message, line, column);
    }
  }
  // Handle computed access (test[index])
  parseComputedAccess(objectName, index, line, column) {
    const varType = this.globalScope.typeof(objectName);
    if (!varType) {
      throw new CompilerError(`Variable ${objectName} not found`, line, column);
    }
    if (varType === "list") {
      try {
        const methodResult = this.globalScope.exprMethod(objectName, "at", [
          index.value
        ]);
        if (methodResult) {
          return methodResult;
        }
        throw new CompilerError(
          `Method 'at' not found on list ${objectName}`,
          line,
          column
        );
      } catch (error) {
        if (error instanceof CompilerError) {
          throw error;
        }
        throw new CompilerError(error.message, line, column);
      }
    } else if (varType === "scalar" || varType === "arg_any") {
      try {
        const methodResult = this.globalScope.exprMethod(objectName, "at", [
          index.value
        ]);
        if (methodResult) {
          return methodResult;
        }
        throw new CompilerError(
          `Method 'at' not found on variable ${objectName}`,
          line,
          column
        );
      } catch (error) {
        if (error instanceof CompilerError) {
          throw error;
        }
        throw new CompilerError(error.message, line, column);
      }
    } else {
      throw new CompilerError(
        `Cannot use computed access on ${varType}`,
        line,
        column
      );
    }
  }
  // Parse ScratchFunction
  parseScratchFunction(func) {
    const oldScope = this.globalScope;
    this.globalScope = func.scope;
    try {
      const returnType = func.decl.returnType.name === "bool" ? "bool" : func.decl.returnType.name === "void" ? "void" : "any";
      const impl = this.parseBlockStatement(func.decl.body, returnType);
      return {
        decl: func.decl,
        proccode: func.proccode,
        impl
      };
    } finally {
      this.globalScope = oldScope;
    }
  }
  // Parse Program (top-level)
  parseProgram(program) {
    const scripts = [];
    const errors = [];
    for (const stmt of program.body) {
      try {
        switch (stmt.type) {
          case "ExpressionStatement":
            const exprStmt = stmt;
            if (exprStmt.expression.type === "CallExpression") {
              const callExpr = exprStmt.expression;
              if (callExpr.then) {
                const hatScript = this.parseHatCall(callExpr);
                if (hatScript) {
                  scripts.push(hatScript);
                  continue;
                }
              }
            }
            throw new CompilerError(
              "Top-level expressions must be hat block calls",
              stmt.line,
              stmt.column
            );
          case "NamespaceDeclaration":
          case "FunctionDeclaration":
          case "VariableDeclaration":
            break;
          case "DecoratorStatement": {
            const decorator = stmt;
            if (decorator.name.name !== "export") {
              throw new CompilerError(
                `Unknown decorator @${decorator.name.name}`,
                decorator.line,
                decorator.column
              );
            }
            if (!["FunctionDeclaration", "VariableDeclaration"].includes(
              decorator.target.type
            )) {
              throw new CompilerError(
                "@export can only be applied to functions or variables",
                decorator.line,
                decorator.column
              );
            }
            break;
          }
          default:
            throw new CompilerError(
              `Statement type ${stmt.type} is not allowed at top level`,
              stmt.line,
              stmt.column
            );
        }
      } catch (error) {
        errors.push(error);
      }
    }
    if (errors.length > 0) {
      throw new import_utility.ErrorList(errors);
    }
    return scripts;
  }
  // Parse Statement (in function/block context)
  parseStatement(stmt, functionReturnType) {
    switch (stmt.type) {
      case "NoopStatement":
        return [];
      case "ExpressionStatement":
        return this.parseExpressionStatement(
          stmt,
          functionReturnType
        );
      case "AssignmentStatement":
        return this.parseAssignmentStatement(stmt);
      case "IncrementStatement":
        return this.parseIncrementStatement(stmt);
      case "IfStatement":
        return this.parseIfStatement(stmt, functionReturnType);
      case "WhileStatement":
        return this.parseWhileStatement(
          stmt,
          functionReturnType
        );
      case "ForStatement":
        return this.parseForStatement(stmt, functionReturnType);
      case "LoopStatement":
        return this.parseLoopStatement(
          stmt,
          functionReturnType
        );
      case "ReturnStatement":
        return this.parseReturnStatement(
          stmt,
          functionReturnType
        );
      case "BlockStatement":
        return this.parseBlockStatement(
          stmt,
          functionReturnType
        );
      case "NamespaceDeclaration":
        throw new CompilerError(
          "Namespace declarations are not allowed in function bodies",
          stmt.line,
          stmt.column
        );
      case "VariableDeclaration":
        throw new CompilerError(
          "Variable declarations are not allowed in function bodies (use assignment instead)",
          stmt.line,
          stmt.column
        );
      case "FunctionDeclaration":
        throw new CompilerError(
          "Function declarations are not allowed in function bodies",
          stmt.line,
          stmt.column
        );
      case "DecoratorStatement":
        throw new CompilerError(
          "Decorators are not allowed in function bodies",
          stmt.line,
          stmt.column
        );
      default:
        throw new CompilerError(
          `Unsupported statement type: ${stmt.type}`,
          stmt.line,
          stmt.column
        );
    }
  }
  // Parse hat block calls
  parseHatCall(callExpr) {
    if (!callExpr.then) return null;
    if (callExpr.callee.type === "MemberExpression") {
      const memberExpr = callExpr.callee;
      if (memberExpr.object.type === "Identifier" && memberExpr.property.type === "Identifier") {
        const namespaceName = memberExpr.object.name;
        const functionName = memberExpr.property.name;
        const namespace = this.namespaces.get(namespaceName);
        if (!namespace) return null;
        const entry = namespace.get(functionName);
        if (!entry || entry.type !== "hat") return null;
        const parsedArgs = callExpr.arguments.map((arg) => this.parseExpr(arg));
        const inputs = Object.assign({}, entry.inputs);
        const fields = Object.assign(
          {},
          entry.fields
        );
        if (parsedArgs.length !== entry.args.length) {
          throw new CompilerError(
            `${namespaceName}.${functionName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`,
            callExpr.line,
            callExpr.column
          );
        }
        for (let i = 0; i < entry.args.length; i++) {
          const argDef = entry.args[i];
          const argValue = parsedArgs[i];
          if (argDef.type === "field") {
            const rawArgValue = callExpr.arguments[i];
            if (rawArgValue.type !== "Literal") {
              throw new CompilerError(
                `Argument ${argDef.name} must be a literal for field`,
                callExpr.line,
                callExpr.column
              );
            }
            const literal = rawArgValue;
            if (typeof literal.value !== "string") {
              throw new CompilerError(
                `Argument ${argDef.name} must be a string literal`,
                callExpr.line,
                callExpr.column
              );
            }
            const fieldValue = String(literal.value);
            if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
              throw new CompilerError(
                `Argument ${argDef.name} has invalid value ${fieldValue}`,
                callExpr.line,
                callExpr.column
              );
            }
            fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue;
          }
          if (argDef.type === "bool" && argValue.type !== "bool") {
            throw new CompilerError(
              `Argument ${argDef.name} must be boolean`,
              callExpr.line,
              callExpr.column
            );
          }
          inputs[argDef.name] = argValue.type === "bool" ? { type: "bool", value: argValue.value } : { type: "any", value: argValue.value };
        }
        const hat = {
          opcode: entry.opcode,
          fields,
          inputs
        };
        const blocks = this.parseBlockStatement(callExpr.then, null);
        return { hat, blocks };
      }
    } else if (callExpr.callee.type === "Identifier") {
      const funcName = callExpr.callee.name;
      return null;
    }
    return null;
  }
  // Parse hat member calls (like event.greenflag {})
  // This method is no longer needed as parser wraps member expressions with then blocks into CallExpressions
  // Statement parsing methods
  parseExpressionStatement(stmt, functionReturnType) {
    const expr = stmt.expression;
    if (expr.type === "CallExpression") {
      const callExpr = expr;
      if (callExpr.callee.type === "MemberExpression") {
        const memberExpr = callExpr.callee;
        if (memberExpr.object.type === "Identifier" && memberExpr.property.type === "Identifier") {
          const objectName = memberExpr.object.name;
          const propertyName = memberExpr.property.name;
          const namespace = this.namespaces.get(objectName);
          if (namespace) {
            return this.parseNamespaceCallAsCommand(
              memberExpr,
              callExpr.arguments,
              functionReturnType,
              callExpr.then,
              callExpr.line,
              callExpr.column
            );
          }
          const varType = this.globalScope.typeof(objectName);
          if (varType) {
            const args = callExpr.arguments.map(
              (arg) => this.parseExpr(arg).value
            );
            try {
              const methodResult = this.globalScope.stmtMethod(
                objectName,
                propertyName,
                args
              );
              if (methodResult) {
                return methodResult;
              }
              throw new CompilerError(
                `Method ${propertyName} not found on ${objectName} or cannot be used as a statement`,
                callExpr.line,
                callExpr.column
              );
            } catch (error) {
              if (error instanceof CompilerError) {
                throw error;
              }
              throw new CompilerError(
                error.message,
                callExpr.line,
                callExpr.column
              );
            }
          }
          throw new CompilerError(
            `Object ${objectName} not found (neither namespace nor variable)`,
            callExpr.line,
            callExpr.column
          );
        }
      } else if (callExpr.callee.type === "Identifier") {
        const funcName = callExpr.callee.name;
        return this.parseFunctionCallAsCommand(
          funcName,
          callExpr.arguments,
          callExpr.line,
          callExpr.column
        );
      }
    }
    throw new CompilerError(
      "Expression statements must be function calls or method calls",
      stmt.line,
      stmt.column
    );
  }
  operateVar(varName, operator, value, line, column) {
    try {
      switch (operator) {
        case "=":
          return this.globalScope.set(varName, value.value);
        case "+=":
          return this.globalScope.add(varName, value.value);
        case "-=":
          return this.globalScope.add(varName, {
            opcode: "operator_subtract",
            fields: {},
            inputs: {
              NUM1: { type: "any", value: "0" },
              NUM2: { type: "any", value: value.value }
            }
          });
        // *= -> = var * value, /= -> = var / value, etc.
        case "*=":
        case "/=":
        case "%=":
        case "..=": {
          const operatorMap = {
            "*": "operator_multiply",
            "/": "operator_divide",
            "%": "operator_mod",
            ".": "operator_join"
          };
          const op = operator[0];
          let varValue;
          try {
            const val = this.globalScope.get(varName);
            if (!val) {
              throw new CompilerError(
                `Variable ${varName} not found for operation`,
                0,
                0
              );
            }
            varValue = val;
          } catch (error) {
            throw new CompilerError(error.message, line, column);
          }
          const operationBlock = {
            opcode: operatorMap[op],
            fields: {},
            inputs: {
              NUM1: { type: "any", value: varValue },
              NUM2: { type: "any", value: value.value }
            }
          };
          return this.globalScope.set(varName, operationBlock);
        }
        default:
          throw new CompilerError(
            `Unsupported operator: ${operator}`,
            line,
            column
          );
      }
    } catch (error) {
      if (error instanceof CompilerError) {
        throw error;
      }
      throw new CompilerError(error.message, line, column);
    }
  }
  operateComputed(memberExpr, operator, value, line, column) {
    if (!memberExpr.computed) {
      throw new CompilerError(
        "Dot notation assignments are not supported",
        line,
        column
      );
    }
    if (memberExpr.object.type !== "Identifier") {
      throw new CompilerError(
        "Only simple computed assignments are supported",
        line,
        column
      );
    }
    const objectName = memberExpr.object.name;
    const varType = this.globalScope.typeof(objectName);
    if (varType !== "list") {
      throw new CompilerError(
        "Computed assignment is only supported for lists",
        line,
        column
      );
    }
    switch (operator) {
      case "=": {
        const index = this.parseExpr(memberExpr.property);
        try {
          const methodResult = this.globalScope.stmtMethod(
            objectName,
            "replace",
            [index.value, value.value]
          );
          if (methodResult) {
            return methodResult;
          }
          throw new CompilerError(
            `Method 'replace' not found on list ${objectName}`,
            line,
            column
          );
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error;
          }
          throw new CompilerError(error.message, line, column);
        }
      }
      case "+=":
      case "-=":
      case "*=":
      case "/=":
      case "%=":
      case "..=": {
        const operatorMap = {
          "*": "operator_multiply",
          "/": "operator_divide",
          "%": "operator_mod",
          ".": "operator_join"
        };
        const op = operator[0];
        const index = this.parseExpr(memberExpr.property);
        let currentValue;
        try {
          currentValue = this.parseComputedAccess(
            objectName,
            index,
            line,
            column
          );
        } catch (error) {
          throw new CompilerError(error.message, line, column);
        }
        const additionBlock = {
          opcode: operatorMap[op],
          fields: {},
          inputs: {
            NUM1: { type: "any", value: currentValue.value },
            NUM2: { type: "any", value: value.value }
          }
        };
        try {
          const methodResult = this.globalScope.stmtMethod(
            objectName,
            "replace",
            [index.value, additionBlock]
          );
          if (methodResult) {
            return methodResult;
          }
          throw new CompilerError(
            `Method 'replace' not found on list ${objectName}`,
            line,
            column
          );
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error;
          }
          throw new CompilerError(error.message, line, column);
        }
      }
      default:
        throw new CompilerError(
          `Unsupported operator: ${operator}`,
          line,
          column
        );
    }
  }
  parseIncrementStatement(stmt) {
    if (stmt.target.type === "Identifier") {
      const varName = stmt.target.name;
      switch (stmt.operator) {
        case "++":
          return this.operateVar(
            varName,
            "+=",
            { type: "any", value: "1" },
            stmt.line,
            stmt.column
          );
        case "--":
          return this.operateVar(
            varName,
            "-=",
            { type: "any", value: "1" },
            stmt.line,
            stmt.column
          );
        default:
          throw new CompilerError(
            `Unsupported increment operator: ${stmt.operator}`,
            stmt.line,
            stmt.column
          );
      }
    } else if (stmt.target.type === "MemberExpression") {
      const memberExpr = stmt.target;
      switch (stmt.operator) {
        case "++":
          return this.operateComputed(
            memberExpr,
            "+=",
            { type: "any", value: "1" },
            stmt.line,
            stmt.column
          );
        case "--":
          return this.operateComputed(
            memberExpr,
            "-=",
            { type: "any", value: "1" },
            stmt.line,
            stmt.column
          );
        default:
          throw new CompilerError(
            `Unsupported increment operator: ${stmt.operator}`,
            stmt.line,
            stmt.column
          );
      }
    } else {
      throw new CompilerError(
        "Unsupported increment target",
        stmt.line,
        stmt.column
      );
    }
  }
  parseAssignmentStatement(stmt) {
    const value = this.parseExpr(stmt.right);
    if (stmt.left.type === "Identifier") {
      const varName = stmt.left.name;
      return this.operateVar(
        varName,
        stmt.operator,
        value,
        stmt.line,
        stmt.column
      );
    } else if (stmt.left.type === "MemberExpression") {
      const memberExpr = stmt.left;
      return this.operateComputed(
        memberExpr,
        stmt.operator,
        value,
        stmt.line,
        stmt.column
      );
    } else {
      throw new CompilerError(
        "Unsupported assignment target",
        stmt.line,
        stmt.column
      );
    }
  }
  parseIfStatement(stmt, functionReturnType) {
    const condition = this.parseExpr(stmt.condition);
    this.ensureBooleanType(
      condition,
      "If condition must be boolean",
      stmt.line,
      stmt.column
    );
    const thenBlocks = stmt.then.type === "BlockStatement" ? this.parseBlockStatement(
      stmt.then,
      functionReturnType
    ) : this.parseStatement(stmt.then, functionReturnType);
    const elseBlocks = stmt.else ? stmt.else.type === "BlockStatement" ? this.parseBlockStatement(
      stmt.else,
      functionReturnType
    ) : this.parseStatement(stmt.else, functionReturnType) : [];
    return [
      {
        opcode: elseBlocks.length > 0 ? "control_if_else" : "control_if",
        fields: {},
        inputs: {
          CONDITION: { type: "bool", value: condition.value },
          SUBSTACK: { type: "substack", value: thenBlocks },
          ...elseBlocks.length > 0 && {
            SUBSTACK2: { type: "substack", value: elseBlocks }
          }
        }
      }
    ];
  }
  parseWhileStatement(stmt, functionReturnType) {
    const condition = this.parseExpr(stmt.condition);
    this.ensureBooleanType(
      condition,
      "While condition must be boolean",
      stmt.line,
      stmt.column
    );
    const bodyBlocks = stmt.body.type === "BlockStatement" ? this.parseBlockStatement(
      stmt.body,
      functionReturnType
    ) : this.parseStatement(stmt.body, functionReturnType);
    return [
      {
        opcode: "control_while",
        fields: {},
        inputs: {
          CONDITION: { type: "bool", value: condition.value },
          SUBSTACK: { type: "substack", value: bodyBlocks }
        }
      }
    ];
  }
  getBooleanLiteral(value) {
    if (value)
      return {
        type: "bool",
        value: {
          opcode: "operator_not",
          fields: {},
          inputs: {}
        }
      };
    return {
      type: "bool",
      value: {
        opcode: "operator_not",
        fields: {},
        inputs: {
          OPERAND: {
            type: "bool",
            value: {
              opcode: "operator_not",
              fields: {},
              inputs: {}
            }
          }
        }
      }
    };
  }
  parseForStatement(stmt, functionReturnType) {
    const condition = stmt.condition ? this.parseExpr(stmt.condition) : this.getBooleanLiteral(true);
    this.ensureBooleanType(
      condition,
      "While condition must be boolean",
      stmt.line,
      stmt.column
    );
    const init = stmt.init ? this.parseStatement(stmt.init, functionReturnType) : [];
    const increment = stmt.increment ? this.parseStatement(stmt.increment, functionReturnType) : [];
    const bodyBlocks = stmt.body.type === "BlockStatement" ? this.parseBlockStatement(
      stmt.body,
      functionReturnType
    ) : this.parseStatement(stmt.body, functionReturnType);
    return [
      ...init,
      {
        opcode: "control_while",
        fields: {},
        inputs: {
          CONDITION: { type: "bool", value: condition.value },
          SUBSTACK: {
            type: "substack",
            value: [...bodyBlocks, ...increment]
          }
        }
      }
    ];
  }
  parseLoopStatement(stmt, functionReturnType) {
    const bodyBlocks = stmt.body.type === "BlockStatement" ? this.parseBlockStatement(
      stmt.body,
      functionReturnType
    ) : this.parseStatement(stmt.body, functionReturnType);
    return [
      {
        opcode: "control_forever",
        fields: {},
        inputs: {
          SUBSTACK: { type: "substack", value: bodyBlocks }
        }
      }
    ];
  }
  parseReturnStatement(stmt, functionReturnType) {
    if (stmt.value) {
      if (functionReturnType === "void" || !functionReturnType) {
        throw new CompilerError(
          `Cannot return a value from a void function or a non-function context`,
          stmt.line,
          stmt.column
        );
      }
      const returnValue = this.parseExpr(stmt.value);
      if (functionReturnType === "bool") {
        this.ensureBooleanType(
          returnValue,
          "Return value must be boolean",
          stmt.line,
          stmt.column
        );
      }
      return [
        {
          opcode: "procedures_return",
          fields: {},
          inputs: {
            VALUE: { type: "any", value: returnValue.value }
          }
        }
      ];
    } else {
      if (functionReturnType !== "void" && functionReturnType !== null) {
        throw new CompilerError(
          `Must return a value from a non-void function`,
          stmt.line,
          stmt.column
        );
      }
      return [
        {
          opcode: "control_stop",
          fields: {
            STOP_OPTION: "this script"
          },
          inputs: {}
        }
      ];
    }
  }
  parseBlockStatement(stmt, functionReturnType) {
    const blocks = [];
    for (const statement of stmt.body) {
      blocks.push(...this.parseStatement(statement, functionReturnType));
    }
    return blocks;
  }
  // Namespace and function call helpers
  parseNamespaceCallAsCommand(memberExpr, args, functionReturnType, thenBlock, line, column) {
    if (memberExpr.object.type !== "Identifier" || memberExpr.property.type !== "Identifier") {
      throw new CompilerError(
        "Namespace calls must use simple identifiers",
        line,
        column
      );
    }
    const namespaceName = memberExpr.object.name;
    const functionName = memberExpr.property.name;
    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      throw new CompilerError(
        `Namespace ${namespaceName} not found`,
        line,
        column
      );
    }
    const entry = namespace.get(functionName);
    if (!entry) {
      throw new CompilerError(
        `Function ${functionName} not found in namespace ${namespaceName}`,
        line,
        column
      );
    }
    if (entry.type !== "void") {
      throw new CompilerError(
        `${namespaceName}.${functionName} cannot be used as a command (type: ${entry.type})`,
        line,
        column
      );
    }
    const parsedArgs = args.map((arg) => this.parseExpr(arg));
    const inputs = Object.assign({}, entry.inputs);
    const fields = Object.assign({}, entry.fields);
    let substackArgDef = entry.args.find((arg) => arg.type === "substack");
    const nonSubstackArgs = entry.args.filter((arg) => arg.type !== "substack");
    if (parsedArgs.length !== nonSubstackArgs.length) {
      throw new CompilerError(
        `${namespaceName}.${functionName} expects ${nonSubstackArgs.length} non-substack arguments, got ${parsedArgs.length}`,
        line,
        column
      );
    }
    for (let i = 0; i < nonSubstackArgs.length; i++) {
      const argDef = nonSubstackArgs[i];
      const argValue = parsedArgs[i];
      if (argDef.type === "bool" && argValue.type !== "bool") {
        throw new CompilerError(
          `Argument ${argDef.name} must be boolean`,
          line,
          column
        );
      }
      if (argDef.type === "field") {
        const rawArgValue = args[i];
        if (rawArgValue.type !== "Literal") {
          throw new CompilerError(
            `Argument ${argDef.name} must be a literal for field`,
            line,
            column
          );
        }
        const literal = rawArgValue;
        if (typeof literal.value !== "string") {
          throw new CompilerError(
            `Argument ${argDef.name} must be a string literal`,
            line,
            column
          );
        }
        const fieldValue = String(literal.value);
        if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
          throw new CompilerError(
            `Argument ${argDef.name} has invalid value ${fieldValue}`,
            line,
            column
          );
        }
        fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue;
      } else {
        inputs[argDef.name] = argValue.type === "bool" ? { type: "bool", value: argValue.value } : { type: "any", value: argValue.value };
      }
    }
    if (substackArgDef) {
      if (thenBlock) {
        inputs[substackArgDef.name] = {
          type: "substack",
          value: this.parseBlockStatement(thenBlock, functionReturnType)
        };
      } else {
        inputs[substackArgDef.name] = { type: "substack", value: [] };
      }
    } else if (thenBlock) {
      throw new CompilerError(
        `${namespaceName}.${functionName} does not accept a then block`,
        line,
        column
      );
    }
    return [
      {
        opcode: entry.opcode,
        fields,
        inputs
      }
    ];
  }
  parseFunctionCallAsCommand(funcName, args, line, column) {
    const func = this.funcs.get(funcName);
    if (!func) {
      throw new CompilerError(`Function ${funcName} not found`, line, column);
    }
    if (func.decl.returnType.name !== "void") {
      throw new CompilerError(
        `Function ${funcName} returns a value and cannot be used as a command`,
        line,
        column
      );
    }
    const parsedArgs = args.map((arg) => this.parseExpr(arg));
    if (parsedArgs.length !== func.decl.parameters.length) {
      throw new CompilerError(
        `Function ${funcName} expects ${func.decl.parameters.length} arguments, got ${parsedArgs.length}`,
        line,
        column
      );
    }
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType = func.decl.parameters[i].type.name === "bool" ? "bool" : "any";
      if (paramType === "bool" && parsedArgs[i].type !== "bool") {
        throw new CompilerError(
          `Parameter ${func.decl.parameters[i].name.name} must be boolean`,
          line,
          column
        );
      }
    }
    const inputs = {};
    for (let i = 0; i < parsedArgs.length; i++) {
      const paramName = func.decl.parameters[i].name.name;
      const paramType = func.decl.parameters[i].type.name === "bool" ? "bool" : "any";
      inputs[paramName] = paramType === "bool" ? { type: "bool", value: parsedArgs[i].value } : { type: "any", value: parsedArgs[i].value };
    }
    const names = func.decl.parameters.map((p) => p.name.name);
    return [
      {
        opcode: "procedures_call",
        fields: {},
        inputs,
        mutation: {
          tagName: "mutation",
          proccode: func.proccode,
          children: [],
          warp: func.decl.once ? "true" : "false",
          argumentids: JSON.stringify(names),
          // FIXME: use stable IDs
          argumentnames: JSON.stringify(names),
          argumentdefaults: JSON.stringify(
            func.decl.parameters.map(
              (p) => p.type.name === "any" ? "" : "false"
            )
          )
        }
      }
    ];
  }
  static getFunctions(globalScope, program) {
    const functions = /* @__PURE__ */ new Map();
    for (const decl of program.body) {
      if (decl.type === "FunctionDeclaration" || decl.type === "DecoratorStatement") {
        let funcDecl;
        let exportName = null;
        if (decl.type === "DecoratorStatement") {
          const decorator = decl;
          if (decorator.name.name !== "export") {
            throw new CompilerError(
              `Unknown decorator @${decorator.name.name}`,
              decorator.line,
              decorator.column
            );
          }
          if (decorator.target.type !== "FunctionDeclaration") {
            continue;
          }
          if (decorator.arguments.length !== 1) {
            throw new CompilerError(
              "@export takes at most one argument",
              decorator.line,
              decorator.column
            );
          }
          const arg = decorator.arguments[0];
          if (typeof arg.value !== "string") {
            throw new CompilerError(
              "@export argument must be a string literal",
              decorator.line,
              decorator.column
            );
          }
          exportName = arg.value;
          funcDecl = decorator.target;
        } else {
          funcDecl = decl;
        }
        const func = new ScratchFunction(
          globalScope.variables,
          funcDecl,
          exportName
        );
        if (functions.has(funcDecl.name.name)) {
          throw new CompilerError(
            `Function ${funcDecl.name.name} is already declared`,
            funcDecl.line,
            funcDecl.column
          );
        }
        functions.set(funcDecl.name.name, func);
      }
    }
    return functions;
  }
};
function getProgramInfo(program) {
  const variables = /* @__PURE__ */ new Map();
  const namespaces = /* @__PURE__ */ new Map();
  for (const decl of program.body) {
    if (decl.type === "VariableDeclaration" || decl.type === "DecoratorStatement") {
      let varDecl;
      let exportName = null;
      if (decl.type === "DecoratorStatement") {
        const decorator = decl;
        if (decorator.name.name !== "export") {
          throw new CompilerError(
            `Unknown decorator @${decorator.name.name}`,
            decorator.line,
            decorator.column
          );
        }
        if (decorator.target.type !== "VariableDeclaration") {
          continue;
        }
        if (decorator.arguments.length !== 1) {
          throw new CompilerError(
            "@export takes at most one argument",
            decorator.line,
            decorator.column
          );
        }
        const arg = decorator.arguments[0];
        if (typeof arg.value !== "string") {
          throw new CompilerError(
            "@export argument must be a string literal",
            decorator.line,
            decorator.column
          );
        }
        exportName = arg.value;
        varDecl = decorator.target;
      } else {
        varDecl = decl;
      }
      let value;
      if (varDecl.initializer.type === "ArrayExpression") {
        value = varDecl.initializer.elements.map((elem) => {
          if (elem.type === "Literal") {
            return elem.value;
          } else {
            throw new CompilerError(
              "Array elements must be literals",
              elem.line,
              elem.column
            );
          }
        });
      } else if (varDecl.initializer.type === "Literal") {
        value = varDecl.initializer.value;
      } else {
        throw new CompilerError(
          `Variable initializer must be a literal or array, got ${varDecl.initializer.type}`,
          varDecl.initializer.line,
          varDecl.initializer.column
        );
      }
      const variable = {
        name: varDecl.name,
        exportName,
        type: Array.isArray(value) ? "list" : "scalar",
        isGlobal: varDecl.isGlobal
      };
      if (variables.has(varDecl.name)) {
        const exist = variables.get(varDecl.name);
        if (exist && exist[0].isGlobal && variable.isGlobal || exist && !exist[0].isGlobal && !variable.isGlobal) {
          throw new CompilerError(
            `Variable ${varDecl.name} is already declared`,
            varDecl.line,
            varDecl.column
          );
        }
      }
      variables.set(varDecl.name, [variable, value]);
    } else if (decl.type === "NamespaceDeclaration") {
      const nsDecl = decl;
      const namespaceMap = /* @__PURE__ */ new Map();
      if (nsDecl.body && nsDecl.body.properties) {
        for (const prop of nsDecl.body.properties) {
          if (prop.value && typeof prop.value === "object") {
            namespaceMap.set(prop.key, prop.value);
          } else {
            throw new CompilerError(
              `Namespace property ${prop.key} must be an object`,
              nsDecl.line,
              nsDecl.column
            );
          }
        }
      }
      const existing = namespaces.get(nsDecl.name);
      if (existing) {
        namespaces.set(nsDecl.name, mergeNamespace(existing, namespaceMap));
      } else {
        namespaces.set(nsDecl.name, namespaceMap);
      }
    }
  }
  return { namespaces, variables };
}
function mergeNamespace(base, additional) {
  const merged = new Map(base);
  for (const [key, value] of additional) {
    if (merged.has(key)) {
      throw new CompilerError(`Duplicate namespace entry: ${key}`, 0, 0);
    }
    merged.set(key, value);
  }
  return merged;
}

// src/decompiler.ts
var import_utility2 = require("@scratch-fuse/utility");
var DecompilerError = class extends Error {
  constructor(message) {
    super(message);
    this.name = "DecompilerError";
  }
};
function isValidIdentifier(name) {
  if (!name || name.length === 0) return false;
  if (!/^[\p{L}\p{Nl}_]/u.test(name)) return false;
  if (!/^[\p{L}\p{Nl}_][\p{L}\p{Nl}\p{Nd}\p{Mn}\p{Mc}_]*$/u.test(name))
    return false;
  const keywords = [
    "if",
    "else",
    "while",
    "for",
    "loop",
    "return",
    "global",
    "true",
    "false",
    "namespace",
    "export"
  ];
  return !keywords.includes(name);
}
function sanitizeVarName(rawName) {
  const parts = rawName.trim().split(/\s+/).map((part) => part.replace(/[^\p{L}\p{Nl}\p{Nd}_]/gu, "")).filter((part) => part.length > 0);
  if (parts.length === 0) return null;
  let sanitized = parts.map((part, index) => {
    if (index === 0) {
      return part.charAt(0).toLowerCase() + part.slice(1).toLowerCase();
    } else {
      return part.charAt(0).toUpperCase() + part.slice(1).toLowerCase();
    }
  }).join("");
  if (!isValidIdentifier(sanitized[0])) sanitized = "v" + sanitized[0].toUpperCase() + sanitized.slice(1);
  return isValidIdentifier(sanitized) ? sanitized : null;
}
function sanitizeFnName(proccode) {
  const parts = proccode.split(/\s+/).map((part) => {
    if (part === "%s" || part === "%b") {
      return "";
    }
    return part.replace(/[^\p{L}\p{Nl}\p{Nd}_]/gu, "");
  }).filter((part) => part.length > 0);
  if (parts.length === 0) return null;
  let sanitized = parts.map((part, index) => {
    if (index === 0) {
      return part.charAt(0).toLowerCase() + part.slice(1).toLowerCase();
    } else {
      return part.charAt(0).toUpperCase() + part.slice(1).toLowerCase();
    }
  }).join("");
  if (!isValidIdentifier(sanitized[0])) sanitized = "f" + sanitized[0].toUpperCase() + sanitized.slice(1);
  return isValidIdentifier(sanitized) ? sanitized : null;
}
function makeIdentifier(name) {
  return {
    type: "Identifier",
    name,
    line: 0,
    column: 0
  };
}
function makeLiteral(value) {
  return {
    type: "Literal",
    value,
    raw: (0, import_utility2.sanitize)(value),
    line: 0,
    column: 0
  };
}
var VariableNameManager = class _VariableNameManager {
  // Shared across all decompilers for global variables
  globalVariableNames = /* @__PURE__ */ new Map();
  // Sprite-specific for local variables - allows name reuse across different sprites
  localVariableNames = /* @__PURE__ */ new Map();
  // Function names mapping
  functionNames = /* @__PURE__ */ new Map();
  // Counters for name generation (global is shared, local and func are not)
  globalCounter = { value: 0 };
  localCounter = 0;
  funcCounter = 0;
  /**
   * Get all used names in the specified scope
   * @param scope - 'global' | 'local' | 'func'
   */
  getUsedNames(scope) {
    const used = /* @__PURE__ */ new Set();
    for (const name of this.globalVariableNames.values()) {
      used.add(name);
    }
    if (scope === "local" || scope === "func") {
      for (const name of this.localVariableNames.values()) {
        used.add(name);
      }
    }
    for (const name of this.functionNames.values()) {
      used.add(name);
    }
    return used;
  }
  /**
   * Check if a name is already used in the appropriate scope
   */
  isUsed(name, scope) {
    const usedNames = this.getUsedNames(scope);
    return usedNames.has(name);
  }
  /**
   * Generate a unique name
   * @param preferredName - The preferred name
   * @param type - 'global' | 'local' | 'func'
   */
  generateName(preferredName, type) {
    const valid = isValidIdentifier(preferredName);
    if (!valid) {
      if (type === "func") {
        preferredName = sanitizeFnName(preferredName) ?? preferredName;
      } else preferredName = sanitizeVarName(preferredName) ?? preferredName;
    }
    if (!isValidIdentifier(preferredName) || this.isUsed(preferredName, type)) {
      const prefix = type === "global" ? "global" : type === "local" ? "local" : "func";
      let counter;
      let generatedName;
      if (type === "global") {
        do {
          this.globalCounter.value++;
          counter = this.globalCounter.value;
          generatedName = `${prefix}${counter}`;
        } while (this.isUsed(generatedName, type));
      } else if (type === "local") {
        do {
          this.localCounter++;
          counter = this.localCounter;
          generatedName = `${prefix}${counter}`;
        } while (this.isUsed(generatedName, type));
      } else {
        do {
          this.funcCounter++;
          counter = this.funcCounter;
          generatedName = `${prefix}${counter}`;
        } while (this.isUsed(generatedName, type));
      }
      return generatedName;
    }
    return preferredName;
  }
  /**
   * Assign a name to a variable and store the mapping
   * Returns the assigned name
   */
  assignVariableName(variable, preferredName, isGlobal) {
    const existingGlobal = this.globalVariableNames.get(variable);
    if (existingGlobal !== void 0) {
      return existingGlobal;
    }
    const existingLocal = this.localVariableNames.get(variable);
    if (existingLocal !== void 0) {
      return existingLocal;
    }
    const scope = isGlobal ? "global" : "local";
    const generatedName = this.generateName(preferredName, scope);
    if (isGlobal) {
      this.globalVariableNames.set(variable, generatedName);
    } else {
      this.localVariableNames.set(variable, generatedName);
    }
    return generatedName;
  }
  /**
   * Assign a name to a function and store the mapping
   * Returns the assigned name
   */
  assignFunctionName(proccode, preferredName) {
    const existing = this.functionNames.get(proccode);
    if (existing !== void 0) {
      return existing;
    }
    const generatedName = this.generateName(preferredName, "func");
    this.functionNames.set(proccode, generatedName);
    return generatedName;
  }
  /**
   * Get the assigned name for a function
   * Returns undefined if the function has not been assigned a name
   */
  getFunctionName(proccode) {
    return this.functionNames.get(proccode);
  }
  /**
   * Get the assigned name for a variable
   * Returns undefined if the variable has not been assigned a name
   */
  getVariableName(variable) {
    return this.globalVariableNames.get(variable) ?? this.localVariableNames.get(variable);
  }
  /**
   * Find a variable by its exportName or name
   * Returns the variable and its generated name, or null if not found
   * @param searchName - The name to search for (exportName or name)
   * @param expectedType - Optional type filter ('scalar' or 'list')
   */
  findVariable(searchName, expectedType) {
    for (const [variable, generatedName] of this.globalVariableNames) {
      if (variable.exportName && variable.exportName === searchName || variable.name === searchName) {
        if (expectedType && variable.type !== expectedType) {
          continue;
        }
        return { variable, generatedName };
      }
    }
    for (const [variable, generatedName] of this.localVariableNames) {
      if (variable.exportName && variable.exportName === searchName || variable.name === searchName) {
        if (expectedType && variable.type !== expectedType) {
          continue;
        }
        return { variable, generatedName };
      }
    }
    return null;
  }
  /**
   * Get all variables (global and local) managed by this name manager
   */
  getAllVariables() {
    const variables = [];
    for (const variable of this.globalVariableNames.keys()) {
      variables.push(variable);
    }
    for (const variable of this.localVariableNames.keys()) {
      variables.push(variable);
    }
    return variables;
  }
  /**
   * Create a new local scope manager that shares the global scope
   * This allows different sprites to reuse local variable names
   * - global counter and globalVariableNames are shared
   * - local counter, func counter, localVariableNames, and functionNames are not shared
   */
  createLocalScope() {
    const manager = new _VariableNameManager();
    manager.globalVariableNames = this.globalVariableNames;
    manager.globalCounter = this.globalCounter;
    return manager;
  }
};
function parseProccode(proccode) {
  const match = proccode.match(/^([a-zA-Z_][a-zA-Z0-9_]*)\((.*)\)$/);
  if (!match) return null;
  const name = match[1];
  const paramsStr = match[2].trim();
  if (!paramsStr) {
    return { name, params: [] };
  }
  const params = [];
  const paramParts = paramsStr.split(",");
  for (const part of paramParts) {
    const trimmed = part.trim();
    const paramMatch = trimmed.match(/^(.+?)\s*=\s*(?<!%)%([sb])$/);
    if (!paramMatch) return null;
    const paramName = paramMatch[1].trim();
    const paramType = paramMatch[2] === "b" ? "bool" : "any";
    params.push({ name: paramName, type: paramType });
  }
  return { name, params };
}
function generateParameterNames(params, functionVariables) {
  const usedNames = new Set(functionVariables);
  const result = [];
  for (let i = 0; i < params.length; i++) {
    const param = params[i];
    const originalName = param.name;
    if (!isValidIdentifier(originalName) || usedNames.has(originalName)) {
      let counter = 1;
      let generatedName = `arg${counter}`;
      while (usedNames.has(generatedName)) {
        counter++;
        generatedName = `arg${counter}`;
      }
      usedNames.add(generatedName);
      result.push({ name: generatedName, originalName, type: param.type });
    } else {
      usedNames.add(originalName);
      result.push({ name: originalName, originalName, type: param.type });
    }
  }
  return result;
}
function parseExportProccode(proccode, argumentNames) {
  const params = [];
  const templateParts = [];
  let currentPos = 0;
  let paramIndex = 0;
  const paramRegex = /(?<!%)%([sb])/g;
  let match;
  while ((match = paramRegex.exec(proccode)) !== null) {
    if (match.index > currentPos) {
      templateParts.push({
        text: proccode.slice(currentPos, match.index),
        paramIndex: null
      });
    }
    const paramType = match[1] === "b" ? "bool" : "any";
    const paramName = argumentNames[paramIndex] || `arg${paramIndex + 1}`;
    params.push({ name: paramName, type: paramType });
    templateParts.push({
      text: "",
      // Will be filled with [paramName] later
      paramIndex
    });
    currentPos = match.index + match[0].length;
    paramIndex++;
  }
  if (currentPos < proccode.length) {
    templateParts.push({
      text: proccode.slice(currentPos),
      paramIndex: null
    });
  }
  if (paramIndex !== argumentNames.length) {
    return null;
  }
  return { params, templateParts };
}
function parseOpcodeNamespace(opcode) {
  const parts = opcode.split("_");
  if (parts.length < 2) return null;
  const namespace = parts[0];
  const member = parts.slice(1).join("_");
  return { namespace, member };
}
var Decompiler = class {
  constructor(context) {
    this.context = context;
  }
  get namespaces() {
    return this.context.generatedNamespaces;
  }
  /**
   * Decompile a variable to VariableDeclaration
   * Also handles @export decorator if exportName differs from name
   */
  decompileVariable(variable, value) {
    const name = variable.name;
    let initializer;
    if (Array.isArray(value)) {
      const arrayExpr = {
        type: "ArrayExpression",
        elements: value.map((v) => makeLiteral(v)),
        line: 0,
        column: 0
      };
      initializer = arrayExpr;
    } else {
      initializer = makeLiteral(value);
    }
    if (variable.exportName) {
      const varDecl = {
        type: "VariableDeclaration",
        name,
        isGlobal: variable.isGlobal,
        initializer,
        line: 0,
        column: 0
      };
      const decorator = {
        type: "DecoratorStatement",
        name: makeIdentifier("export"),
        arguments: [makeLiteral(variable.exportName)],
        target: varDecl,
        line: 0,
        column: 0
      };
      return decorator;
    }
    return {
      type: "VariableDeclaration",
      name,
      isGlobal: variable.isGlobal,
      initializer,
      line: 0,
      column: 0
    };
  }
  /**
   * Decompile a reporter (value block) to Expression
   * @param reporter - The reporter or string literal to decompile
   * @param expectedType - The expected type based on usage context ('bool' for boolean contexts, 'any' otherwise)
   */
  decompileReporter(reporter, expectedType = "any") {
    if (typeof reporter === "string") {
      const num = Number(reporter);
      if (!isNaN(num) && String(num) === reporter) {
        return makeLiteral(num);
      }
      return makeLiteral(reporter);
    }
    const { opcode, fields, inputs } = reporter;
    if (opcode === "data_variable") {
      const varName = fields.VARIABLE;
      const found = this.context.nameManager.findVariable(varName, "scalar");
      if (!found) {
        throw new DecompilerError(`Variable not found: ${varName}`);
      }
      return makeIdentifier(found.generatedName);
    }
    if (opcode === "data_listcontents") {
      const listName = fields.LIST;
      const found = this.context.nameManager.findVariable(listName, "list");
      if (!found) {
        throw new DecompilerError(`List variable not found: ${listName}`);
      }
      return makeIdentifier(found.generatedName);
    }
    if (opcode === "argument_reporter_string_number") {
      const argId = fields.VALUE;
      const paramName = this.context.currentFunctionParams.get(argId);
      if (paramName) {
        return makeIdentifier(paramName);
      }
    }
    if (opcode === "argument_reporter_boolean") {
      const argId = fields.VALUE;
      const paramName = this.context.currentFunctionParams.get(argId);
      if (paramName) {
        return makeIdentifier(paramName);
      }
    }
    switch (opcode) {
      case "operator_add": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.NUM1.value),
          operator: "+",
          right: this.decompileReporter(inputs.NUM2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_subtract": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.NUM1.value),
          operator: "-",
          right: this.decompileReporter(inputs.NUM2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_multiply": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.NUM1.value),
          operator: "*",
          right: this.decompileReporter(inputs.NUM2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_divide": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.NUM1.value),
          operator: "/",
          right: this.decompileReporter(inputs.NUM2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_mod": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.NUM1.value),
          operator: "%",
          right: this.decompileReporter(inputs.NUM2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_join": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.STRING1.value),
          operator: "..",
          right: this.decompileReporter(inputs.STRING2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_equals": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.OPERAND1.value),
          operator: "==",
          right: this.decompileReporter(inputs.OPERAND2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_lt": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.OPERAND1.value),
          operator: "<",
          right: this.decompileReporter(inputs.OPERAND2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_gt": {
        const expr = {
          type: "BinaryExpression",
          left: this.decompileReporter(inputs.OPERAND1.value),
          operator: ">",
          right: this.decompileReporter(inputs.OPERAND2.value),
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_and": {
        const expr = {
          type: "BinaryExpression",
          left: inputs.OPERAND1 ? this.decompileReporter(
            inputs.OPERAND1.value,
            "bool"
          ) : makeLiteral(false),
          // Fallback if missing
          operator: "&&",
          right: inputs.OPERAND2 ? this.decompileReporter(
            inputs.OPERAND2.value,
            "bool"
          ) : makeLiteral(false),
          // Fallback if missing
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_or": {
        const expr = {
          type: "BinaryExpression",
          left: inputs.OPERAND1 ? this.decompileReporter(
            inputs.OPERAND1.value,
            "bool"
          ) : makeLiteral(false),
          // Fallback if missing
          operator: "||",
          right: inputs.OPERAND2 ? this.decompileReporter(
            inputs.OPERAND2.value,
            "bool"
          ) : makeLiteral(false),
          // Fallback if missing
          line: 0,
          column: 0
        };
        return expr;
      }
      case "operator_not": {
        const operand = inputs.OPERAND?.value;
        if (!operand) {
          return makeLiteral(true);
        }
        if (typeof operand !== "string" && operand.opcode === "operator_equals") {
          const innerExpr = {
            type: "BinaryExpression",
            left: this.decompileReporter(
              operand.inputs.OPERAND1.value
            ),
            operator: "!=",
            right: this.decompileReporter(
              operand.inputs.OPERAND2.value
            ),
            line: 0,
            column: 0
          };
          return innerExpr;
        } else if (typeof operand !== "string" && operand.opcode === "operator_gt") {
          const innerExpr = {
            type: "BinaryExpression",
            left: this.decompileReporter(
              operand.inputs.OPERAND1.value
            ),
            operator: "<=",
            right: this.decompileReporter(
              operand.inputs.OPERAND2.value
            ),
            line: 0,
            column: 0
          };
          return innerExpr;
        } else if (typeof operand !== "string" && operand.opcode === "operator_lt") {
          const innerExpr = {
            type: "BinaryExpression",
            left: this.decompileReporter(
              operand.inputs.OPERAND1.value
            ),
            operator: ">=",
            right: this.decompileReporter(
              operand.inputs.OPERAND2.value
            ),
            line: 0,
            column: 0
          };
          return innerExpr;
        }
        const expr = {
          type: "UnaryExpression",
          operator: "!",
          operand: this.decompileReporter(operand, "bool"),
          line: 0,
          column: 0
        };
        return expr;
      }
      // List operations as member expressions
      case "data_itemoflist": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.resolveVariable(fields.LIST, "list"),
          property: this.decompileReporter(inputs.INDEX.value),
          computed: true,
          line: 0,
          column: 0
        };
        return memberExpr;
      }
      case "data_lengthoflist": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.resolveVariable(fields.LIST, "list"),
          property: makeIdentifier("length"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [],
          line: 0,
          column: 0
        };
        return callExpr;
      }
      case "data_listcontainsitem": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.resolveVariable(fields.LIST, "list"),
          property: makeIdentifier("includes"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [this.decompileReporter(inputs.ITEM.value)],
          line: 0,
          column: 0
        };
        return callExpr;
      }
      case "data_itemnumoflist": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.resolveVariable(fields.LIST, "list"),
          property: makeIdentifier("indexOf"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [this.decompileReporter(inputs.ITEM.value)],
          line: 0,
          column: 0
        };
        return callExpr;
      }
      // String methods
      case "operator_letter_of": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.decompileReporter(inputs.STRING.value),
          property: this.decompileReporter(inputs.LETTER.value),
          computed: true,
          line: 0,
          column: 0
        };
        return memberExpr;
      }
      case "operator_length": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.decompileReporter(inputs.STRING.value),
          property: makeIdentifier("length"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [],
          line: 0,
          column: 0
        };
        return callExpr;
      }
      case "operator_contains": {
        const memberExpr = {
          type: "MemberExpression",
          object: this.decompileReporter(inputs.STRING1.value),
          property: makeIdentifier("includes"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [
            this.decompileReporter(inputs.STRING2.value)
          ],
          line: 0,
          column: 0
        };
        return callExpr;
      }
    }
    if (opcode === "procedures_call") {
      const result = this.decompileProcedureCall(reporter);
      if (!result) return makeLiteral(0);
      return result;
    }
    const namespaceMatch = this.tryMatchNamespace(reporter, expectedType);
    if (namespaceMatch) {
      return namespaceMatch.value;
    }
    throw new DecompilerError(
      `Cannot decompile reporter with opcode: ${opcode}. Unable to parse as namespace call.`
    );
  }
  /**
   * Resolve a variable by its export name or name
   * @param name - The variable name to resolve
   * @param expectedType - The expected type of the variable ('scalar' or 'list')
   */
  resolveVariable(name, expectedType) {
    const found = this.context.nameManager.findVariable(name, expectedType);
    if (!found) {
      throw new Error(`Unable to resolve ${expectedType} variable: ${name}`);
    }
    return makeIdentifier(found.generatedName);
  }
  /**
   * Common logic to match a block/reporter to a namespace entry from provided namespaces
   */
  tryMatchNamespaceFromMap(block, namespaces, supportSubstack) {
    for (const [nsName, namespace] of namespaces) {
      for (const [memberName, entry] of namespace) {
        if (entry.opcode === block.opcode) {
          if (supportSubstack) {
            const matched = this.tryMatchEntryWithSubstack(
              block,
              entry
            );
            if (matched) {
              const memberExpr = {
                type: "MemberExpression",
                object: makeIdentifier(nsName),
                property: makeIdentifier(memberName),
                computed: false,
                line: 0,
                column: 0
              };
              const callExpr = {
                type: "CallExpression",
                callee: memberExpr,
                arguments: matched.args,
                then: matched.then,
                line: 0,
                column: 0
              };
              return {
                type: entry.type,
                value: callExpr
              };
            }
          } else {
            const matched = this.tryMatchEntry(block, entry);
            if (matched) {
              const memberExpr = {
                type: "MemberExpression",
                object: makeIdentifier(nsName),
                property: makeIdentifier(memberName),
                computed: false,
                line: 0,
                column: 0
              };
              const callExpr = {
                type: "CallExpression",
                callee: memberExpr,
                arguments: matched,
                line: 0,
                column: 0
              };
              return {
                type: entry.type,
                value: callExpr
              };
            }
          }
        }
      }
    }
    return null;
  }
  /**
   * Generate a dynamic namespace call from opcode and save to generatedNamespaces
   */
  generateDynamicNamespaceCall(block, entryType) {
    const parsed = parseOpcodeNamespace(block.opcode);
    if (!parsed) return null;
    const args = [];
    let thenBlock;
    const generatedArgs = [];
    for (const [fieldName, fieldValue] of Object.entries(block.fields)) {
      args.push(makeLiteral(fieldValue));
      generatedArgs.push({ type: "field", name: fieldName, menu: null });
    }
    for (const [name, input] of Object.entries(block.inputs)) {
      if (input.type === "any") {
        args.push(this.decompileReporter(input.value, "any"));
        generatedArgs.push({ type: "any", name });
      } else if (input.type === "bool") {
        args.push(this.decompileReporter(input.value, "bool"));
        generatedArgs.push({ type: "bool", name });
      } else if (input.type === "substack") {
        thenBlock = {
          type: "BlockStatement",
          body: this.tryConvertToForLoop(
            input.value.map((b) => this.decompileBlock(b))
          ),
          line: 0,
          column: 0
        };
        generatedArgs.push({ type: "substack", name });
      }
    }
    if (!this.context.generatedNamespaces.has(parsed.namespace)) {
      this.context.generatedNamespaces.set(parsed.namespace, /* @__PURE__ */ new Map());
    }
    const ns = this.context.generatedNamespaces.get(parsed.namespace);
    if (!ns.has(parsed.member)) {
      const entry = {
        type: entryType,
        opcode: block.opcode,
        args: generatedArgs
      };
      ns.set(parsed.member, entry);
    }
    const memberExpr = {
      type: "MemberExpression",
      object: makeIdentifier(parsed.namespace),
      property: makeIdentifier(parsed.member),
      computed: false,
      line: 0,
      column: 0
    };
    const callExpr = {
      type: "CallExpression",
      callee: memberExpr,
      arguments: args,
      then: thenBlock,
      line: 0,
      column: 0
    };
    return {
      type: entryType,
      value: callExpr
    };
  }
  /**
   * Try to match a block/reporter to a namespace entry (for reporters)
   */
  tryMatchNamespace(block, entryType) {
    const matched = this.tryMatchNamespaceFromMap(
      block,
      this.context.namespaces,
      false
    );
    if (matched) return matched;
    const generatedMatch = this.tryMatchNamespaceFromMap(
      block,
      this.context.generatedNamespaces,
      false
    );
    if (generatedMatch) return generatedMatch;
    return this.generateDynamicNamespaceCall(block, entryType);
  }
  /**
   * Try to match block inputs/fields to namespace entry args
   */
  tryMatchEntry(block, entry) {
    const args = [];
    if (entry.inputs) {
      for (const [name, input] of Object.entries(entry.inputs)) {
        if (typeof input.value === "object") {
          if (JSON.stringify(block.inputs[name]) !== JSON.stringify(input.value)) {
            return null;
          }
        } else {
          if (block.inputs[name].type !== input.type) return null;
          if (block.inputs[name].type === "any" && input.type === "any" && block.inputs[name].value === input.value) {
            continue;
          }
          return null;
        }
      }
    }
    if (entry.fields) {
      for (const [name, value] of Object.entries(entry.fields)) {
        if (block.fields[name] !== value) return null;
      }
    }
    for (const arg of entry.args) {
      if (arg.type === "field") {
        const fieldValue = block.fields[arg.name];
        if (fieldValue === void 0) return null;
        const menu = arg.menu;
        if (menu) {
          const menuEntry = Object.entries(menu).find((e) => e[1] === fieldValue);
          if (!menuEntry) return null;
          args.push(makeLiteral(menuEntry[0]));
        } else args.push(makeLiteral(fieldValue));
      } else if (arg.type === "any") {
        const input = block.inputs[arg.name];
        if (!input) return null;
        args.push(this.decompileReporter(input.value, "any"));
      } else if (arg.type === "bool") {
        const input = block.inputs[arg.name];
        if (!input) return null;
        args.push(this.decompileReporter(input.value, "bool"));
      } else if (arg.type === "substack") {
        return null;
      }
    }
    return args;
  }
  /**
   * Decompile a block to Statement
   */
  decompileBlock(block) {
    const { opcode, fields, inputs } = block;
    if (opcode === "data_setvariableto") {
      const varName = fields.VARIABLE;
      const value = this.decompileReporter(inputs.VALUE.value);
      const stmt = {
        type: "AssignmentStatement",
        left: this.resolveVariable(varName, "scalar"),
        operator: "=",
        right: value,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "data_changevariableby") {
      const varName = fields.VARIABLE;
      const value = this.decompileReporter(inputs.VALUE.value);
      if (value.type === "Literal" && value.value === 1) {
        const stmt2 = {
          type: "IncrementStatement",
          operator: "++",
          target: this.resolveVariable(varName, "scalar"),
          line: 0,
          column: 0
        };
        return stmt2;
      }
      const stmt = {
        type: "AssignmentStatement",
        left: this.resolveVariable(varName, "scalar"),
        operator: "+=",
        right: value,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "control_if") {
      const condition = inputs.CONDITION ? this.decompileReporter(
        inputs.CONDITION.value,
        "bool"
      ) : makeLiteral(false);
      const thenBlocks = inputs.SUBSTACK?.value;
      const thenBody = thenBlocks ? {
        type: "BlockStatement",
        body: this.tryConvertToForLoop(
          thenBlocks.map((b) => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      } : void 0;
      const stmt = {
        type: "IfStatement",
        condition,
        then: thenBody ?? {
          type: "NoopStatement",
          line: 0,
          column: 0
        },
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "control_if_else") {
      const condition = inputs.CONDITION ? this.decompileReporter(
        inputs.CONDITION.value,
        "bool"
      ) : makeLiteral(false);
      const thenBlocks = inputs.SUBSTACK?.value;
      const elseBlocks = inputs.SUBSTACK2?.value;
      const thenBody = thenBlocks ? {
        type: "BlockStatement",
        body: this.tryConvertToForLoop(
          thenBlocks.map((b) => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      } : void 0;
      const elseBody = elseBlocks ? {
        type: "BlockStatement",
        body: this.tryConvertToForLoop(
          elseBlocks.map((b) => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      } : void 0;
      const stmt = {
        type: "IfStatement",
        condition,
        then: thenBody ?? {
          type: "NoopStatement",
          line: 0,
          column: 0
        },
        else: elseBody,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "control_forever") {
      const body = inputs.SUBSTACK?.value;
      const blockStmt = body ? {
        type: "BlockStatement",
        body: this.tryConvertToForLoop(
          body.map((b) => this.decompileBlock(b))
        ),
        line: 0,
        column: 0
      } : void 0;
      const stmt = {
        type: "LoopStatement",
        body: blockStmt ?? {
          type: "NoopStatement",
          line: 0,
          column: 0
        },
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "control_while") {
      const condition = this.decompileReporter(
        inputs.CONDITION.value,
        "bool"
      );
      const body = inputs.SUBSTACK.value;
      const blockStmt = {
        type: "BlockStatement",
        body: this.tryConvertToForLoop(body.map((b) => this.decompileBlock(b))),
        line: 0,
        column: 0
      };
      const stmt = {
        type: "WhileStatement",
        condition,
        body: blockStmt,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "control_stop") {
      const stopOption = fields.STOP_OPTION;
      if (stopOption === "this script") {
        const stmt = {
          type: "ReturnStatement",
          line: 0,
          column: 0
        };
        return stmt;
      }
    }
    if (opcode === "data_addtolist") {
      const listName = fields.LIST;
      const item = this.decompileReporter(inputs.ITEM.value);
      const memberExpr = {
        type: "MemberExpression",
        object: this.resolveVariable(listName, "list"),
        property: makeIdentifier("push"),
        computed: false,
        line: 0,
        column: 0
      };
      const callExpr = {
        type: "CallExpression",
        callee: memberExpr,
        arguments: [item],
        line: 0,
        column: 0
      };
      const stmt = {
        type: "ExpressionStatement",
        expression: callExpr,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "data_deleteoflist") {
      const listName = fields.LIST;
      const index = inputs.INDEX ? this.decompileReporter(inputs.INDEX.value) : makeLiteral("last");
      if (typeof inputs.INDEX.value === "string" && inputs.INDEX.value === "last") {
        const memberExpr = {
          type: "MemberExpression",
          object: this.resolveVariable(listName, "list"),
          property: makeIdentifier("pop"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [],
          line: 0,
          column: 0
        };
        const stmt = {
          type: "ExpressionStatement",
          expression: callExpr,
          line: 0,
          column: 0
        };
        return stmt;
      } else {
        const memberExpr = {
          type: "MemberExpression",
          object: this.resolveVariable(listName, "list"),
          property: makeIdentifier("remove"),
          computed: false,
          line: 0,
          column: 0
        };
        const callExpr = {
          type: "CallExpression",
          callee: memberExpr,
          arguments: [index],
          line: 0,
          column: 0
        };
        const stmt = {
          type: "ExpressionStatement",
          expression: callExpr,
          line: 0,
          column: 0
        };
        return stmt;
      }
    }
    if (opcode === "data_insertatlist") {
      const listName = fields.LIST;
      const index = this.decompileReporter(inputs.INDEX.value);
      const item = this.decompileReporter(inputs.ITEM.value);
      const memberExpr = {
        type: "MemberExpression",
        object: this.resolveVariable(listName, "list"),
        property: makeIdentifier("insert"),
        computed: false,
        line: 0,
        column: 0
      };
      const callExpr = {
        type: "CallExpression",
        callee: memberExpr,
        arguments: [index, item],
        line: 0,
        column: 0
      };
      const stmt = {
        type: "ExpressionStatement",
        expression: callExpr,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "data_replaceitemoflist") {
      const listName = fields.LIST;
      const index = this.decompileReporter(inputs.INDEX.value);
      const item = this.decompileReporter(inputs.ITEM.value);
      const memberExpr = {
        type: "MemberExpression",
        object: this.resolveVariable(listName, "list"),
        property: index,
        computed: true,
        line: 0,
        column: 0
      };
      const stmt = {
        type: "AssignmentStatement",
        left: memberExpr,
        operator: "=",
        right: item,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "data_deletealloflist") {
      const listName = fields.LIST;
      const memberExpr = {
        type: "MemberExpression",
        object: this.resolveVariable(listName, "list"),
        property: makeIdentifier("clear"),
        computed: false,
        line: 0,
        column: 0
      };
      const callExpr = {
        type: "CallExpression",
        callee: memberExpr,
        arguments: [],
        line: 0,
        column: 0
      };
      const stmt = {
        type: "ExpressionStatement",
        expression: callExpr,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "data_showlist" || opcode === "data_showvariable") {
      const varName = fields.LIST || fields.VARIABLE;
      const varType = opcode === "data_showlist" ? "list" : "scalar";
      const memberExpr = {
        type: "MemberExpression",
        object: this.resolveVariable(varName, varType),
        property: makeIdentifier("show"),
        computed: false,
        line: 0,
        column: 0
      };
      const callExpr = {
        type: "CallExpression",
        callee: memberExpr,
        arguments: [],
        line: 0,
        column: 0
      };
      const stmt = {
        type: "ExpressionStatement",
        expression: callExpr,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "data_hidelist" || opcode === "data_hidevariable") {
      const varName = fields.LIST || fields.VARIABLE;
      const varType = opcode === "data_hidelist" ? "list" : "scalar";
      const memberExpr = {
        type: "MemberExpression",
        object: this.resolveVariable(varName, varType),
        property: makeIdentifier("hide"),
        computed: false,
        line: 0,
        column: 0
      };
      const callExpr = {
        type: "CallExpression",
        callee: memberExpr,
        arguments: [],
        line: 0,
        column: 0
      };
      const stmt = {
        type: "ExpressionStatement",
        expression: callExpr,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "procedures_return") {
      const returnValue = this.decompileReporter(
        inputs.VALUE.value
      );
      const stmt = {
        type: "ReturnStatement",
        value: returnValue,
        line: 0,
        column: 0
      };
      return stmt;
    }
    if (opcode === "procedures_call") {
      const callExpr = this.decompileProcedureCall(block);
      if (!callExpr)
        return {
          type: "NoopStatement",
          line: 0,
          column: 0
        };
      const stmt = {
        type: "ExpressionStatement",
        expression: callExpr,
        line: 0,
        column: 0
      };
      return stmt;
    }
    const namespaceMatch = this.tryMatchNamespaceBlock(block);
    if (namespaceMatch) {
      const stmt = {
        type: "ExpressionStatement",
        expression: namespaceMatch.value,
        line: 0,
        column: 0
      };
      return stmt;
    }
    throw new DecompilerError(
      `Cannot decompile block with opcode: ${opcode}. Unable to parse as namespace call.`
    );
  }
  /**
   * Try to match a command block to namespace (for command blocks)
   */
  tryMatchNamespaceBlock(block) {
    const matched = this.tryMatchNamespaceFromMap(
      block,
      this.context.namespaces,
      true
    );
    if (matched) return matched;
    const generatedMatch = this.tryMatchNamespaceFromMap(
      block,
      this.context.generatedNamespaces,
      true
    );
    if (generatedMatch) return generatedMatch;
    return this.generateDynamicNamespaceCall(block, "void");
  }
  /**
   * Match entry with substack support
   */
  tryMatchEntryWithSubstack(block, entry) {
    const args = [];
    let thenBlock;
    if (entry.inputs) {
      for (const [name, input] of Object.entries(entry.inputs)) {
        if (typeof input.value === "object") {
          if (JSON.stringify(block.inputs[name]) !== JSON.stringify(input.value)) {
            return null;
          }
        } else {
          if (block.inputs[name].type !== input.type) return null;
          if (block.inputs[name].type === "any" && input.type === "any" && block.inputs[name].value === input.value) {
            continue;
          }
          return null;
        }
      }
    }
    if (entry.fields) {
      for (const [name, value] of Object.entries(entry.fields)) {
        if (block.fields[name] !== value) return null;
      }
    }
    for (const arg of entry.args) {
      if (arg.type === "field") {
        const fieldValue = block.fields[arg.name];
        if (fieldValue === void 0) return null;
        const menu = arg.menu;
        if (menu) {
          const menuEntry = Object.entries(menu).find((e) => e[1] === fieldValue);
          if (!menuEntry) return null;
          args.push(makeLiteral(menuEntry[0]));
        } else args.push(makeLiteral(fieldValue));
      } else if (arg.type === "any") {
        const input = block.inputs[arg.name];
        if (!input) return null;
        args.push(this.decompileReporter(input.value, "any"));
      } else if (arg.type === "bool") {
        const input = block.inputs[arg.name];
        if (!input) return null;
        args.push(this.decompileReporter(input.value, "bool"));
      } else if (arg.type === "substack") {
        const input = block.inputs[arg.name];
        if (!input)
          thenBlock = {
            type: "BlockStatement",
            body: [],
            line: 0,
            column: 0
          };
        else
          thenBlock = {
            type: "BlockStatement",
            body: this.tryConvertToForLoop(
              input.value.map((b) => this.decompileBlock(b))
            ),
            line: 0,
            column: 0
          };
      }
    }
    return { args, then: thenBlock };
  }
  /**
   * Try to detect for loop pattern in consecutive statements
   * Pattern: init statement + while loop with increment at end
   */
  tryConvertToForLoop(statements) {
    const result = [];
    let i = 0;
    while (i < statements.length) {
      if (i + 1 < statements.length) {
        const initStmt = statements[i];
        const nextStmt = statements[i + 1];
        if ((initStmt.type === "AssignmentStatement" || initStmt.type === "IncrementStatement") && nextStmt.type === "WhileStatement") {
          const assignment = initStmt;
          const whileStmt = nextStmt;
          const body = whileStmt.body.type === "BlockStatement" ? whileStmt.body.body : [whileStmt.body];
          if (body.length > 0) {
            const lastStmt = body[body.length - 1];
            if (lastStmt.type === "IncrementStatement" || lastStmt.type === "AssignmentStatement") {
              const forStmt = {
                type: "ForStatement",
                init: assignment,
                condition: whileStmt.condition,
                increment: lastStmt,
                body: body.length === 1 ? {
                  type: "NoopStatement",
                  line: 0,
                  column: 0
                } : {
                  type: "BlockStatement",
                  body: body.slice(0, -1),
                  // Remove the last increment statement
                  line: 0,
                  column: 0
                },
                line: 0,
                column: 0
              };
              result.push(forStmt);
              i += 2;
              continue;
            }
          }
        }
      }
      result.push(statements[i]);
      i++;
    }
    return result;
  }
  /**
   * Try to match a hat block to a namespace entry
   */
  tryMatchHatBlock(hat) {
    for (const [nsName, namespace] of this.context.namespaces) {
      for (const [memberName, entry] of namespace) {
        if (entry.opcode === hat.opcode && entry.type === "hat") {
          const matched = this.tryMatchEntry(hat, entry);
          if (matched) {
            const memberExpr = {
              type: "MemberExpression",
              object: makeIdentifier(nsName),
              property: makeIdentifier(memberName),
              computed: false,
              line: 0,
              column: 0
            };
            const callExpr = {
              type: "CallExpression",
              callee: memberExpr,
              arguments: matched,
              line: 0,
              column: 0
            };
            return callExpr;
          }
        }
      }
    }
    return null;
  }
  /**
   * Decompile a script
   */
  decompileScript(script) {
    const statements = script.blocks.map((block) => this.decompileBlock(block));
    const processedStatements = this.tryConvertToForLoop(statements);
    if (script.hat) {
      const hatCall = this.tryMatchHatBlock(script.hat);
      if (hatCall) {
        hatCall.then = {
          type: "BlockStatement",
          body: processedStatements,
          line: 0,
          column: 0
        };
        const hatStmt = {
          type: "ExpressionStatement",
          expression: hatCall,
          line: 0,
          column: 0
        };
        return [hatStmt];
      }
    }
    return [];
  }
  /**
   * Decompile a procedure call (function call)
   */
  decompileProcedureCall(block) {
    const mutation = block.mutation;
    if (!mutation) {
      throw new DecompilerError("procedures_call block missing mutation");
    }
    const proccode = mutation.proccode;
    if (!proccode) {
      throw new DecompilerError("procedures_call block missing proccode");
    }
    const argumentidsStr = mutation.argumentids;
    const argumentids = argumentidsStr ? JSON.parse(argumentidsStr) : [];
    const argumentTypes = (0, import_utility2.parseProccodeArgumentTypes)(proccode);
    const compiledFunction = this.context.functionRegistry.get(proccode);
    if (!compiledFunction) {
      console.warn(
        `Cannot resolve export format procedure call: ${proccode}. Function must be decompiled before its calls can be resolved.`
      );
      return null;
    }
    const args = [];
    for (let i = 0; i < argumentids.length; i++) {
      const argId = argumentids[i];
      const input = block.inputs[argId];
      const expectedType = argumentTypes[i] || "any";
      if (!input) {
        let defaultValue = compiledFunction.defaultValues[i];
        if (defaultValue !== void 0) {
          let defaultValueType = ["true", "false"].includes(defaultValue) ? "bool" : "any";
          if (expectedType === "bool" && defaultValueType !== "bool") {
            defaultValueType = "bool";
            defaultValue = "false";
          }
          console.warn(
            `Missing input for argument ${argId} in procedure call ${proccode}. Using default value: ${defaultValue}`
          );
          if (defaultValueType === "bool") {
            args.push(makeLiteral(defaultValue === "true"));
          } else {
            args.push(makeLiteral(defaultValue));
          }
        } else {
          throw new DecompilerError(
            `Missing input for argument ${argId} in procedure call ${proccode}`
          );
        }
        continue;
      }
      if (input.type === "bool") {
        args.push(this.decompileReporter(input.value));
      } else if (input.type === "any") {
        args.push(this.decompileReporter(input.value));
      } else {
        throw new DecompilerError(`Unexpected input type for argument ${argId}`);
      }
    }
    const functionName = compiledFunction.decl.name.name;
    const callExpr = {
      type: "CallExpression",
      callee: makeIdentifier(functionName),
      arguments: args,
      line: 0,
      column: 0
    };
    return callExpr;
  }
  /**
   * Infer return type from procedures_return blocks in the function body
   */
  inferReturnType(blocks) {
    const returnTypes = /* @__PURE__ */ new Set();
    const analyzeBlock = (block) => {
      if (block.opcode === "procedures_return") {
        const returnValue = block.inputs.VALUE.value;
        if (typeof returnValue !== "string") {
          const isBooleanExpr = this.isBooleanExpression(returnValue);
          returnTypes.add(isBooleanExpr ? "bool" : "any");
        } else {
          returnTypes.add("any");
        }
      }
      for (const inputKey in block.inputs) {
        const input = block.inputs[inputKey];
        if (input && "type" in input && input.type === "substack") {
          const substackInput = input;
          substackInput.value.forEach(analyzeBlock);
        }
      }
    };
    blocks.forEach(analyzeBlock);
    if (returnTypes.size === 0) {
      return "void";
    } else if (returnTypes.size === 1 && returnTypes.has("bool")) {
      return "bool";
    } else {
      return "any";
    }
  }
  /**
   * Check if a reporter is a boolean expression
   */
  isBooleanExpression(reporter) {
    const matched = this.tryMatchNamespaceFromMap(
      reporter,
      this.context.namespaces,
      false
    );
    return matched?.type === "bool";
  }
  /**
   * Decompile function from proccode and blocks
   */
  decompileFunction(raw) {
    let name;
    let params;
    let generatedExportName;
    const registeredFunction = this.context.functionRegistry.get(raw.proccode);
    if (!registeredFunction) {
      throw new DecompilerError(
        `Function not found in registry for proccode: ${raw.proccode}. Did you pass compiledFunctions to createDecompiler?`
      );
    }
    name = registeredFunction.decl.name.name;
    const parsed = parseProccode(raw.proccode);
    const argumentNames = raw.decl.parameters.map((p) => p.name.name);
    const usedVarNames = /* @__PURE__ */ new Set();
    for (const variable of this.context.nameManager.getAllVariables()) {
      const generatedName = this.context.nameManager.getVariableName(variable);
      if (generatedName) {
        usedVarNames.add(generatedName);
      }
    }
    if (parsed) {
      const paramNames = generateParameterNames(parsed.params, usedVarNames);
      params = paramNames.map((p) => ({
        name: makeIdentifier(p.name),
        type: makeIdentifier(p.type)
      }));
    } else if (argumentNames) {
      const exportParsed = parseExportProccode(raw.proccode, argumentNames);
      if (!exportParsed) {
        throw new DecompilerError(
          `Cannot parse export format proccode: ${raw.proccode} with ${argumentNames.length} arguments`
        );
      }
      const paramNames = generateParameterNames(
        exportParsed.params,
        usedVarNames
      );
      params = paramNames.map((p) => ({
        name: makeIdentifier(p.name),
        type: makeIdentifier(p.type)
      }));
      let exportNameBuilder = "";
      for (const part of exportParsed.templateParts) {
        if (part.paramIndex === null) {
          exportNameBuilder += part.text;
        } else {
          const generatedParamName = paramNames[part.paramIndex].name;
          exportNameBuilder += `[${generatedParamName}]`;
        }
      }
      generatedExportName = exportNameBuilder;
    } else {
      throw new DecompilerError(
        "Invalid decompileFunction call: must provide either compiler format proccode or argumentNames"
      );
    }
    const returnType = this.inferReturnType(raw.impl);
    const oldParams = this.context.currentFunctionParams;
    this.context.currentFunctionParams = /* @__PURE__ */ new Map();
    for (let i = 0; i < argumentNames.length; i++) {
      const argId = argumentNames[i];
      const paramName = params[i].name.name;
      this.context.currentFunctionParams.set(argId, paramName);
    }
    const body = {
      type: "BlockStatement",
      body: this.tryConvertToForLoop(
        raw.impl.map((block) => this.decompileBlock(block))
      ),
      line: 0,
      column: 0
    };
    this.context.currentFunctionParams = oldParams;
    const funcDecl = {
      type: "FunctionDeclaration",
      name: makeIdentifier(name),
      parameters: params,
      returnType: makeIdentifier(returnType),
      once: raw.decl.once,
      body,
      line: 0,
      column: 0
    };
    if (generatedExportName) {
      return {
        type: "DecoratorStatement",
        name: makeIdentifier("export"),
        arguments: [makeLiteral(generatedExportName)],
        target: funcDecl,
        line: 0,
        column: 0
      };
    }
    return funcDecl;
  }
};
function createDecompiler(globalVars, namespaces, compiledFunctions, sharedNameManager) {
  const functionRegistry = /* @__PURE__ */ new Map();
  const nameManager = sharedNameManager ? sharedNameManager.createLocalScope() : new VariableNameManager();
  for (const func of compiledFunctions) {
    const parsed = parseProccode(func.proccode);
    let preferredName;
    if (parsed) {
      preferredName = parsed.name;
    } else {
      preferredName = func.proccode;
    }
    const assignedName = nameManager.assignFunctionName(
      func.proccode,
      preferredName
    );
    func.decl.name.name = assignedName;
    functionRegistry.set(func.proccode, func);
  }
  for (const variable of globalVars) {
    if (variable.exportName && variable.name !== variable.exportName) {
      nameManager.assignVariableName(variable, variable.name, variable.isGlobal);
      continue;
    }
    const preferredName = variable.name;
    const isGlobal = variable.isGlobal;
    const generatedName = nameManager.assignVariableName(
      variable,
      preferredName,
      isGlobal
    );
    variable.name = generatedName;
    if (preferredName !== generatedName) {
      variable.exportName = preferredName;
    } else {
      variable.exportName = null;
    }
  }
  const context = {
    namespaces,
    generatedNamespaces: /* @__PURE__ */ new Map(),
    nameManager,
    functionRegistry,
    currentFunctionParams: /* @__PURE__ */ new Map()
  };
  return new Decompiler(context);
}
// Annotate the CommonJS export names for ESM import in node:
0 && (module.exports = {
  Compiler,
  CompilerError,
  Decompiler,
  DecompilerError,
  Scope,
  ScratchFunction,
  VariableNameManager,
  createDecompiler,
  getProgramInfo,
  mergeNamespace
});
