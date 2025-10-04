import { getProgramInfo } from '../../compiler'
import { Lexer, Parser } from '@scratch-fuse/core'
import fuse from './builtin.fuse'

export const Sb3NamespacesRaw = fuse

export const Sb3Namespaces = (() => {
  const lexer = new Lexer(Sb3NamespacesRaw)
  const parser = new Parser(lexer.all())
  const program = parser.parse()
  return getProgramInfo(program).namespaces
})()
