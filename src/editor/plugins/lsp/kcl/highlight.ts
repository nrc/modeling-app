import { styleTags, tags as t } from '@lezer/highlight'

export const kclHighlight = styleTags({
  'fn var let const': t.definitionKeyword,
  return: t.controlKeyword,
  'true false': t.bool,
  nil: t.null,
  'AddOp MultOp ExpOp': t.arithmeticOperator,
  BangOp: t.logicOperator,
  CompOp: t.logicOperator,
  'Equals Arrow': t.definitionOperator,
  PipeOperator: t.controlOperator,
  String: t.string,
  Number: t.number,
  LineComment: t.lineComment,
  BlockComment: t.blockComment,
  Shebang: t.meta,
  PipeSubstitution: t.atom,
  VariableDefinition: t.definition(t.variableName),
  VariableName: t.variableName,
  PropertyName: t.propertyName,
  TagDeclarator: t.tagName,
  '( )': t.paren,
  '{ }': t.brace,
  '[ ]': t.bracket,
  ', . : ? ..': t.punctuation,
})
