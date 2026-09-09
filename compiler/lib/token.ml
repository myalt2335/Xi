type kind =
  | Include
  | LinkDirective
  | SubsystemDirective
  | KwExtern
  | KwCextern
  | KwCstruct
  | KwIntrinsic
  | KwF
  | KwIf
  | KwElse
  | KwSwitch
  | KwMatch
  | KwCase
  | KwDefault
  | KwWhile
  | KwFor
  | KwBreak
  | KwContinue
  | KwTry
  | KwIn
  | KwReturn
  | KwInt
  | KwUint
  | KwDec
  | KwString
  | KwBool
  | KwTrue
  | KwFalse
  | KwArray
  | KwRef
  | KwLink
  | KwStruct
  | KwAs
  | EqEq
  | NotEq
  | LessEq
  | GreaterEq
  | AndAnd
  | OrOr
  | ShiftLeft
  | CaretCaret
  | Namespace
  | Arrow
  | RangeInclusive
  | RangeExclusive
  | Eq
  | PlusEq
  | MinusEq
  | StarEq
  | SlashEq
  | PercentEq
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Amp
  | Pipe
  | Tilde
  | Less
  | Greater
  | Not
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Colon
  | Semicolon
  | Dot
  | StringLiteral
  | RawCBlock
  | DecLiteral
  | IntLiteral
  | Identifier
  | Eof

let kind_to_string = function
  | Include -> "Include"
  | LinkDirective -> "LinkDirective"
  | SubsystemDirective -> "SubsystemDirective"
  | KwExtern -> "KwExtern"
  | KwCextern -> "KwCextern"
  | KwCstruct -> "KwCstruct"
  | KwIntrinsic -> "KwIntrinsic"
  | KwF -> "KwF"
  | KwIf -> "KwIf"
  | KwElse -> "KwElse"
  | KwSwitch -> "KwSwitch"
  | KwMatch -> "KwMatch"
  | KwCase -> "KwCase"
  | KwDefault -> "KwDefault"
  | KwWhile -> "KwWhile"
  | KwFor -> "KwFor"
  | KwBreak -> "KwBreak"
  | KwContinue -> "KwContinue"
  | KwTry -> "KwTry"
  | KwIn -> "KwIn"
  | KwReturn -> "KwReturn"
  | KwInt -> "KwInt"
  | KwUint -> "KwUint"
  | KwDec -> "KwDec"
  | KwString -> "KwString"
  | KwBool -> "KwBool"
  | KwTrue -> "KwTrue"
  | KwFalse -> "KwFalse"
  | KwArray -> "KwArray"
  | KwRef -> "KwRef"
  | KwLink -> "KwLink"
  | KwStruct -> "KwStruct"
  | KwAs -> "KwAs"
  | EqEq -> "EqEq"
  | NotEq -> "NotEq"
  | LessEq -> "LessEq"
  | GreaterEq -> "GreaterEq"
  | AndAnd -> "AndAnd"
  | OrOr -> "OrOr"
  | ShiftLeft -> "ShiftLeft"
  | CaretCaret -> "CaretCaret"
  | Namespace -> "Namespace"
  | Arrow -> "Arrow"
  | RangeInclusive -> "RangeInclusive"
  | RangeExclusive -> "RangeExclusive"
  | Eq -> "Eq"
  | PlusEq -> "PlusEq"
  | MinusEq -> "MinusEq"
  | StarEq -> "StarEq"
  | SlashEq -> "SlashEq"
  | PercentEq -> "PercentEq"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | Percent -> "Percent"
  | Caret -> "Caret"
  | Amp -> "Amp"
  | Pipe -> "Pipe"
  | Tilde -> "Tilde"
  | Less -> "Less"
  | Greater -> "Greater"
  | Not -> "Not"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | LBracket -> "LBracket"
  | RBracket -> "RBracket"
  | Comma -> "Comma"
  | Colon -> "Colon"
  | Semicolon -> "Semicolon"
  | Dot -> "Dot"
  | StringLiteral -> "StringLiteral"
  | RawCBlock -> "RawCBlock"
  | DecLiteral -> "DecLiteral"
  | IntLiteral -> "IntLiteral"
  | Identifier -> "Identifier"
  | Eof -> "Eof"

type t = { kind : kind; lexeme : string; span : Span.t }
