import std/[lenientops, sequtils, strutils]

type
    TokenType* {.pure.} = enum
        default,
        `char`,
        `string`,
        `int`,
        `float`,
        `double`,
        `bool`,
        seperator,
        operator,
        ident,
        keyword,
        newline
    Token*   = object
        tktype*:TokenType
        lexeme*:string
    ASTInfo* {.pure.} = enum
        value,
        list,
        pair,
        `block`,
        `func`,
        bin,
        un
    ASTNode* = object
        head*:Token
        info*:ASTInfo
        body*:seq[ref ASTNode]
    AST = ref ASTNode


proc new_ast(head:Token): AST =
    new result
    result.head = head

const Operators = [
    "+",
    "-",
    "*",
    "/",
    "=",
    "==",
    "->"
]

const BinaryOperators = [
    "+",
    "-",
    "*",
    "/",
    "=",
    "=="
]

const UnaryOperators = [
    "-"
]

const Seperators = [
    '(',
    ')',
    '[',
    ']',
    ':',
    ',',
    ';'
]

const Keywords = [
    "fn",
    "let",
    "var",
    "const",
    "return",
    "if"
]

proc `$`*(x:AST, ind:int = 0, padding:int = 2): string =
    return $x.head & " => " & x.head.lexeme & (if x.body.len() == 0: "" else: x.body.map(proc(child:AST): string = $child).foldl(a & ",x\n" & b))

template repeat(n:int, body:untyped) =
    if n > 0:
        for _ in 1..n:
            body

proc lex*(inp:string): seq[Token] =
    let compilation_target = inp.split('\n')
    var
        last_indent = 0
        current_token: Token
        lexed_tokens:  seq[Token]
        unhandledInfo: string

    proc handleInfo() =
        if unhandledInfo != "":
            var more_unhandled: string
            if unhandledInfo in Operators:
                lexed_tokens.add(Token(
                    tktype: TokenType.operator,
                    lexeme: unhandledInfo
                ))
            else:
                for x in unhandledInfo:
                    if x in Seperators:
                        lexed_tokens.add(Token(
                            tktype: TokenType.seperator,
                            lexeme: $x
                        ))
                    elif more_unhandled != "":
                        echo more_unhandled
                        if more_unhandled in Operators:
                            lexed_tokens.add(Token(
                                tktype: TokenType.operator,
                                lexeme: more_unhandled
                            ))
                        else:
                            var even_more_unhandled = ""
                            for x in more_unhandled:
                                if even_more_unhandled in Operators:
                                    lexed_tokens.add(Token(
                                        tktype: TokenType.operator,
                                        lexeme: even_more_unhandled
                                    ))
                                    even_more_unhandled = ""
                                elif x in Seperators:
                                    lexed_tokens.add(Token(
                                        tktype: TokenType.seperator,
                                        lexeme: $x
                                    ))
                                    even_more_unhandled = ""
                                else:
                                    even_more_unhandled.add(x)
                                echo even_more_unhandled
                        more_unhandled = ""
                    else:
                        more_unhandled.add(x)
            if more_unhandled != "":
                if more_unhandled in Operators:
                    lexed_tokens.add(Token(
                        tktype: TokenType.operator,
                        lexeme: more_unhandled
                    ))
                else:
                    var even_more_unhandled = ""
                    for x in more_unhandled:
                        if even_more_unhandled in Operators:
                            lexed_tokens.add(Token(
                                tktype: TokenType.operator,
                                lexeme: even_more_unhandled
                            ))
                            even_more_unhandled = ""
                        elif x in Seperators:
                            lexed_tokens.add(Token(
                                tktype: TokenType.seperator,
                                lexeme: $x
                            ))
                            even_more_unhandled = ""
                        else:
                            even_more_unhandled.add(x)
                        echo even_more_unhandled
                more_unhandled = ""
            unhandledInfo = ""
    
    for line in compilation_target:
        last_indent = line.indentation
        for ch in line.unindent():
            case current_token.tktype
            of TokenType.default:
                case ch
                of Whitespace:
                    handleInfo()
                of IdentStartChars:
                    handleInfo()
                    current_token.lexeme  = $ch
                    current_token.tktype  = TokenType.ident
                of Digits:
                    handleInfo()
                    current_token.lexeme  = $ch
                    current_token.tktype  = TokenType.int
                of '\"':
                    handleInfo()
                    current_token.tktype  = TokenType.string
                of '\'':
                    handleInfo()
                    current_token.tktype  = TokenType.char
                else:
                    unhandledInfo.add(ch)
            of TokenType.char:
                if ch == '\'':
                    if current_token.lexeme[0] == '\\':
                        if current_token.lexeme.len() == 2:
                            case current_token.lexeme[1]
                            of '\\':
                                current_token.lexeme = "\\"
                            of '\"':
                                current_token.lexeme = "\""
                            of 'n':
                                current_token.lexeme = "\n"
                            else:
                                discard
                        elif current_token.lexeme.len() == 1:
                            current_token.lexeme.add(ch)
                    lexed_tokens.add(current_token)
                    current_token.reset()
                else:
                    current_token.lexeme.add(ch)
            of TokenType.string:
                if ch == '\"':
                    lexed_tokens.add(current_token)
                    current_token.reset()
                else:
                    current_token.lexeme.add(ch)
            of TokenType.int:
                case ch
                of Digits:
                    current_token.lexeme.add(ch)
                of '.':
                    current_token.lexeme.add(ch)
                    current_token.tktype = TokenType.double
                of Whitespace:
                    lexed_tokens.add(current_token)
                    current_token.reset()
                of '\"':
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    current_token.tktype = TokenType.string
                else:
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    unhandledInfo.add(ch)
            of TokenType.double:
                case ch
                of Digits:
                    current_token.lexeme.add(ch)
                of Whitespace:
                    lexed_tokens.add(current_token)
                    current_token.reset()
                else:
                    lexed_tokens.add(current_token)
                    current_token.reset()
            of TokenType.ident:
                case ch
                of IdentChars:
                    current_token.lexeme.add(ch)
                of Whitespace:
                    current_token.tktype = (case current_token.lexeme:
                        of "true", "false": TokenType.bool
                        of Keywords:        TokenType.keyword
                        else:               TokenType.ident)
                    lexed_tokens.add(current_token)
                    current_token.reset()
                else:
                    current_token.tktype = (case current_token.lexeme:
                        of "true", "false": TokenType.bool
                        of Keywords:        TokenType.keyword
                        else:               TokenType.ident)
                    lexed_tokens.add(current_token)
                    current_token.reset()
                    unhandledInfo.add(ch)
            else:
                unhandledInfo.add(ch)
        handleInfo()
        lexed_tokens.add(Token(
            tktype: TokenType.newline,
            lexeme: $line.indentation()
        ))

    return lexed_tokens

proc parse*(lexed_tokens: seq[Token]): AST =
    var
        parsed_ast: AST = new AST
        last_asts:  seq[AST] = @[parsed_ast]
        last_lines:   seq[int]   = @[0] #  gets the last indents indents in ast
        last_indents: seq[int] = @[0] #  gets the last indents in text
    
    parsed_ast.info = ASTInfo.block
    var skipNext = false
    for idx, x in lexed_tokens:
        if skipNext:
            skipNext = false
        else:
            case x.tktype
            of TokenType.keyword:
                last_asts[^1].body.add(new_ast x)
                last_asts.add(last_asts[^1].body[^1])
            of TokenType.char, TokenType.string, TokenType.int, TokenType.double, TokenType.float, TokenType.bool:
                last_asts[^1].body.add(new_ast x)
            of TokenType.ident:
                last_asts[^1].body.add(new_ast x)
            of TokenType.seperator:
                case x.lexeme
                of "[", "(":
                    if (try: last_asts[^1].body[^1].head.tktype == TokenType.ident except: false):
                        last_asts.add(last_asts[^1].body[^1])
                        last_asts[^1].info = ASTInfo.func
                    else:
                        last_asts[^1].body.add(new AST)    
                        last_asts[^1].body[^1].info = ASTInfo.list
                        last_asts.add(last_asts[^1].body[^1])
                of "]", ")":
                    last_asts.delete(last_asts.high)
                of ":":
                    if lexed_tokens[idx + 1].tktype == TokenType.newline:
                        if last_asts[^2].body[^1].head == Token(tktype: operator, lexeme: "->"):
                            last_asts.delete(last_asts.high)
                    last_asts[^1].body.add(new AST)
                    last_asts[^1].body[^1].info = ASTInfo.pair
                    last_asts.add(last_asts[^1].body[^1])
                    last_asts[^1].body.add(last_asts[^2].body[^2])
                    last_asts[^2].body.delete(last_asts[^2].body.high - 1)
                    if lexed_tokens[idx + 1].tktype == TokenType.newline:
                        last_asts[^1].body.add(new AST)
                        last_asts[^1].body[^1].info = ASTInfo.block
                        last_asts.add(last_asts[^1].body[^1])
                        skipNext = true
                        last_lines.add(last_asts.high)
                        last_indents.add(lexed_tokens[idx + 1].lexeme.parseInt())
                of ",":
                    if last_asts[^1].head.tktype == TokenType.operator:
                        last_asts.delete(last_asts.high)
                        last_asts.delete(last_asts.high)
                    else:
                        last_asts.delete(last_asts.high)
            of TokenType.operator:
                if last_asts[^1].body.len() == 0:
                    if x.lexeme in UnaryOperators:
                        last_asts[^1].body.add(new_ast x)
                        last_asts[^1].body[^1].info = ASTInfo.un
                        last_asts.add(last_asts[^1].body[^1])
                else:
                    if x.lexeme in BinaryOperators:
                        last_asts[^1].body.add(new_ast x)
                        last_asts[^1].body[^1].info = ASTInfo.bin
                        last_asts.add(last_asts[^1].body[^1]) 
                        last_asts[^1].body.add(last_asts[^2].body[^2])
                        last_asts[^2].body.delete(last_asts[^2].body.high - 1)
                    elif x.lexeme in UnaryOperators:
                        last_asts[^1].body.add(new_ast x)
                        last_asts[^1].body[^1].info = ASTInfo.un
                        last_asts.add(last_asts[^1].body[^1])
                    elif x.lexeme == "->":
                        last_asts[^1].body.add(new_ast x)
                        last_asts[^1].body[^1].body.add(new AST)
                        last_asts[^1].body[^1].body[^1].body.add(last_asts[^1].body[0..^2])
                        repeat last_asts[^1].body[^1].body.high - 1:
                            last_asts[^1].body[^1].body.delete(0)
                        repeat last_asts[^1].body.high:
                            last_asts[^1].body.delete(0)
                        last_asts[^1].body[^1].body.add(new AST)
                        last_asts.add(last_asts[^1].body[^1].body[^1])
            of TokenType.newline:
                echo last_indents[^1]
                echo x.lexeme
                var isFn = last_asts[^1].head == Token(tktype: keyword, lexeme: "fn") and last_asts[^1].body[^1].body[^1].head != Token(tktype: TokenType.default, lexeme: "")
                if last_indents[^1] < x.lexeme.parseInt():
                    last_lines.delete(last_lines.high)
                    last_indents.delete(last_indents.high)
                last_asts = last_asts[0..last_lines[^1]]
                if isFn:
                    last_asts.delete(last_asts.high)
                    last_lines[^1] -= 1
            else:
                discard
    return parsed_ast

proc analyze*(inp:AST): AST =
    if inp.body.len() == 0:
        result = inp
    elif inp.head.tktype == TokenType.operator:
        var cnstExpr = false
        for x in inp.body:
            if x.head.tktype == TokenType.ident:
                cnstExpr = true
                break
        if cnstExpr:
            var operands: array[2, Token]
            operands[0] = inp.body[0].head
            operands[1] = inp.body[1].head
            case inp.head.lexeme
            of "+":
                case operands[0].tktype
                of TokenType.int:
                    case operands[1].tktype
                    of TokenType.int:
                        result.head = Token(tktype: TokenType.int,   lexeme: $(operands[0].lexeme.parseInt() + operands[1].lexeme.parseInt()))
                    of TokenType.float, TokenType.double:
                        result.head = Token(tktype: TokenType.float, lexeme: $(operands[0].lexeme.parseInt() + operands[1].lexeme.parseFloat()))
                    else:
                        discard
                of TokenType.float, TokenType.double:
                    case operands[1].tktype
                    of TokenType.int:
                        result.head = Token(tktype: TokenType.int,   lexeme: $(operands[0].lexeme.parseFloat() + operands[1].lexeme.parseInt()))
                    of TokenType.float, TokenType.double:
                        result.head = Token(tktype: TokenType.float, lexeme: $(operands[0].lexeme.parseFloat() + operands[1].lexeme.parseFloat()))
                    else:
                        discard
                else:
                    discard
            of "-":
                case operands[0].tktype
                of TokenType.int:
                    case operands[1].tktype
                    of TokenType.int:
                        result.head = Token(tktype: TokenType.int,   lexeme: $(operands[0].lexeme.parseInt() - operands[1].lexeme.parseInt()))
                    of TokenType.float, TokenType.double:
                        result.head = Token(tktype: TokenType.float, lexeme: $(operands[0].lexeme.parseInt() - operands[1].lexeme.parseFloat()))
                    else:
                        discard
                of TokenType.float, TokenType.double:
                    case operands[1].tktype
                    of TokenType.int:
                        result.head = Token(tktype: TokenType.int,   lexeme: $(operands[0].lexeme.parseFloat() - operands[1].lexeme.parseInt()))
                    of TokenType.float, TokenType.double:
                        result.head = Token(tktype: TokenType.float, lexeme: $(operands[0].lexeme.parseFloat() - operands[1].lexeme.parseFloat()))
                    else:
                        discard
                else:
                    discard
    elif inp.info == ASTInfo.block:
        result = inp
        result.body = result.body.mapIt(analyze(it))
    else:
        result = inp
        result.body = result.body.mapIt(analyze(it))