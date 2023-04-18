import lib/parse
import std/[os]

let lexed = lex(readFile(paramStr(1)))
echo lexed
let ast = parse(lexed)

echo analyze(ast)