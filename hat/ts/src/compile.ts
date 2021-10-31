import { readFileSync } from "fs";
import { basename } from "path";
import { Lexer, Token } from "./lexer";

export function compile(inputPath: string, outputPath: string): void {
    let data: string = readFileSync(inputPath, { encoding: 'utf8' });
    let lexer = new Lexer(basename(inputPath), data);
    let token = Token.ILLEGAL;
    while (token != Token.EOF) {
        token = lexer.next();
        console.log(Token.toString(token));
    }
}
