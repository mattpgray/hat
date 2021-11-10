import { readFileSync } from "fs";
import { basename } from "path";
import { Lexer, tokenTypeString, TokenType } from "./lexer";

export function compile(inputPath: string, outputPath: string): void {
    let data: string = readFileSync(inputPath, { encoding: 'utf8' });
    let lexer = new Lexer(basename(inputPath), data);
    let token = lexer.next();
    while (token.type != TokenType.EOF) {
        token = lexer.next();
        console.log(tokenTypeString(token.type));
    }
}
