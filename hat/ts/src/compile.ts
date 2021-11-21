import { readFileSync } from "fs";
import { basename } from "path";
import { Lexer, tokenTypeString, TokenType } from "./lexer";

export function compile(inputPath: string, outputPath: string): void {
    let data: string = readFileSync(inputPath, { encoding: 'utf8' });
    let lexer = new Lexer(inputPath, data);
    for (let token = lexer.next(); token.type != TokenType.EOF; token = lexer.next()) {
        console.log(token);
    }
}
