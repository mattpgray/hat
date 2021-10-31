import assert from "assert";
import { readFile, readFileSync } from "fs";
import { Lexer, Token } from "./lexer";

export function compile(inputPath: string, outputPath: string): void {
    let data: string = readFileSync(inputPath, { encoding: 'utf8' });
    console.log(data);
    let lexer = new Lexer(data);
    for (let token = Token.ILLEGAL; token != Token.EOF; token = lexer.next()) {
        console.log(token);
    }
}
