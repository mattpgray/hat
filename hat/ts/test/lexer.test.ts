import { Lexer, TokenType, Token, Position } from "../src/lexer";
import testsJSON from './lexer.test.json';

interface TestCase {
    test: string;
    input: string;
    output: Token[];
}

let testCases: TestCase[] = testsJSON;

for (let testCase of testCases) {
   test(testCase.test, () => {
        const lexer = new Lexer('', testCase.input);
        let tokenType = TokenType.ILLEGAL;
        let output: Token[] = [];
        while (tokenType != TokenType.EOF) {
            let token = lexer.next();
            tokenType = token.type;
            output.push(token);
        }
        let expected: Token[] = testCase.output.map((testToken) => {
    	    let token = {
                type: testToken.type, 
                text: testToken.text,
                start: new Position(testToken.start.file, testToken.start.line, testToken.start.column)
            }
        if (testToken.value != undefined && testToken.value != null) {
    	    token.value = testToken.value;
        }
        return token;
        });
    	console.log(output);
    	console.log(expected);
        expect(output).toStrictEqual(expected);
    });
}
