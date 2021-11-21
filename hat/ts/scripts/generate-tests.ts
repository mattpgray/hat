import testJSON from '../test/lexer.test.json';
import {Lexer, Token, TokenType} from '../src/lexer';
import {writeFileSync} from 'fs';

interface TestCase {
    test: string;
    input: string;
    output: Token[];
}

function main() {
    let output: TestCase[] = [];
    for (let test of testJSON) {
    	let tokens: Token[] = [];
    	let lexer = new Lexer('', test.input);
    	let tokenType = TokenType.ILLEGAL;
    	while (tokenType != TokenType.EOF) {
    		let token = lexer.next();
		tokenType = token.type;
    		tokens.push(token);
    	}
    	output.push({
    		test: test.test,
    		input: test.input,
    		output: tokens,
    	})
    }
    writeFileSync('./test/lexer.test.json', JSON.stringify(output, null, 4));
}

main();
