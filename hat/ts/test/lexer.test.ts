import { Lexer, TokenType, Token, Position } from "../src/lexer";

test('expect to consume one number', () => {
    const lexer = new Lexer('', '123');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 123, text: '123', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume one number with newline', () => {
    const lexer = new Lexer('', '123\n');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 123, text: '123', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume many numbers', () => {
    const lexer = new Lexer('', '12 34 56 78\n12 34 4 5\n2');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 12, text: '12', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 34, text: '34', start: new Position('', 1, 4) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 56, text: '56', start: new Position('', 1, 7) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 78, text: '78', start: new Position('', 1, 10) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 12, text: '12', start: new Position('', 2, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 34, text: '34', start: new Position('', 2, 4) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 4, text: '4', start: new Position('', 2, 7) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 5, text: '5', start: new Position('', 2, 9) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 2, text: '2', start: new Position('', 3, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume one comment', () => {
    const lexer = new Lexer('', '// my comment');
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: ' my comment', text: '// my comment', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume one comment with newline', () => {
    const lexer = new Lexer('', '// my comment\n');
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: ' my comment', text: '// my comment', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume many comments', () => {
    const lexer = new Lexer('', '// com1 // com2\n// com3\n    //com4');
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: ' com1 // com2', text: '// com1 // com2', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: ' com3', text: '// com3', start: new Position('', 2, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: 'com4', text: '//com4', start: new Position('', 3, 5) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume comment after integer', () => {
    const lexer = new Lexer('', '12 // my comment');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 12, text: '12', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: ' my comment', text: '// my comment', start: new Position('', 1, 4) });
});

test('expect to consume operation', () => {
    const lexer = new Lexer('', '+ -2+ */=');
    expect(lexer.next()).toStrictEqual({ type: TokenType.ADD, text: '+', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.SUB, text: '-', start: new Position('', 1, 3) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 2, text: '2', start: new Position('', 1, 4) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.ADD, text: '+', start: new Position('', 1, 5) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.MUL, text: '*', start: new Position('', 1, 7) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.DIV, text: '/', start: new Position('', 1, 8) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EQ,  text: '=', start: new Position('', 1, 9) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect to consume words', () => {
    const lexer = new Lexer('', 'words $$ __ a1');
    expect(lexer.next()).toStrictEqual({ type: TokenType.WORD, text: 'words', value: 'words', start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.WORD, text: '$$', value: '$$', start: new Position('', 1, 7) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.WORD, text: '__', value: '__', start: new Position('', 1, 10) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.WORD, text: 'a1', value: 'a1', start: new Position('', 1, 13) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});

test('expect number before words', () => {
    const lexer = new Lexer('', '1a');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, text: '1', value: 1, start: new Position('', 1, 1) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.WORD, text: 'a', value: 'a', start: new Position('', 1, 2) });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF, text: '', start: new Position('', 0, 0) });
});
