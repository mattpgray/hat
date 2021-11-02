import { Lexer, TokenType } from "../src/lexer";

test('expect to consume one number', () => {
    const lexer = new Lexer('', '123');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 123 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});

test('expect to consume one number with newline', () => {
    const lexer = new Lexer('', '123\n');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 123 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});

test('expect to consume many numbers', () => {
    const lexer = new Lexer('', '12 34 56 78\n12 34 4 5\n2');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 12 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 34 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 56 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 78 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 12 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 34 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 4 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 5 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 2 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});

test('expect to consume one comment', () => {
    const lexer = new Lexer('', '// my comment');
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: '// my comment' });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});

test('expect to consume one comment with newline', () => {
    const lexer = new Lexer('', '// my comment\n');
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: '// my comment' });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});

test('expect to consume many comments', () => {
    const lexer = new Lexer('', '// com1 // com2\n// com3\n    //com4');
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: '// com1 // com2' });
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: '// com3' });
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: '//com4' });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});

test('expect to consume comment after integer', () => {
    const lexer = new Lexer('', '12 // my comment');
    expect(lexer.next()).toStrictEqual({ type: TokenType.INT, value: 12 });
    expect(lexer.next()).toStrictEqual({ type: TokenType.COMMENT, value: '// my comment' });
    expect(lexer.next()).toStrictEqual({ type: TokenType.EOF });
});
