import { Lexer, Token } from "../src/lexer";

test('expect to consume one number', () => {
    const lexer = new Lexer('', '123');
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.EOF);
});

test('expect to consume one number with newline', () => {
    const lexer = new Lexer('', '123\n');
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.EOF);
});

test('expect to consume many numbers', () => {
    const lexer = new Lexer('', '12 34 56 78\n12 34 4 5\n2');
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.INT);
    expect(lexer.next()).toBe(Token.EOF);
});
