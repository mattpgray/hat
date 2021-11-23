import {stderr, exit} from "process";

enum TokenType {
    // Special tokens
    ILLEGAL = 1,
    EOF,
    COMMENT,
    NEWLINE, // Not using semi colons. Require on comments and newlines for expression ends

    // Identifiers
    INT, // 1213

    // Operations
    ADD, // +
    SUB, // -
    MUL, // *
    DIV, // /
    EQ,  // =

    WORD, // General word. Mapped to keywords in the parser
}

interface Token {
    type: TokenType;
    start: Position;
    text: string;
    value?: string | number;
}

function tokenTypeString(token: TokenType): string {
    return `TokenType.${TokenType[token]}`;
}

class Position {
    file: string;
    line: number;
    column: number;

    constructor(file: string, line: number, column: number) {
        this.file = file;
        this.line = line;
        this.column = column;
    }

    toString(): string {
        return `${this.file}:${this.line}:${this.column}`;
    }
}

interface Position {
    file: string;
    line: number;
    column: number;
}

class Lexer {
    filename: string;
    data: string;
    line: string;
    col: number;
    lineNumber: number;

    constructor(filename: string, data: string) {
        this.filename = filename;
        this.data = data;
        this.col = 0;
        this.line = '';
        this.lineNumber = 0;
        this._nextLine();
    }

    next(): Token {
        while (this.line.length > 0) {
            let token = this._nextTokenInLine();
            if (token != null) {
                return token;
            }
            this._nextLine();
        }
        return { type: TokenType.EOF, text: '', start: this._position(0) };
    }

    _nextTokenInLine(): Token | null {
        // We are either:
        // 1. On a new line so need to read the first char
        // 2. In the loop after emitting a token and so we have already
        //   cosumeed
        // Either way we do not need to step.
        let loop = this._hasChar();
        while (loop) {
            const char = this._currentChar();
            // All of the consumers step by themselves and so we so not need to step again
            if (isDigit(char)) {
                return this._consumeNumber();
            }
            if (isLetter(char) || char === '_' || char === '$') {
                return this._consumeWord();
            }
            switch (char) {
                case '/':
                    if (this._step() && this._currentChar() === '/') {
                        // Comments consume until the end of the line
                        const start = this._position(-1);
                        let comment = this.line.slice(this.col);
                        while (comment.length > 0) {
                            let lastChar = comment.slice(-1);
                            if (lastChar === '\r' || lastChar === '\n') {
                                comment = comment.slice(0, -1);
                                continue;
                            }
                            break;
                        }
                        this.col = this.line.length + 1;
                        return {
                            type: TokenType.COMMENT,
                            text: '//' + comment,
                            start: start,
                            value: comment,
                        };
                    }
                    return { type: TokenType.DIV, text: '/', start: this._position(-1) };
                case '+': this._step(); return { type: TokenType.ADD, text: '+', start: this._position(-1) };
                case '-': this._step(); return { type: TokenType.SUB, text: '-', start: this._position(-1) };
                case '*': this._step(); return { type: TokenType.MUL, text: '*', start: this._position(-1) };
                case '=': this._step(); return { type: TokenType.EQ, text: '=', start: this._position(-1) };
                case '\n': this._step(); return { type: TokenType.NEWLINE, text: '\n', start: this._position(-1) };
            }
            // Whitespace does not need to be emitted and so we can step
            if (isWhitespace(char)) {
                loop = this._step();
                continue;
            }
            this._unexpectedToken();
        }
        return null;
    }

    _nextLine(): void {
        const idx = this.data.indexOf('\n');
        if (idx < 0) {
            this.line = this.data;
            this.data = '';
        } else {
            this.line = this.data.slice(0, idx + 1);
            this.data = this.data.slice(idx + 1);
        }
        if (this.line.length === 0) {
            this.col = 0;
            this.lineNumber = 0;
            return;
        }
        this.col = 1;
        this.lineNumber++;
    }

    _consumeWord(): Token {
        const start = this._position(0);
        const word = this._consumeMatching(function (char: string): boolean {
            return isDigit(char) || isLetter(char) || char === '_' || char === '$';
        });
        return {
            type: TokenType.WORD,
            text: word,
            value: word,
            start: start,
        };
    }

    _consumeNumber(): Token {
        const start = this._position(0);
        const number = this._consumeMatching(isDigit);
        return {
            type: TokenType.INT,
            text: number,
            value: parseInt(number),
            start: start
        };
    }

    _consumeMatching(matcher: (char: string) => boolean): string {
        const start = this.col - 1;
        let end = this.col;
        while (this._step()) {
            const char = this._currentChar();
            if (matcher(char)) {
                end++;
                continue;
            }
            break;
        }
        const number = this.line.slice(start, end);
        return this.line.slice(start, end);

    }

    _peekString(s: string): boolean {
        if (!this._hasChar()) {
            return s === '';
        }
        const remaining = this.line.length - this.col + 1;
        if (s.length > remaining) {
            return false;
        }
        return this.line.slice(this.col - 1, this.col - 1 + s.length) === s;
    }

    _unexpectedToken(offset: number = 0): never {
        unexpectedToken(this._currentChar(), this._position(offset));
    }

    _hasChar(): boolean {
        return this.col <= this.line.length;
    }

    _step(): boolean {
        this.col++;
        if (!this._hasChar()) {
            return false;
        }
        return true;
    }

    _currentChar(): string {
        return this.line.charAt(this.col - 1);
    }

    _position(offset: number): Position {
        return new Position(this.filename, this.lineNumber, this.col + offset);
    }
}

function unexpectedToken(s: string, pos: Position): never {
    stderr.write(`${pos.toString()} unexpected token "${s}"\n`);
    exit(1);
}

function isWhitespace(c: string): boolean {
    return c === ' '
        || c === '\n'
        || c === '\t'
        || c === '\r'
        || c === '\f'
        || c === '\v';
}

function isDigit(c: string): boolean {
    return (c >= '0') && (c <= '9');
}

function isLetter(c: string): boolean {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
}

export {
    Lexer,
    Token,
    TokenType,
    tokenTypeString,
    Position
};
