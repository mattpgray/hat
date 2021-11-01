
enum Token {
    // Special tokens
    ILLEGAL = 1,
    EOF,
    COMMENT,

    // Identifiers
    INT, // 1213

    // Operations
    ADD // +
}

namespace Token {
    export function toString(token: Token): string {
        return `Token.${Token[token]}`;
    }
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
        return Token.EOF;
    }

    _nextTokenInLine(): Token | null {
        // We are either:
        // 1. On a new line so need to read the first char
        // 2. In the loop after emitting a token and so we have already
        //   cosumeed
        // Either way we do not need to step.
        let loop = true;
        while (loop) {
            const char = this._currentChar();
            // All of the consumers step by themselves and so we so not need to step again
            if (isDigit(char)) {
                return this._consumeNumber();
            }
            if (char === '/') {
                return this._consumeComment();
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

    _consumeNumber(): Token.INT {
        const start = this.col - 1;
        while (this._step()) {
            const char = this._currentChar();
            if (isWhitespace(char)) {
                break;
            }
            if (isDigit(char)) {
                continue;
            }
            // TODO: hex/binary/octal
            this._unexpectedToken();
        }
        const number = this.line.slice(start, this.col - 1);
        console.log(`number ${number} at position ${this._position(0).toString()}`);
        return Token.INT;
    }

    _consumeComment(): Token.COMMENT {
        if (this._step()) {
            this._unexpectedToken(-1);
        }
        let nextChar = this._currentChar();
        if (nextChar !== '/') {
            this._unexpectedToken();
        }
        let comment = this.line.slice(this.col).trimRight();
        console.log(`comment ${comment} at position ${this._position(0).toString()}`);
        // Consume the rest of the line without altering the line buffer so that we keep
        // line scanning to main tokenizing loop
        this.col = this.line.length;
        return Token.COMMENT;
    }

    _unexpectedToken(offset: number = 0): never {
        unexpectedToken(this._currentChar(), this._position(offset));
    }

    _step(): boolean {
        if (this.col >= this.line.length) {
            return false;
        }
        this.col++;
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
    throw new Error(`unexpected token ${s} at position ${pos.toString}`);
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

export {
    Lexer,
    Token,
    Position,
};