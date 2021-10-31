
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

interface Position {
    file: string;
    line: number;
    column: number;
}

class Lexer {
    data: string;
    line: string;
    col: number;
    lineNumber: number;


    constructor(data: string) {
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
        for (const char of this._iter()) {
            console.log(`main ${char}`);
            if (isWhitespace(char)) {
                continue;
            }
            if (isDigit(char)) {
                return this._consumeNumber();
            }
            if (char === '#') {
                return this._consumeComment();
            }
            // this._unexpectedToken();
        }
        return null;
    }

    _nextLine(): void {
        const idx = this.data.indexOf('\n');
        if (idx < 0) {
            this.line = this.data;
            this.data = '';
        } else {
            this.line = this.data.slice(0, idx);
            this.data = this.data.slice(idx);
        }
        if (this.line.length === 0) {
            this.col = 0;
            this.lineNumber = 0;
            return;
        }
        this.col = 1;
        this.lineNumber++;
        console.log(`line ${this.line}`);
    }

    _consumeNumber(): Token.INT {
        const start = this.col - 1;
        for (let char of this._iter()) {
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
        console.log(`number ${number} at position ${this.lineNumber}:${this.col}`);
        return Token.INT;
    }

    _consumeComment(): Token.COMMENT {
        let comment = this.line.slice(this.col).trimRight();
        console.log(`comment ${comment} at position ${this.lineNumber}:${this.col}`);
        // Consume the rest of the line without altering the line buffer so that we keep
        // line scanning to main tokenizing loop
        this.col = this.line.length;
        return Token.COMMENT;
    }

    _unexpectedToken(): never {
        throw new Error(`unexpected token ${this._currentChar()} at position ${this.lineNumber}:${this.col}`);
    }

    *_iter() {
        while (this.col <= this.line.length) {
            yield this._currentChar();
            this.col++;
        }
    }

    _currentChar(): string {
        return this.line.charAt(this.col - 1);
    }
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