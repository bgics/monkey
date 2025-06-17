package lexer

import (
	"bufio"
	"io"
	"strings"

	"github.com/bgics/monkey/token"
)

type Lexer struct {
	reader *bufio.Reader
}

func New(input io.Reader) *Lexer {
	reader := bufio.NewReader(input)
	return &Lexer{reader: reader}
}

func (l *Lexer) NextToken() (token.Token, error) {
	var tok token.Token
	err := l.skipWhitespace()

	if err != nil {
		return tok, err
	}

	char, err := l.reader.ReadByte()

	if err == io.EOF {
		tok = token.Token{Type: token.EOF, Literal: ""}
		return tok, nil
	} else if err != nil {
		return tok, err
	}

	switch char {
	case '=':
		tok, err = l.tryMakeTwoCharToken(char, '=', token.EQ, token.ASSIGN)
		if err != nil {
			return tok, err
		}
	case '!':
		tok, err = l.tryMakeTwoCharToken(char, '=', token.NOT_EQ, token.BANG)
		if err != nil {
			return tok, err
		}
	case '+':
		tok = newToken(token.PLUS, char)
	case '-':
		tok = newToken(token.MINUS, char)
	case '*':
		tok = newToken(token.ASTERISK, char)
	case '/':
		tok = newToken(token.SLASH, char)
	case '<':
		tok = newToken(token.LT, char)
	case '>':
		tok = newToken(token.GT, char)
	case '(':
		tok = newToken(token.LPAREN, char)
	case ')':
		tok = newToken(token.RPAREN, char)
	case '{':
		tok = newToken(token.LBRACE, char)
	case '}':
		tok = newToken(token.RBRACE, char)
	case ',':
		tok = newToken(token.COMMA, char)
	case ';':
		tok = newToken(token.SEMICOLON, char)
	default:
		if isLetter(char) {
			tok.Literal, err = l.readSymbol(char, isLetter)
			if err != nil {
				return tok, err
			}
			tok.Type = token.LookupIdent(tok.Literal)
		} else if isDigit(char) {
			tok.Literal, err = l.readSymbol(char, isDigit)
			if err != nil {
				return tok, err
			}
			tok.Type = token.INT
		} else {
			tok = newToken(token.ILLEGAL, char)
		}
	}

	return tok, nil
}

func (l *Lexer) tryMakeTwoCharToken(currentChar, expectedNextChar byte, successTokenType, failTokenType token.TokenType) (token.Token, error) {
	var tok token.Token
	nextChar, err := l.reader.ReadByte()

	if err == io.EOF {
		tok = newToken(failTokenType, currentChar)
		return tok, nil
	} else if err != nil {
		return tok, err
	}

	if nextChar == expectedNextChar {
		tok = token.Token{Type: successTokenType, Literal: string(currentChar) + string(nextChar)}
		return tok, nil
	} else {
		err = l.reader.UnreadByte()
		tok = newToken(failTokenType, currentChar)
		return tok, err
	}
}

func (l *Lexer) skipWhitespace() error {
	for {
		ch, err := l.reader.ReadByte()

		if err == io.EOF {
			return nil
		} else if err != nil {
			return err
		}

		if !isWhitespace(ch) {
			err = l.reader.UnreadByte()
			return err
		}
	}
}

func (l *Lexer) readSymbol(startChar byte, isSymbol func(byte) bool) (string, error) {
	var builder strings.Builder
	builder.WriteByte(startChar)

	for {
		ch, err := l.reader.ReadByte()

		if err == io.EOF {
			return builder.String(), nil
		} else if err != nil {
			return builder.String(), err
		}

		if !isSymbol(ch) {
			err = l.reader.UnreadByte()
			return builder.String(), err
		}

		builder.WriteByte(ch)
	}
}

func newToken(tokenType token.TokenType, char byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(char)}
}

func isWhitespace(ch byte) bool {
	return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}
