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

func (l *Lexer) NextToken() token.Token {
	var tok token.Token
	err := l.skipWhitespace()

	if err != nil {
		return newToken(token.ERROR, err.Error())
	}

	char, err := l.reader.ReadByte()

	if err == io.EOF {
		return newToken(token.EOF, "")
	} else if err != nil {
		return newToken(token.ERROR, err.Error())
	}

	switch char {
	case '=':
		tok = l.tryMakeTwoCharToken(char, '=', token.EQ, token.ASSIGN)
	case '!':
		tok = l.tryMakeTwoCharToken(char, '=', token.NOT_EQ, token.BANG)
	case '+':
		tok = newToken(token.PLUS, string(char))
	case '-':
		tok = newToken(token.MINUS, string(char))
	case '*':
		tok = newToken(token.ASTERISK, string(char))
	case '/':
		tok = newToken(token.SLASH, string(char))
	case '<':
		tok = newToken(token.LT, string(char))
	case '>':
		tok = newToken(token.GT, string(char))
	case '(':
		tok = newToken(token.LPAREN, string(char))
	case ')':
		tok = newToken(token.RPAREN, string(char))
	case '{':
		tok = newToken(token.LBRACE, string(char))
	case '}':
		tok = newToken(token.RBRACE, string(char))
	case ',':
		tok = newToken(token.COMMA, string(char))
	case ';':
		tok = newToken(token.SEMICOLON, string(char))
	default:
		if isLetter(char) {
			literal, err := l.readSymbol(char, isLetter)
			if err != nil {
				return newToken(token.ERROR, err.Error())
			}

			tok.Type = token.LookupIdent(literal)
			tok.Literal = literal
		} else if isDigit(char) {
			literal, err := l.readSymbol(char, isDigit)
			if err != nil {
				return newToken(token.ERROR, err.Error())
			}

			tok.Type = token.INT
			tok.Literal = literal
		} else {
			tok = newToken(token.ILLEGAL, string(char))
		}
	}

	return tok
}

func (l *Lexer) tryMakeTwoCharToken(currentChar, expectedNextChar byte, successTokenType, failTokenType token.TokenType) token.Token {
	nextChar, err := l.reader.ReadByte()

	if err == io.EOF {
		return newToken(failTokenType, string(currentChar))
	} else if err != nil {
		return newToken(token.ERROR, err.Error())
	}

	if nextChar == expectedNextChar {
		twoCharTokenLiteral := string(currentChar) + string(nextChar)
		return newToken(successTokenType, twoCharTokenLiteral)
	} else {
		err = l.reader.UnreadByte()
		if err != nil {
			return newToken(token.ERROR, err.Error())
		}
		return newToken(failTokenType, string(currentChar))
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

func newToken(tokenType token.TokenType, literal string) token.Token {
	return token.Token{Type: tokenType, Literal: literal}
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
