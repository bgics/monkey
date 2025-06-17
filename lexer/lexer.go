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

	ch, err := l.reader.ReadByte()

	if err == io.EOF {
		tok = token.Token{Type: token.EOF, Literal: ""}
		return tok, nil
	} else if err != nil {
		return tok, err
	}

	newToken := func(tokenType token.TokenType) token.Token {
		return token.Token{Type: tokenType, Literal: string(ch)}
	}

	switch ch {
	case '=':
		tok, err = l.tryMakeTwoCharToken(ch, '=', token.EQ, token.ASSIGN)
		if err != nil {
			return tok, err
		}
	case '!':
		tok, err = l.tryMakeTwoCharToken(ch, '=', token.NOT_EQ, token.BANG)
		if err != nil {
			return tok, err
		}
	case '+':
		tok = newToken(token.PLUS)
	case '-':
		tok = newToken(token.MINUS)
	case '*':
		tok = newToken(token.ASTERISK)
	case '/':
		tok = newToken(token.SLASH)
	case '<':
		tok = newToken(token.LT)
	case '>':
		tok = newToken(token.GT)
	case '(':
		tok = newToken(token.LPAREN)
	case ')':
		tok = newToken(token.RPAREN)
	case '{':
		tok = newToken(token.LBRACE)
	case '}':
		tok = newToken(token.RBRACE)
	case ',':
		tok = newToken(token.COMMA)
	case ';':
		tok = newToken(token.SEMICOLON)
	default:
		if isLetter(ch) {
			tok.Literal, err = l.readIdentifier(ch)
			tok.Type = token.LookupIdent(tok.Literal)
		} else if isDigit(ch) {
			tok.Type = token.INT
			tok.Literal, err = l.readNumber(ch)
		} else {
			tok = newToken(token.ILLEGAL)
		}
	}

	return tok, err
}

func (l *Lexer) tryMakeTwoCharToken(currentChar, expectedNextChar byte, successTokenType, failTokenType token.TokenType) (token.Token, error) {
	var tok token.Token
	nextChar, err := l.reader.ReadByte()

	if err == io.EOF {
		tok = token.Token{Type: failTokenType, Literal: string(currentChar)}
		return tok, nil
	} else if err != nil {
		return tok, err
	}

	if nextChar == expectedNextChar {
		tok = token.Token{Type: successTokenType, Literal: string(currentChar) + string(nextChar)}
		return tok, nil
	} else {
		err = l.reader.UnreadByte()
		tok = token.Token{Type: failTokenType, Literal: string(currentChar)}
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

func (l *Lexer) readIdentifier(startChar byte) (string, error) {
	var builder strings.Builder
	builder.WriteByte(startChar)

	for {
		ch, err := l.reader.ReadByte()

		if err == io.EOF {
			return builder.String(), nil
		} else if err != nil {
			return builder.String(), err
		}

		if !isLetter(ch) {
			err = l.reader.UnreadByte()
			return builder.String(), err
		}

		builder.WriteByte(ch)
	}
}

func (l *Lexer) readNumber(startChar byte) (string, error) {
	var builder strings.Builder
	builder.WriteByte(startChar)

	for {
		ch, err := l.reader.ReadByte()

		if err == io.EOF {
			return builder.String(), nil
		} else if err != nil {
			return builder.String(), err
		}

		if !isDigit(ch) {
			err = l.reader.UnreadByte()
			return builder.String(), err
		}

		builder.WriteByte(ch)
	}
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
