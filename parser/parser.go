package parser

import (
	"fmt"
	"strconv"

	"github.com/bgics/monkey/ast"
	"github.com/bgics/monkey/lexer"
	"github.com/bgics/monkey/token"
)

const (
	_ = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // myFunction(X)
)

var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.ASTERISK: PRODUCT,
	token.SLASH:    PRODUCT,
}

type Parser struct {
	l *lexer.Lexer

	errors []string

	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

type (
	prefixParseFn func() (ast.Expression, error)
	infixParseFn  func(ast.Expression) (ast.Expression, error)
)

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func New(l *lexer.Lexer) (*Parser, error) {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.infixParseFns = make(map[token.TokenType]infixParseFn)

	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)

	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)

	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)

	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)

	p.registerPrefix(token.IF, p.parseIfExpression)

	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)

	for range 2 {
		if err := p.nextToken(); err != nil {
			return nil, err
		}
	}

	return p, nil
}

func (p *Parser) nextToken() error {
	tok, err := p.l.NextToken()
	if err != nil {
		return err
	}

	p.curToken = p.peekToken
	p.peekToken = tok
	return nil
}

func (p *Parser) ParseProgram() (*ast.Program, error) {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		if stmt, err := p.parseStatement(); err != nil {
			return nil, err
		} else if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}

		if err := p.nextToken(); err != nil {
			return nil, err
		}
	}

	return program, nil
}

func (p *Parser) parseStatement() (ast.Statement, error) {
	switch p.curToken.Type {
	case token.LET:
		if stmt, err := p.parseLetStatement(); err != nil {
			return nil, err
		} else if stmt != nil {
			return stmt, nil
		}
		return nil, nil
	case token.RETURN:
		if stmt, err := p.parseReturnStatement(); err != nil {
			return nil, err
		} else if stmt != nil {
			return stmt, nil
		}
		return nil, nil
	default:
		if stmt, err := p.parseExpressionStatement(); err != nil {
			return nil, err
		} else if stmt != nil {
			return stmt, nil
		}
		return nil, nil
	}
}

func (p *Parser) parseLetStatement() (*ast.LetStatement, error) {
	stmt := &ast.LetStatement{Token: p.curToken}

	if ok, err := p.expectPeek(token.IDENT); err != nil {
		return nil, err
	} else if !ok {
		return nil, nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if ok, err := p.expectPeek(token.ASSIGN); err != nil {
		return nil, err
	} else if !ok {
		return nil, nil
	}

	// TODO: Skipping expressions until semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		if err := p.nextToken(); err != nil {
			return nil, err
		}
	}

	return stmt, nil
}

func (p *Parser) parseReturnStatement() (*ast.ReturnStatement, error) {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	// TODO: Skipping expressions until semicolon
	for !p.curTokenIs(token.SEMICOLON) {
		if err := p.nextToken(); err != nil {
			return nil, err
		}
	}

	return stmt, nil
}

func (p *Parser) parseExpressionStatement() (*ast.ExpressionStatement, error) {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	expression, err := p.parseExpression(LOWEST)
	if err != nil {
		return nil, err
	}

	stmt.Expression = expression

	if p.peekTokenIs(token.SEMICOLON) {
		if err := p.nextToken(); err != nil {
			return nil, err
		}
	}

	return stmt, nil
}

func (p *Parser) parseExpression(precedence int) (ast.Expression, error) {
	prefix := p.prefixParseFns[p.curToken.Type]

	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil, nil
	}

	leftExp, err := prefix()
	if err != nil {
		return nil, err
	}

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp, nil
		}

		if err := p.nextToken(); err != nil {
			return nil, err
		}

		leftExp, err = infix(leftExp)
		if err != nil {
			return nil, err
		}
	}

	return leftExp, nil
}

func (p *Parser) parseIdentifier() (ast.Expression, error) {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}, nil
}

func (p *Parser) parseIntegerLiteral() (ast.Expression, error) {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil, nil
	}

	lit.Value = value

	return lit, nil
}

func (p *Parser) parsePrefixExpression() (ast.Expression, error) {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	if err := p.nextToken(); err != nil {
		return nil, err
	}

	right, err := p.parseExpression(PREFIX)
	if err != nil {
		return nil, err
	}

	expression.Right = right
	return expression, nil
}

func (p *Parser) parseInfixExpression(left ast.Expression) (ast.Expression, error) {
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Left:     left,
		Operator: p.curToken.Literal,
	}

	precedence := p.curPrecedence()
	if err := p.nextToken(); err != nil {
		return nil, err
	}

	right, err := p.parseExpression(precedence)
	if err != nil {
		return nil, err
	}

	expression.Right = right
	return expression, nil
}

func (p *Parser) parseBoolean() (ast.Expression, error) {
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}, nil
}

func (p *Parser) parseGroupedExpression() (ast.Expression, error) {
	if err := p.nextToken(); err != nil {
		return nil, err
	}

	exp, err := p.parseExpression(LOWEST)
	if err != nil {
		return nil, err
	}

	if ok, err := p.expectPeek(token.RPAREN); err != nil {
		return nil, err
	} else if !ok {
		return nil, nil
	}

	return exp, nil
}

func (p *Parser) parseIfExpression() (ast.Expression, error) {
	expression := &ast.IfExpression{Token: p.curToken}

	if ok, err := p.expectPeek(token.LPAREN); err != nil {
		return nil, err
	} else if !ok {
		return nil, nil
	}

	if err := p.nextToken(); err != nil {
		return nil, err
	}

	condition, err := p.parseExpression(LOWEST)
	if err != nil {
		return nil, err
	}

	expression.Condition = condition

	if ok, err := p.expectPeek(token.RPAREN); err != nil {
		return nil, err
	} else if !ok {
		return nil, nil
	}

	if ok, err := p.expectPeek(token.LBRACE); err != nil {
		return nil, err
	} else if !ok {
		return nil, nil
	}

	blockStatement := &ast.BlockStatement{Token: p.curToken, Statements: []ast.Statement{}}
	if err := p.nextToken(); err != nil {
		return nil, err
	}

	for !p.curTokenIs(token.RBRACE) {
		if stmt, err := p.parseStatement(); err != nil {
			return nil, err
		} else if stmt != nil {
			blockStatement.Statements = append(blockStatement.Statements, stmt)
		}

		if err := p.nextToken(); err != nil {
			return nil, err
		}
	}

	expression.Consequence = blockStatement
	return expression, nil
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) (bool, error) {
	if p.peekTokenIs(t) {
		err := p.nextToken()
		return true, err
	}
	p.peekError(t)
	return false, nil
}

func (p *Parser) peekPrecedence() int {
	if pr, ok := precedences[p.peekToken.Type]; ok {
		return pr
	}

	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if pr, ok := precedences[p.curToken.Type]; ok {
		return pr
	}

	return LOWEST
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}
