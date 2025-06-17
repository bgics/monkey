package repl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/bgics/monkey/lexer"
	"github.com/bgics/monkey/token"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		if _, err := io.WriteString(out, PROMPT); err != nil {
			fmt.Fprintf(os.Stderr, "Error writing prompt: %v\n", err)
			return
		}

		if !scanner.Scan() {
			return
		}

		line := scanner.Text()

		l := lexer.New(strings.NewReader(line))

		for {
			tok, err := l.NextToken()

			if err != nil {
				fmt.Fprintf(os.Stderr, "Lexer error: %v\n", err)
				break
			}

			if tok.Type == token.EOF {
				break
			}

			fmt.Fprintf(out, "%+v\n", tok)
		}
	}
}
