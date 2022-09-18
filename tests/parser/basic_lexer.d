/// Basic rcdata.parser usage with arrays.
version (unittest) private pure @safe:

import std.algorithm;
import rcdata.parser;
import parser.base;


// Create the parser
struct Parser {

    mixin makeParser!(string, consume);

    static:

    // Merge a token list into a single token of given type
    TokenList merge(TokenType type, funs...)(string text) {

        const matched = match!funs(text);

        return TokenList(matched.consumed, [Token(matched.consumed, matched.text, type)]);

    }

    // A simple grammar
    TokenList lex(string input) @safe pure {

        return matchRepeat!(
            matchOr!(
                merge!(
                    TokenType.keyword,
                    matchOr!("if", "else", "end", "set", "equals", "echo"),
                ),
                merge!(
                    TokenType.number,
                    matchRepeatMinOnce!(
                        matchAny!(a => a >= '0' && a <= '9')
                    )
                ),
                merge!(
                    TokenType.identifier,
                    matchAny!(a => a >= 'a' && a <= 'z' || a >= 'A' && a <= 'Z')
                ),
                merge!(
                    TokenType.eol,
                    matchOr!("\r\n", "\r", "\n")
                ),
                merge!(
                    TokenType.whitespace,
                    matchRepeatMinOnce!(
                        matchOr!(" ", "\t")
                    )
                ),
            )
        )(input);

    }

}

// Simple lexer.
unittest {

    // Note: see file for definitions of TokenList, Token, etc.

    string source = `
        set A 15
        if A equals 15
            echo a
        end
    `;

    TokenList result = Parser.lex(source);

    with (TokenType) {

        // Filter all tokens except those with whitespace
        auto allButWhitespace = result.tokens.filter!(a => a.type != whitespace);
        // You'd like to keep them in the final output, as they're needed to calculate all token positions

        assert(allButWhitespace.equal([
            Token(1, "\n",     eol),
            Token(3, "set",    keyword),
            Token(1, "A",      identifier),
            Token(2, "15",     number),
            Token(1, "\n",     eol),
            Token(2, "if",     keyword),
            Token(1, "A",      identifier),
            Token(6, "equals", keyword),
            Token(2, "15",     number),
            Token(1, "\n",     eol),
            Token(4, "echo",   keyword),
            Token(1, "a",      identifier),
            Token(1, "\n",     eol),
            Token(3, "end",    keyword),
            Token(1, "\n",     eol),
        ]));

    }

}
