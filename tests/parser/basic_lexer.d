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
    Match merge(TokenType type, funs...)(string text) {

        import std.array;

        auto matched = match!funs(text);

        return matched
            ? Match(
                matched.matched,
                [Token(type, matched.data.map!"a.content".join)]
            )
            : matched;

    }

    // A simple grammar
    Match lex(string input) @safe pure {

        return matchUntil!(
            "",  // Match until end of file
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

    Parser.Match result = Parser.lex(`
        set A 15
        if A equals 15
            echo a
        end
    `);

    // Check tokens except those with whitespace
    with (TokenType)
    assert(result.data.allButWhitespace.equal([
        Token(eol,        "\n"),
        Token(keyword,    "set"),
        Token(identifier, "A"),
        Token(number,     "15"),
        Token(eol,        "\n"),

        Token(keyword,    "if"),
        Token(identifier, "A"),
        Token(keyword,    "equals"),
        Token(number,     "15"),
        Token(eol,        "\n"),

        Token(keyword,    "echo"),
        Token(identifier, "a"),
        Token(eol,        "\n"),

        Token(keyword,    "end"),
        Token(eol,        "\n"),
    ]));

}

// If a match fails, we can find out where it happened
unittest {

    string source = `
        set A 1
        set B // We never defined a syntax for comments!
    `;

    Parser.Match result = Parser.lex(source);

    assert(!result);

    // matched.source should point at the comment
    assert(result.matched.source == source.find("//"));

    // Match data should include all tokens before the match
    with (TokenType)
    assert(result.data.allButWhitespace.equal([
        Token(eol,        "\n"),
        Token(keyword,    "set"),
        Token(identifier, "A"),
        Token(number,     "1"),
        Token(eol,        "\n"),

        Token(keyword,    "set"),
        Token(identifier, "B"),
    ]));

}
