/// Test to cover usage of input and output ranges for rcdata.parser
version (unittest):

import std.conv;
import std.array;
import std.range;
import std.algorithm;

import tests.parser.base;
import rcdata.parser;


alias TokenList = Appender!(Token[]);


// Create the parser
mixin makeParser!(SourceRange, supply);

TokenList supply(Take!SourceRange input) pure @safe {

    return TokenList([
        Token(TokenType.none, input.to!string)
    ]);

}

TokenList supply(TokenList listA, TokenList listB) pure @safe {

    put(listA, listB[]);

    return listA;

}

// Merge a token list into a single token of given type
Match merge(TokenType type, funs...)(SourceRange text) {

    auto matched = match!funs(text);

    return matched
        ? Match(
            matched.matched,
            TokenList([
                Token(type, matched.data[].map!"a.content".join)
            ])
        )
        : matched;

}

immutable source = `
    set A 15
    if A equals 15
        echo a
    end
`d;

struct SourceRange {

    size_t index;

    size_t length() const pure @safe {

        return source.length - index;

    }

    dchar front() const pure @safe {

        return source[index];

    }

    void popFront() pure @safe {

        index++;

    }

    bool empty() const pure @safe {

        return length == 0;

    }

}

pure @safe unittest {

    // Create a simple grammar
    Match lex(SourceRange range) pure @safe {

        return range.matchRepeat!(
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
        );

    }

    with (TokenType) {

        // Filter all tokens except those with whitespace
        auto result = lex(SourceRange());

        assert(result.data[].allButWhitespace.equal([
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

}
