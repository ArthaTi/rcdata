/// Test to cover usage of input and output ranges for rcdata.parser
version (unittest) private pure @safe:

import std.conv;
import std.array;
import std.range;
import std.algorithm;

import rcdata.parser;

alias MyAppender = Appender!(Token[]);

enum TokenType {

    none, keyword, identifier, number, whitespace, eol

}

struct Token {

    size_t consumed;
    dstring content;
    TokenType type;

}

struct TokenList {

    size_t consumed;
    MyAppender tokens;

    dstring text() const pure {

        return tokens[].map!"a.content.dtext".join;

    }

}

// Create the parser
mixin makeParser!(SourceRange, consume, matchText);

TokenList consume(size_t consumed, Take!SourceRange input) {

    return TokenList(consumed, MyAppender([Token(consumed, input.to!dstring)]));

}

TokenList consume(TokenList listA, TokenList listB) {

    put(listA.tokens, listB.tokens[]);

    return TokenList(
        listA.consumed + listB.consumed,
        listA.tokens,
    );

}

// Merge a token list into a single token of given type
TokenList merge(TokenType type, funs...)(SourceRange text) {

    const matched = match!funs(text);

    return TokenList(matched.consumed, MyAppender([Token(matched.consumed, matched.text, type)]));

}

auto matchText(dstring text)(SourceRange input) {

    import std.string;
    import std.exception;

    import rcdata.utils;

    // Match EOF
    static if (text.length == 0) {

        enforceX(input.length == 0, format!"Expected end of file", input);

    }

    // Match the text
    else enforceX(input.startsWith(text), format!"Couldn't match `%s`"(text), input);

    // Consume the content
    return consume(text.length, input.take(text.length));

}

immutable source = `
    set A 15
    if A equals 15
        echo a
    end
`d;

struct SourceRange {

    size_t index;

    dchar front() const pure {

        return source[index];

    }

    void popFront() pure {

        index++;

    }

    bool empty() const pure {

        return index >= source.length;

    }

}

unittest {

    // Create a simple grammar
    TokenList lex(SourceRange range) pure @safe {

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
        auto allButWhitespace = lex(SourceRange()).tokens[].filter!(a => a.type != whitespace);
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
