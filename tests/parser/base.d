module parser.base;

import std.conv;
import std.range;
import std.algorithm;

import rcdata.parser;


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
    Token[] tokens;

    dstring text() const pure @safe {

        return tokens.map!"a.content.dtext".join;

    }

}

TokenList consume(size_t consumed, Take!string input) pure @safe {

    return TokenList(consumed, [Token(consumed, input.to!dstring)]);

}

TokenList consume(TokenList listA, TokenList listB) pure @safe {

    return TokenList(
        listA.consumed + listB.consumed,
        listA.tokens ~ listB.tokens
    );

}
