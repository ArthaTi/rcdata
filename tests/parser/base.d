module tests.parser.base;

import std.range;
import rcdata.parser;


enum TokenType {

    none, keyword, identifier, number, comment, whitespace, eol

}

struct Token {

    TokenType type;
    string content;

}

Token[] supply(Take!string input) pure @safe {

    import std.conv;

    return [Token(TokenType.none, input.to!string)];

}

Token[] supply(Token[] listA, Token[] listB) pure @safe {

    return listA ~ listB;

}

auto allButWhitespace(Range)(Range tokens) {

    import std.algorithm;

    return tokens.filter!(a => a.type != TokenType.whitespace);

}
