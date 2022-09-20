module parser.base;

import std.range;
import rcdata.parser;


enum TokenType {

    none, keyword, identifier, number, whitespace, eol

}

struct Token {

    TokenType type;
    string content;

}

Token[] consume(Take!string input) pure @safe {

    import std.conv;

    return [Token(TokenType.none, input.to!string)];

}

Token[] consume(Token[] listA, Token[] listB) pure @safe {

    return listA ~ listB;

}

auto allButWhitespace(Range)(Range tokens) {

    import std.algorithm;

    return tokens.filter!(a => a.type != TokenType.whitespace);

}
