/// AST generation test w/ matchCapture
import rcdata.parser;

// Let's reuse the lexer test
import tests.parser.base;
import tests.parser.basic_lexer;

import std.sumtype;


mixin makeParser!(string, supply, matchToken);


pure @safe:


/// Match a token and check it against given type.
Match matchToken(TokenType type)(string source) {

    import std.format;

    // Get the token
    auto match = Lexer.match!(

        // Match all preceding whitespace
        Lexer.merge!(
            TokenType.comment,
            Lexer.matchRepeat!(
                Lexer.insignificant,
            ),
        ),

        // Match the token
        Lexer.token,

    )(source);

    enum message = format!"Expected a '%s' token"(type);

    assert(match.data.length == 3);

    // Check the type
    return match && match.data[1].type == type
        ? match
        : Match.fail(source, message);

}

/// Match a keyword
Match matchToken(string pattern)(string source) {

    // Match an operator
    auto result = matchToken!(TokenType.keyword)(source);
    enum message = format!"Expected '%s'"(pattern);

    // Check the token
    return result && result.data[1].content == pattern
        ? result
        : Match.fail(source, message);

}


alias Statement = SumType!(ConditionalStatement, SetStatement, EchoStatement, Expression);

struct ConditionalStatement {

    Expression condition;
    Statement[] body;
    Statement[] elseBody;

}

struct SetStatement {

    IdentifierExpression name;
    Expression value;

}

struct EchoStatement {

    Expression value;

}


alias Expression = SumType!(IdentifierExpression, NumberExpression);

struct IdentifierExpression {

    string name;

}

struct NumberExpression {

    int value;

}

MatchCapture!IdentifierExpression matchIdentifier(string source) {

    return source.matchCapture!(
        IdentifierExpression,

        // Match an identifier
        TokenType.identifier,

        // Load it
        (ref expr, string input) => expr.name = input,
    );

}

MatchCapture!NumberExpression matchNumber(string source) {

    import std.conv;

    return source.matchCapture!(
        NumberExpression,

        // Match a number
        TokenType.number,

        // Load it
        (ref expr, string input) => expr.value = input.to!int,

    );

}

/// Match either
MatchCapture!Expression matchExpression(string source) {

    return source.matchCapture!(
        Expression,

        matchOr!(
            matchNumber,
            matchIdentifier,
        ),
    );

}
