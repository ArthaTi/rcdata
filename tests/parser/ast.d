/// AST generation test w/ matchCapture
import rcdata.parser;

// Let's reuse the lexer test
import tests.parser.base;
import tests.parser.basic_lexer;

import std.sumtype;


mixin makeParser!(string, supply, matchToken);


pure @safe:


// Basic parser setup


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

    import std.stdio;
    debug writefln!"matchToken %s"(match);

    enum message = format!"Expected a '%s' token"(type);

    assert(match.data.length == 2);

    // Check the type
    return match && match.data[1].type == type
        ? match
        : Match.fail(source, message);

}

/// Match a keyword
Match matchToken(string pattern)(string source) {

    import std.format;

    // Match an operator
    auto result = matchToken!(TokenType.keyword)(source);
    enum message = format!"Expected '%s'"(pattern);

    // Check the token
    return result && result.data[1].content == pattern
        ? result
        : Match.fail(source, message);

}


// AST structs definition


alias Expression = SumType!(IdentifierExpression, NumberExpression, EqualsExpression);

struct IdentifierExpression {

    string name;

}

struct NumberExpression {

    int value;

}

struct EqualsExpression {

    Expression* left, right;
    invariant(left);
    invariant(right);

    version (none)
    bool opEquals(const EqualsExpression other) const pure @safe @nogc {

        return *left == *other.left
            && *right == *other.right;

    }

}


alias Statement = SumType!(ConditionalStatement, SetStatement, EchoStatement, Expression);

struct ConditionalStatement {

    Expression* condition;
    Statement[] body;
    Statement[] elseBody;

    bool opEquals(const ConditionalStatement other) const pure @safe @nogc {

        // Compiler Bugs And Where To Find Them: std.sumtype
        return condition == other.condition
            && body == other.body
            && elseBody == other.elseBody;

    }

}

struct SetStatement {

    IdentifierExpression name;
    Expression* value;

}

struct EchoStatement {

    Expression* value;

}


// Match expressions


MatchCapture!IdentifierExpression identifier(string source) {

    return source.matchCapture!(
        IdentifierExpression,

        // Match an identifier
        TokenType.identifier,

        // Load it
        (string input, ref IdentifierExpression expr) => expr.name = input,
    );

}

MatchCapture!NumberExpression number(string source) {

    import std.conv;

    return source.matchCapture!(
        NumberExpression,

        // Match a number
        TokenType.number,

        // Load it
        (string input, ref expr) => expr.value = input.to!int,

    );

}

/// Match any expression
MatchCapture!(Expression*) expression(string source) {

    return source.matchCapture!(
        Expression*,

        // Match any expression
        matchOr!(
            number,
            identifier,
        ),
        (exp, ref result) => new Expression(exp),

        // Match operators
        matchOptional!(
            "equals",
            expression,
        ),
        (exp, ref result) {

            // Matched
            if (!exp.isNull) {

                // Wrap the expression in an equals
                *result = EqualsExpression(new Expression(result), new Expression(exp.get));

            }

        }

    );

}


// Match statements


MatchCapture!EchoStatement echo(string source) {

    return source.matchCapture!(
        EchoStatement,

        "echo",

        // Match an expression and save it to the result
        expression,
        (exp, ref result) => result.value = exp,
    );

}

MatchCapture!SetStatement set(string source) {

    return source.matchCapture!(
        SetStatement,

        "set",

        // Match the identifier to assign
        identifier,
        (id, ref result) => result.name = id,

        // Match the value
        expression,
        (exp, ref result) => result.value = exp,
    );

}

MatchCapture!ConditionalStatement conditional(string source) {

    return source.matchCapture!(
        ConditionalStatement,

        "if",
        expression,
        (exp, ref result) => result.condition = exp,

        // Match statements until else/end
        statementList!(matchOr!("else", "end")),
        (list, ref result) => result.body = list,

        // Optional "else" block
        matchOptional!(
            "else",
            statementList!"end",
        ),
        (list, ref result) => result.body = list.get([]),

        "end",
    );

}

MatchCapture!(Statement*) statement(string source) {

    return source.matchCapture!(
        Statement*,

        matchOr!(
            echo,
            set,
            conditional,
            expression,
        ),
        (stat, ref result) => new Statement(stat),
    );

}

MatchCapture!(Statement[]) statementList(alias terminator)(string source) {

    // Match as many statements as possible
    return source.matchUntil!(
        terminator,
        matchCapture!(
            Statement[],
            // Note: matchCapture is inside the repeat loop, so the call can be performed for each match
            // matchRepeat will keep the data in memory

            // Capture them into an array
            statement,
            (stat, ref result) => result ~= stat,
        ),
    );

}

unittest {

    import std.conv;

    auto list = statementList!(TokenType.eof)(`
        // Set a to 15 first
        set A 15

        // Check if it's true
        if A equals 15
            echo A
            echo 1
        end

        // We should check it the other way around, maybe!
        if 15 equals A
            echo 1
        else  // An else, to be sure
            echo 0
        end
    `);

    // Repetitive stuff
    auto idA = IdentifierExpression("A");
    auto number15 = NumberExpression(15);

    assert(ConditionalStatement() == ConditionalStatement());
    assert(Expression() == Expression());
    assert(Statement() == Statement());
    assert([Statement()] == [Statement()]);

    import std.stdio;
    debug writefln!"test: %s %s"(list, list.capture[0]);

    // Test boilerplate :D
    version (none)
    assert(list.capture[0] == [

        SetStatement(idA, number15.Expression).Statement,

        ConditionalStatement(
            EqualsExpression(new Expression(idA), new Expression(number15)).Expression,
            [
                EchoStatement(idA.Expression).Statement,
                EchoStatement(NumberExpression(1).Expression).Statement
            ]
        ).Statement,

        ConditionalStatement(
            EqualsExpression(new Expression(number15), new Expression(idA)).Expression,
            [
                EchoStatement(NumberExpression(1).Expression).Statement
            ],
            [
                EchoStatement(NumberExpression(0).Expression).Statement,
            ]
        ).Statement,

    ]);

}
