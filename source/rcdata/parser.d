/// Module for easily making parsers.
///
/// Can be used for both lexing and parsing grammars.
module rcdata.parser;

import std.range;
import std.traits;
import std.exception;

import rcdata.utils;

/// Check if the given function is a parser consumer.
enum isConsumer(alias consume, Input) = __traits(compiles, ConsumerResult!(consume, Input));

/// Get the result type of the given consumer.
template ConsumerResult(alias consume, Input) {

    import std.range;

    auto consumerResultA() { return consume(1, Input.init.take(1)); }
    auto consumerResultB() { return consume(consumerResultA, consumerResultA); }

    alias ConsumerResultA = typeof(consumerResultA());
    alias ConsumerResultB = typeof(consumerResultB());

    alias ConsumerResult = CommonType!(ConsumerResultA, ConsumerResultB);

    static assert(!is(ConsumerResult == void),
        "Return types of consume(size_t, " ~ Take!Input.stringof ~ ") (" ~ ConsumerResultA.stringof ~ ")"
            ~ " and consume(consume(...), consume(...)) (" ~ ConsumerResultB.stringof ~ ")"
            ~ " are not compatible");
    static assert(is(typeof(ConsumerResult.consumed) : size_t),
        "Return type of consume(...) does not expose a `size_t consumed` field");
    static assert(consumerResultB().consumed == 2,
        "consume(consume(...), consume(...)) doesn't correctly sum the `consumed` value");

}

/// Get the content element type of a parser result.
template ParserResultContent(T)
if (isParserResult!T) {

    alias ParserResultContent = typeof(T.content[0]);

}

mixin template makeParser(Input, alias consume)
if (is(ElementType!Input : dchar)) {

    mixin makeParser!(Input, consume, matchText);

    static auto matchText(dstring text)(Input input) {

        import std.range;
        import std.string;
        import std.exception;

        import rcdata.utils;

        // Match EOF
        static if (text.length == 0) {

            enforceX(input.empty, format!"Expected end of file", input);

        }

        // Match the text
        else enforceX(input.startsWith(text), format!"Couldn't match `%s`"(text), input);

        // Consume the content
        return consume(text.length, input.take(text.length));

    }

}

/// Mixin to produce matcher templates for processing `Input` input range and creating an `Output` output range.
///
/// The mixin can work both in module scope and at `struct`/`class` scope. No context is required for the matcher
/// functions to work.
///
/// Please note that internally the template only defines aliases to closures rather than functions. This allows them to
/// work without an instance in structs and classes, but still allow for context in template parameters.
mixin template makeParser(Input, alias consume, alias basicMatcher) {

    import std.format;
    import std.range;
    import std.traits;
    import std.functional;

    import rcdata.utils;
    import rcdata.parser;

    static:

    // Check arguments
    static assert(isInputRange!Input, Input.stringof ~ " isn't an input range");

    private {

        alias InputItem = ElementType!Input;
        alias Output = ConsumerResult!(consume, Input);

    }

    private void enforceX(T)(T condition, lazy string msg, Input input) {

        import std.exception;

        if (!condition) {

            throw new ParserMatchException(msg, input);

        }

    }

    alias ParserException = rcdata.parser.ParserExceptionImpl!(Input, consume);
    alias ParserMatchException = rcdata.parser.ParserMatchExceptionImpl!(Input, consume);
    alias ParserCriticalException = rcdata.parser.ParserCriticalExceptionImpl!(Input, consume);

    /// Match anything.
    alias matchAny() = (Input input)

        => matchAny!((InputItem item) => true)(input);


    /// Match one single item if `fun` returns `true`.
    alias matchAny(alias fun) = (Input input) {

        alias funCC = unaryFun!fun;

        enforceX(!input.empty, "Unexpected end of file", input);

        // Get the next item
        InputItem next = input.front;

        enforceX(funCC(next), "Delegate didn't match the element", input);

        return consume(1, input.take(1));

    };

    /// Match a sequence of tokens.
    alias match(funs...) = (Input input) {

        Output result;
        Input context = input;

        // Evaluate each matcher
        try static foreach (i, fun; funs) {{

            Output local;

            // Try to run the matcher
            static if (__traits(compiles, local = fun(context))) {

                local = fun(context);

            }

            // It's not callable. But maybe basicMatcher can handle it?
            else static if (__traits(compiles, local = basicMatcher!fun(context))) {

                // Alias to basicMatcher
                local = basicMatcher!fun(context);

            }

            // Try both to see what the errors are
            else {

                local = fun(context);
                local = basicMatcher!fun(context);

            }

            // Advance the input
            context.popFrontN(local.consumed);

            // Add to the result
            result = consume(result, local);

            static if (isArray!Output)
            assert(sameTail(context, input));

        }}

        // Add context to exceptions
        catch (ParserException exc) {

            throw exc.extend(result);

        }

        return result;

    };


    /// Match one of the given tokens.
    alias matchOr(funs...) = (Input input) {

        // Evaluate each matcher
        foreach (fun; funs) {

            // Return the result of the first one succeeding
            try return match!fun(input);

            // Failed parsing? That's ok, we'll try the next
            catch (ParserMatchException) { }

        }

        throw new ParserMatchException(format!"None of the tokens matched: %s"(funs.stringof), input);

    };

    /// Repeat the token sequence (as in `match`) zero to infinity times
    alias matchRepeat(funs...) = (Input input)

        => matchRepeatMin!(0, funs)(input);


    /// Repeat the token sequence at least once.
    alias matchRepeatMinOnce(funs...) = (Input input)

        => matchRepeatMin!(1, funs)(input);


    alias matchRepeatMin(size_t minMatches, funs...) = (Input input)

        => matchRepeatRange!funs(input, minMatches);


    /// Repeat the token sequence (as in match) `minMatches` to `maxMatches` times
    alias matchRepeatRange(size_t minMatches, size_t maxMatches, funs...) = (Input input)

        => matchRepeatRange!funs(input, minMatches, maxMatches);


    /// ditto
    template matchRepeatRange(funs...)
    if (!is(typeof(funs[0]) : size_t)) {

        alias matchRepeatRange = (Input input, size_t minMatches, size_t maxMatches = size_t.max) {

            Output result;
            size_t matches;

            while (true) {

                Output local;

                // Match the token
                try local = match!funs(input);

                // Stop if complete
                catch (ParserMatchException) break;

                // Check the length
                const length = local.consumed;

                // Add the result
                matches++;
                result = consume(result, local);

                // Advance the input
                input.popFrontN(length);

                // Special case: Match is empty, stop to prevent loops
                if (length == 0) break;

                // Stop if matched enough
                if (matches == maxMatches) break;

            }

            // Didn't match enough
            if (matches < minMatches) {

                throw new ParserMatchException(
                    format!"matchRepeatMin matched %s times, expected %s"(matches, minMatches),
                    result, input,
                );

            }

            return result;

        };

    }

    /// Match anything until the given token. Does NOT match the terminator.
    alias matchUntil(alias terminator) = (Input input)

        => matchUntil!(terminator, matchAny!())(input);


    /// Repeat the sequence until another token matches. Does NOT match the terminator.
    template matchUntil(alias terminator, funs...)
    if (funs.length != 0) {

        alias matchUntil = (Input input) {

            Output result;

            try while (true) {

                // Match the terminator
                try {

                    match!terminator(input);
                    break;

                }

                // Proceed like normal if not matched
                catch (ParserMatchException) { }


                // Match the token
                const local = match!funs(input);

                // Check the length
                const length = local.consumed;

                // Add the result
                result = consume(result, local);
                input.popFrontN(length);

                // Special case: Match is empty, stop to prevent loops
                if (length == 0) break;

            }

            // Failed, try to add context
            catch (ParserException exc) {

                throw exc.extend(result);

            }

            return result;

        };

    }

    /// Match zero or one instances of a token.
    alias matchOptional(funs...) = (Input input) {

        // Match the token
        try return match!funs(input);

        // Ignore failures
        catch (ParserMatchException) { }

        return consume(0, Input.init);

    };

    /// Require the given rule to match, otherwise throw given exception.
    ///
    /// Considers rules as "failing" if they throw `ParserMatchException`.
    ///
    /// Params:
    ///     Exc = Exception type to instantiate and throw. Will be passed a message string and, if supported, input
    ///         range of the following source text. If omitted, but `message` is given, throws
    ///         `ParserCriticalException`.
    ///     message = Message of the exception to throw.
    ///     instance = Already constructed instance of an exception to throw.
    ///     funs = Pattern to match. If empty, any pattern will count as failed.
    alias matchCritical(Exc : Throwable, string message, funs...) = (Input input) {

        // Exception accepting source data
        static if (__traits(compiles, new Exc(message, input))) {

            return matchCriticalImpl!funs(input, new Exc(message, input));

        }

        // Regular exception
        else return matchCriticalImpl!funs(input, new Exc(message));

    };

    /// ditto
    alias matchCritical(string message, funs...) = (Input input)

        => matchCritical!(ParserCriticalException, message, funs)(input);


    /// ditto
    alias matchCritical(Throwable instance, funs...) = (Input input)

        => matchCriticalImpl!funs(input, instance);


    /// ditto
    alias matchCriticalImpl(funs...) = (Input input, Throwable instance) {

        // If there's a sequence to check
        static if (funs.length) {

            // Try to match
            try return match!funs(input);

            // Failed parsing, throw the chosen exception
            catch (ParserMatchException) throw instance;

        }

        // Just throw on encounter
        else {

            // Let the compiler infer the return type
            if (false) return match!funs(input);

            throw instance;

        }

    };

    /// Adjust the `ParserMatchException` message thrown if the match fails.
    alias matchFailMessage(string message, funs...) = (Input input) {

        // Try to match
        try return match!funs(input);

        // Failed parsing
        catch (ParserMatchException exc) {

            exc.msg = message;
            throw exc;

        }

    };

    /// Check if the pattern matches, but do not consume it.
    alias lookAhead(funs...) = (Input input) {

        match!funs(input);
        return consume(0, Input.init);

    };

    /// Fail if the pattern matches. Succeeds if the rule throws a `ParserMatchException`. Doesn't consume anything.
    alias failAhead(funs...) = (Input input) {

        try cast(void) match!funs(input);

        // Failed as expected, good
        catch (ParserMatchException) return consume(0, Input.init);

        throw new ParserMatchException(format!"Unexpected %s"(funs.stringof), input);

    };

    /// Never matches.
    alias matchNever(string msg) = (Input input) pure @safe {

        throw new ParserMatchException(msg, input);

    };

}

/// Exception type thrown on any parser failure.
abstract class ParserExceptionImpl(Input, alias consume) : RCDataException {

    import std.range;

    static assert(isInputRange!Input, Input.stringof ~ " isn't an input range");

    alias Output = ConsumerResult!(consume, Input);

    Output context;
    Input source;

    this(string msg, Input source, string filename = __FILE__, size_t line = __LINE__) pure @safe {

        super(msg, filename, line);
        this.source = source;

    }

    this(string msg, Output context, Input source, string filename = __FILE__, size_t line = __LINE__) pure @safe {

        this(msg, source, filename, line);
        this.context = context;

    }

    mixin template parserExeptionCtors() {

        this(string msg, Input source, string filename = __FILE__, size_t line = __LINE__) pure @safe {

            super(msg, source, filename, line);

        }

        this(string msg, Output context, Input source, string filename = __FILE__, size_t line = __LINE__)
            pure @safe
        do {
            super(msg, context, source, filename, line);
        }

    }

    /// Extend the exception with data about the parent context.
    typeof(this) extend()(Output context) {

        this.context = consume(context, this.context);
        return this;

    }

}

/// Recoverable exception thrown when the parser failed to match a rule.
class ParserMatchExceptionImpl(Input, alias consume) : ParserExceptionImpl!(Input, consume) {

    mixin parserExeptionCtors;

}

/// Exception thrown when the parser fails to match a critical rule. The parser should stop processing once thrown,
/// although catches done to `extend` exceptions with additional context are still allowed.
class ParserCriticalExceptionImpl(Input, alias consume) : ParserExceptionImpl!(Input, consume) {

    mixin parserExeptionCtors;

}
