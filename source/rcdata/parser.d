/// Module for easily making parsers.
///
/// Can be used for both lexing and parsing grammars.
module rcdata.parser;

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
if (isSomeString!Input) {

    mixin makeParser!(Input, consume, matchText);

    static auto matchText(dstring text)(Input input) {

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

}

/// Mixin to produce matcher templates for processing `Input` input range and creating an `Output` output range.
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

            throw new ParserException(msg, input);

        }

    }

    class ParserException : RCDataException {

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

        /// Extend the exception with data about the parent context.
        typeof(this) extend()(Output context) {

            this.context = consume(context, this.context);
            return this;

        }

    }

    /// Remove content of this parser result and merge it into a single one.
    Output dropContent()(Output tokens) {

        return [OutputItem.make(tokens.consumed, Input.init)];

    }

    /// Match given rules but drop their content.
    Output matchDrop(funs...)(Input input) {

        return dropContent(match!funs(input));

    }


    /// Match anything.
    Output matchAny()(Input input) {

        return matchAny!((InputItem item) => true)(input);

    }


    /// Match one single item if `fun` returns `true`.
    Output matchAny(alias fun)(Input input) {

        alias funCC = unaryFun!fun;

        enforceX(!input.empty, "Unexpected end of file", input);

        // Get the next item
        InputItem next = input.front;

        enforceX(funCC(next), "Delegate didn't match the element", input);

        return consume(1, input.take(1));

    }

    /// Match a sequence of tokens.
    Output match(funs...)(Input input) {

        Output result;
        Input context = input;

        // Evaluate each matcher
        try static foreach (i, fun; funs) {{

            Output local;

            // Try to run the matcher
            static if (__traits(compiles, local = fun(context))) {

                local = fun(context);

                static assert(is(typeof(local) : Output),
                    fun.stringof ~ " return type "
                    ~ typeof(local).stringof ~ " doesn't match "
                    ~ Output.stringof);

            }

            // It's not callable. But maybe basicMatcher can handle it?
            else {

                // Alias to basicMatcher
                local = basicMatcher!fun(context);

                static assert(is(typeof(local) : Output),
                    basicMatcher.stringof ~ " return type "
                    ~ typeof(local).stringof ~ " doesn't match "
                    ~ Output.stringof);

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

    }


    /// Match one of the given tokens.
    Output matchOr(funs...)(Input input) {

        // Evaluate each matcher
        foreach (fun; funs) {

            // Return the result of the first one succeeding
            try return match!fun(input);

            // Failed parsing? That's ok, we'll try the next
            catch (ParserException) { }

        }

        throw new ParserException(format!"None of the tokens matched: %s"(funs.stringof), input);

    }

    /// Repeat the token sequence (as in `match`) zero to infinity times
    Output matchRepeat(funs...)(Input input) {

        return matchRepeatMin!(0, funs)(input);

    }

    /// Repeat the token sequence at least once.
    Output matchRepeatMinOnce(funs...)(Input input) {

        return matchRepeatMin!(1, funs)(input);

    }

    /// Repeat the token sequence (as in match) `minMatches` to infinity times
    Output matchRepeatMin(size_t minMatches, funs...)(Input input) {

        Output result;
        size_t matches;

        while (true) {

            Output local;

            // Match the token
            try local = match!funs(input);

            // Stop if complete
            catch (ParserException) break;

            // Check the length
            const length = local.consumed;

            // Add the result
            matches++;
            result = consume(result, local);

            // Advance the input
            input.popFrontN(length);

            // Special case: Match is empty, stop to prevent loops
            if (length == 0) break;

        }

        // Didn't match enough
        if (matches < minMatches) {

            throw new ParserException(
                format!"matchRepeatMin matched %s times, expected %s"(matches, minMatches),
                result, input,
            );

        }

        return result;

    }


    /// Match anything until the given token. Does NOT match the terminator.
    Output matchUntil(alias terminator)(Input input) {

        return matchUntil!(terminator, matchAny!())(input);

    }

    /// Repeat the sequence until another token matches. Does NOT match the terminator.
    Output matchUntil(alias terminator, funs...)(Input input)
    if (funs.length != 0) {

        Output result;

        try while (true) {

            // Match the terminator
            try {

                match!terminator(input);
                break;

            }

            // Proceed like normal if not matched
            catch (ParserException) { }


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

    }

    /// Match zero or one instances of a token.
    Output matchOptional(funs...)(Input input) {

        // Match the token
        try return match!funs(input);

        // Ignore failures
        catch (ParserException) { }

        return consume(0, Input.init);

    }

    /// Require the given rule to match, otherwise throw given exception.
    Output matchCritical(Exc : Throwable, funs...)(Input input) {

        return matchCritical!(new Exc("matchCritical failure!"), funs)(input);

    }

    Output matchCritical(Throwable instance, funs...)(Input input) {

        // Try to match
        try return match!funs(input);

        // Failed parsing
        catch (ParserException) {

            throw instance;

        }

    }

    /// Check if the pattern matches, but do not consume it.
    Output lookAhead(funs...)(Input input) {

        match!funs(input);
        return [];

    }

    /// Fail if the pattern matches. Do not consume anything.
    Output failAhead(funs...)(Input input) {

        try cast(void) match!funs(input);

        // Failed as expected, good
        catch (ParserException) return [];

        throw new ParserException(format!"Unexpected %s"(funs.stringof), input);

    }

    /// Never matches.
    Output matchNever(string msg)(Input input) pure @safe {

        throw new ParserException(msg, input);

    }

}
