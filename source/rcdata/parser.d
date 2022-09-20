/// Module for easily making parsers.
///
/// Can be used for both lexing and parsing grammars.
module rcdata.parser;

import std.range;
import std.traits;
import std.exception;

import rcdata.utils;

/// Check if the given function is a valid parser data supplier.
enum isMatchDataSupplier(Input, alias supply) = __traits(compiles, MatchData!(Input, supply));

/// Check if given struct has a default constructor. True for other types.
enum hasDefaultConstructor(T) = __traits(compiles, {
    T t;
});

/// Get the result type of the given supplier.
template MatchData(Input, alias supply) {

    import std.range;

    auto supplierResultA() { return supply(Input.init.take(1)); }
    auto supplierResultB() { return supply(supplierResultA, supplierResultA); }

    alias SupplierResultA = typeof(supplierResultA());
    alias SupplierResultB = typeof(supplierResultB());

    alias MatchData = CommonType!(SupplierResultA, SupplierResultB);

    static assert(!is(MatchData == void),
        "Return types of supply(" ~ Take!Input.stringof ~ ") (" ~ SupplierResultA.stringof ~ ")"
            ~ " and supply(supply(...), supply(...)) (" ~ SupplierResultB.stringof ~ ")"
            ~ " are not compatible");

}

/// Result of the match.
struct MatchImpl(Input, alias supply) {

    alias Match = typeof(this);
    alias Data = MatchData!(Input, supply);
    alias ParserException = ParserExceptionImpl!(Input, supply);

    static assert(hasDefaultConstructor!Data, Data.stringof ~ " must support default construction");
    static assert(hasDefaultConstructor!(Take!Input), Take!Input.stringof ~ " must support default construction");


    /// If **not null**, the match failed with this message. Empty, but non-null, strings also count as successful
    /// matches. Use the Match directly as a boolean to see.
    string error;

    /// Result of the `supply` function.
    Data data;

    /// Source matched by the result. In case of failure, this is an empty range, but `matched.source` can still be
    /// used, and will contain the source range starting at the point of the match start.
    Take!Input matched;


    /// Match succeeded. Calls `supply`.
    auto this(Input source, size_t consumed) {

        this.matched = source.take(consumed);
        this.data = supply(this.matched);

    }

    /// Match succeeded, but pass the data directly instead of `supply`. Useful for altering existing matches.
    this(Take!Input matched, Data data) nothrow pure @safe @nogc {

        this.matched = matched;
        this.data = data;

    }

    private this(typeof(this.tupleof) args) nothrow pure @safe @nogc{

        this.tupleof = args;

    }

    /// Match failed.
    ///
    /// It's recommended not to use runtime formatted strings for match errors, to avoid constant GC allocations during
    /// runtime. Formatting should be limited to critical errors, see `matchCritical`.
    ///
    /// Params:
    ///     source = Source that failed to match.
    ///     error  = Error message, a reason why the match failed.
    ///     data   = Optionally, context data for the failure, i.e. result data that matched successfully right before.
    static Match fail(Input source, string error, Data data = Data.init) nothrow pure @safe @nogc {

        return Match(error, data, source.take(0));

    }

    /// Get the data of the match. Throws `ParserException` if the match has failed.
    inout(Data) tryData() inout pure @safe {

        // Throw the exception on error
        if (error) throw new ParserException(error, matched.source);

        // Return the data otherwise
        else return data;

    }

    /// Number of source elements consumed.
    size_t consumed() const nothrow pure @safe @nogc {

        return matched.maxLength;

    }

    /// Check if the match succeeded.
    bool opCast(T : bool)() const nothrow pure @safe @nogc {

        return error is null;

    }

    const toString() {

        import std.conv;
        import std.format;

        enum maxLength = 32;

        // Copy the match range into scope so we can iterate over it
        Take!Input matched = matched;

        // Get the first characters from it
        const matchedArr = this
            ? matched.take(maxLength+1).array
            : matched.source.take(maxLength+1).array;

        string matchedText;

        // Include an ellipsis in the format data if the match is longer than length limit
        if (matchedArr.length == maxLength+1) {

            // Include length of the original range if available
            static if (hasLength!(Take!Input))
                matchedText = format!"%(%s%)... %s"(matchedArr[0..$-1].only, matchedArr.length);
            else
                matchedText = format!"%(%s%)..."(matchedArr[0..$-1].only);

        }

        // Match fits, include the full text
        else matchedText = format!"%(%s%)"(matchedArr.only);

        return this
            ? format!"Match(%s, %s)"(matchedText, data)
            : format!"Match.fail(%s, %s)"(matchedText, error.only);

    }

}

unittest {

    import std.conv;

    template supply() {

        string[] supply(Take!string input) {
            return [input.to!string];
        }

        string[] supply(string[] a, string[] b) {
            return a ~ b;
        }

    }

    static assert(isMatchDataSupplier!(string, supply));

}

/// Mixin to produce matcher templates for processing `Input` input range and creating a `Match` output range.
///
/// The parser requires a function with two overloads called `supply`. Both are expected to output a user data type for
/// storing the parsing result (eg. a list of tokens). The first one should take a slice of the input range as
/// a `std.range.Take!Input` and will be called for any successful match. The second should take two instances of the
/// data type and combine them into once.
///
/// A basic implementation of a supply string (with `string` as the input range) would be this:
///
/// ---
/// string[] supply(Take!string input) {
///     return [input.to!string];
/// }
///
/// string[] supply(string[] a, string[] b) {
///     return a ~ b;
/// }
/// ---
///
/// Notes:
///
/// * Performance of the parser heavily depends on `std.range.popFrontN`, so it's the fastest if the range supports
///   slicing and has length. Alternatively, the range may define a `popFrontN` method itself.
/// * The mixin can work both in module scope and at `struct`/`class` scope. No context is required for the matcher
///   functions to work.
///
///   Internally the template only defines aliases to closures rather than functions. This allows them to
///   work without an instance in structs and classes, but they may still use context for template parameters.
mixin template makeParser(Input, alias supply)
if (is(ElementType!Input : dchar)) {

    mixin makeParser!(Input, supply, matchText);

    static auto matchText(dstring text)(Input input) {

        import std.range;
        import std.string;
        import std.exception;

        import rcdata.utils;

        // Match EOF
        static if (text.length == 0) {

            return input.empty
                ? Match(input, 0)
                : Match.fail(input, "Expected end of file");

        }

        // Match the text
        else return input.startsWith(text)
            ? Match(input, text.length)
            : Match.fail(input, "Couldn't match");

    }

}

/// ditto
mixin template makeParser(Input, alias supply, alias basicMatcher) {

    import std.format;
    import std.range;
    import std.traits;
    import std.functional;

    import rcdata.utils;
    import rcdata.parser;

    static:

    // Check arguments
    static assert(isInputRange!Input, Input.stringof ~ " isn't an input range");

    alias Match = rcdata.parser.MatchImpl!(Input, supply);
    alias ParserException = rcdata.parser.ParserExceptionImpl!(Input, supply);

    /// Match anything.
    alias matchAny() = (Input input)

        => matchAny!((ElementType!Input item) => true)(input);


    /// Match one single item if `fun` returns `true`.
    alias matchAny(alias fun) = (Input input) {

        alias funCC = unaryFun!fun;

        // Check for EOF
        return input.empty ? Match.fail(input, "Unexpected end of file")

            // Check if the function matches
            : funCC(input.front) ? Match(input, 1)

            // Fail if it doesn't
            : Match.fail(input, "Delegate didn't match the element");

    };

    /// Match multiple items in order. Each matcher in the pattern will be tested against the source in order, and will
    /// advance the range before the next item in the pattern. The full pattern has to match.
    ///
    /// `match` is a building block for any other pattern matching function. It will automatically call the proper
    /// matcher (either the given matcher function, or `basicMatcher` as a fallback for convenience), and it can be used
    /// to build any other sequential patterns (such as `matchRepeat`) by utilising the `context` parameter.
    ///
    /// Params:
    ///     pattern = Pattern that has to be matched.
    ///     input   = Source to parse.
    ///     context = Optionally, previous match result. Must be successful.
    alias match(pattern...) = (Input input, Match context = Match.init) {

        assert(context, "Given context match has failed");

        Input source = input;
        Match result = context;

        // This function cannot match less than matched by the context
        scope (exit) assert(result.consumed >= context.consumed);

        // Evaluate each matcher
        try static foreach (i, fun; pattern) {{

            Match local;

            // Try to run the matcher
            static if (__traits(compiles, local = fun(source))) {

                local = fun(source);

            }

            // It's not callable. But maybe basicMatcher can handle it?
            else static if (__traits(compiles, local = basicMatcher!fun(source))) {

                // Alias to basicMatcher
                local = basicMatcher!fun(source);

            }

            // Try both to see what the errors are
            else {

                local = fun(source);
                local = basicMatcher!fun(source);

            }


            // Combine data
            auto data = supply(result.data, local.data);

            // If the match failed
            if (!local) {

                // Return its match with updated data
                return Match.fail(local.matched.source, local.error, data);

            }

            // Success, expand the match to contain the full result
            result = Match(
                result.matched.source.take(result.consumed + local.consumed),
                data,
            );

            // Advance the input range
            source.popFrontN(local.consumed);

        }}

        // Expand any caught critical exceptions with context info
        catch (ParserException exc) {

            throw exc.extend(result.data);

        }

        return result;

    };


    /// Match one of the given items.
    alias matchOr(pattern...) = (Input input) {

        // Evaluate each matcher
        foreach (fun; pattern) {

            // Return the result of the first one succeeding
            if (auto result = match!fun(input)) {

                return result;

            }

        }

        return Match.fail(input, "No match found for matchOr!(" ~ pattern.stringof ~ ")");

    };

    /// Repeat the token sequence (as in `match`) zero to infinity times
    alias matchRepeat(pattern...) = (Input input)

        => matchRepeatMin!(0, pattern)(input);


    /// Repeat the token sequence at least once.
    alias matchRepeatMinOnce(pattern...) = (Input input)

        => matchRepeatMin!(1, pattern)(input);


    alias matchRepeatMin(size_t minMatches, pattern...) = (Input input)

        => matchRepeatRange!pattern(input, minMatches);


    /// Repeat the token sequence (as in match) `minMatches` to `maxMatches` times
    alias matchRepeatRange(size_t minMatches, size_t maxMatches, pattern...) = (Input input)

        => matchRepeatRange!pattern(input, minMatches, maxMatches);


    /// ditto
    template matchRepeatRange(pattern...)
    if (!is(typeof(pattern[0]) : size_t)) {

        alias matchRepeatRange = (Input input, size_t minMatches, size_t maxMatches = size_t.max) {

            Match context;
            size_t matches;

            while (true) {

                Match local;

                // Match the token
                local = match!pattern(input, context);

                // Stop if match failed, we don't care about it
                if (!local) break;

                // Count the match
                matches++;

                const length = local.consumed - context.consumed;

                // Advance the input
                input.popFrontN(length);
                context = local;

                // Special case: Match is empty, stop to prevent loops
                if (length == 0) break;

                // Stop if matched enough
                if (matches == maxMatches) break;

            }

            // Check if matched enough times
            return matches < minMatches
                ? Match.fail(input, "matchRepeat didn't match enough times")
                : context;

        };

    }

    /// Repeat the sequence until another token matches. Does NOT match the terminator.
    ///
    /// If the pattern fails, the failure will be propagated, and `matchUntil` will also report failure.
    ///
    /// Params:
    ///     terminator = Matcher to stop the loop when successful.
    ///     pattern    = Pattern to match. Defaults to `matchAny`.
    alias matchUntil(alias terminator) = (Input input)

        => matchUntil!(terminator, matchAny!())(input);


    /// ditto
    template matchUntil(alias terminator, pattern...)
    if (pattern.length != 0) {

        alias matchUntil = (Input input) {

            Match context;

            while (true) {

                // Match the terminator
                if (match!terminator(input, context)) break;

                // Match the token
                auto local = match!pattern(input, context);

                // Propagate failures
                if (!local) return local;

                // Count consumed tokens
                const length = local.consumed - context.consumed;

                // Special case: Match is empty, stop to prevent loops
                if (length == 0) break;

                // Advance the range
                input.popFrontN(length);
                context = local;

            }

            return context;

        };

    }

    /// Match zero or one instances of a token.
    alias matchOptional(pattern...) = (Input input) {

        // Match the token
        if (auto ret = match!pattern(input)) {

            return ret;

        }

        // Ignore failures
        else return Match(input, 0);

    };

    /// Require the given rule to match, otherwise throw given exception.
    /// Params:
    ///     Exc = Exception type to instantiate and throw. Will be passed a message string and, if supported, input
    ///         range of the following source text. If omitted, but `message` is given, throws `ParserException`.
    ///     message = Message of the exception to throw.
    ///     instance = Already constructed instance of an exception to throw.
    ///     pattern = Pattern to match. If empty, trying to match this pattern will result with the exception.
    alias matchCritical(Exc : Throwable, string message, pattern...) = (Input input) {

        // Exception accepting source data
        static if (__traits(compiles, new Exc(message, input))) {

            return matchCriticalImpl!pattern(input, new Exc(message, input));

        }

        // Regular exception
        else return matchCriticalImpl!pattern(input, new Exc(message));

    };

    /// ditto
    alias matchCritical(string message, pattern...) = (Input input)

        => matchCritical!(ParserException, message, pattern)(input);


    /// ditto
    alias matchCritical(Throwable instance, pattern...) = (Input input)

        => matchCriticalImpl!pattern(input, instance);


    /// ditto
    alias matchCriticalImpl(pattern...) = (Input input, Throwable instance) {

        // If there's a sequence to check
        static if (pattern.length) {

            // Try to match
            if (auto result = match!pattern(input)) {

                return result;

            }

            // Match failed, throw the chosen exception
            else throw instance;

        }

        // Just throw on encounter
        else {

            // Let the compiler infer the return type
            if (false) return match!pattern(input);

            throw instance;

        }

    };

    /// Adjust failure message of the given pattern.
    alias matchFailMessage(string message, pattern...) = (Input input) {

        auto result = match!pattern(input);

        return result
            ? result
            : Match.fail(input, message);

    };

    /// Check if the pattern matches, but do not supply it.
    alias lookAhead(pattern...) = (Input input) {

        auto result = match!pattern(input);

        // Return empty match on success, the result if failed
        return result
            ? Match(input, 0)
            : result;

    };

    /// Fail if the pattern matches. Succeeds if the rule fails. Doesn't supply anything.
    alias failAhead(pattern...) = (Input input) {

        return match!pattern(input)
            ? Match.fail(input, "Rule matched but shouldn't have")
            : Match(input, 0);

    };

    /// Never matches.
    alias matchNever(string msg) = (Input input) {

        return Match.fail(input, msg);

    };

}

/// Exception type thrown on a parser failure, usually `matchCritical`.
class ParserExceptionImpl(Input, alias supply) : RCDataException {

    import std.range;

    static assert(isInputRange!Input, Input.stringof ~ " isn't an input range");

    alias Match = MatchData!(Input, supply);

    Match context;
    Input source;

    this(string msg, Input source, string filename = __FILE__, size_t line = __LINE__) pure @safe {

        super(msg, filename, line);
        this.source = source;

    }

    this(string msg, Match context, Input source, string filename = __FILE__, size_t line = __LINE__) pure @safe {

        this(msg, source, filename, line);
        this.context = context;

    }

    mixin template parserExeptionCtors() {

        this(string msg, Input source, string filename = __FILE__, size_t line = __LINE__) pure @safe {

            super(msg, source, filename, line);

        }

        this(string msg, Match context, Input source, string filename = __FILE__, size_t line = __LINE__)
            pure @safe
        do {
            super(msg, context, source, filename, line);
        }

    }

    /// Extend the exception with data about the parent context.
    auto extend(Match context) {

        this.context = supply(context, this.context);
        return this;

    }

}
