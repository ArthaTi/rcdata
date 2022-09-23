/// Module for easily making parsers.
///
/// Can be used for both lexing and parsing grammars.
module rcdata.parser;

import std.range;
import std.traits;
import std.exception;

import rcdata.utils;

// TODO for each matcher, write a set of tests with no captures, one capture and multiple captures, mix them all too!


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
/// Params:
///     Input  = Input range used by the parser.
///     supply = Function supplying user data such as tokens.
///     Ts     = Optional types for additional values to hold within the match, used to add result data specific to
///         a matcher. If provided, the match can be implicitly casted to the original match type.
struct MatchImpl(Input, alias supply, Ts...) {

    alias Data = MatchData!(Input, supply);
    alias ParserException = ParserExceptionImpl!(Input, supply);
    alias Match = typeof(this);
    alias BaseMatch = MatchImpl!(Input, supply);

    /// Types of the captured values.
    alias Capture = Ts;

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

    /// Additional data added by a specific matcher function.
    Ts capture;

    // Holding an additional value, make the struct implicitly convert into its base
    static if (Ts.length != 0) {

        alias base this;

    }

    invariant {

        assert(error is null || matched.maxLength == 0, "`matched` must be empty if there's an error");

    }


    /// Match succeeded. Calls `supply`.
    auto this(Input source, size_t consumed, Ts values, string filename = __FILE__, size_t line = __LINE__) {

        this.matched = source.take(consumed);
        this.data = supply(this.matched);
        this.capture = values;

    }

    /// Match succeeded, but pass the data directly instead of calling `supply`. Useful for altering existing matches.
    this(Take!Input matched, Data data, Ts values, string filename = __FILE__, size_t line = __LINE__) nothrow pure @safe @nogc {

        this.matched = matched;
        this.data = data;
        this.capture = values;

    }

    private this(inout typeof(this.tupleof) args) inout nothrow pure @safe @nogc {

        this.tupleof = args;

    }

    static ok(Input source, size_t consumed, Ts values) {

        return Match(source, consumed, values);

    }

    static ok(Take!Input matched, Data data, Ts values) {

        return Match(matched, data, values);

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

        assert(error !is null, "Error for Match.fail must not be null. Note an empty string literal is a valid value.");

        return Match(error, data, source.take(0), Ts.init);

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

    /// Get the base match struct, without capture data.
    inout(BaseMatch) base() inout nothrow pure @safe @nogc {

        return inout BaseMatch(error, data, matched);

    }

    /// Assign a slice of the capture output.
    void captureFrom(size_t index, Ts...)(Ts values) {

        static assert(values.length != 0 || Ts.length == 0);

        this.capture[index .. index + Ts.length] = values;

    }

    /// Check if the match succeeded.
    bool opCast(T : bool)() const nothrow pure @safe @nogc {

        return error is null;

    }

    const toString() {

        import std.conv;
        import std.meta;
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


        static if (Capture.length)
            const joined = ", " ~ only(tupleMap!text(capture).expand).join(", ");
        else
            const joined = "";

        return this
            ? format!"Match(%s, %s%s)"(matchedText, data, joined)
            : format!"Match.fail(%s, %(%s%)%s)"(matchedText, error.only, joined);

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

    import std.string;

    mixin makeParser!(Input, supply, matchText);

    static auto matchText(dstring text)(Input input)
    out (r; text.length == 0 || !r || r.consumed != 0, format!"matchText `%s` consumed nothing"(text))
    do {

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

    import std.meta;
    import std.format;
    import std.range;
    import std.traits;
    import std.sumtype;
    import std.functional;

    import rcdata.utils;
    import rcdata.parser;

    static:

    // Check arguments
    static assert(isInputRange!Input, Input.stringof ~ " isn't an input range");

    alias Match = rcdata.parser.MatchImpl!(Input, supply);
    alias MatchCapture(Ts...) = rcdata.parser.MatchImpl!(Input, supply, Ts);
    alias ParserException = rcdata.parser.ParserExceptionImpl!(Input, supply);


    /// Get an AliasSeq of capture types held by a given list of `Match` and `MatchCapture` types.
    template MatchCaptureTypes(Ts...) {

        alias MatchCaptureTypes = AliasSeq!();

        static foreach (T; Ts) {

            static assert(is(T : Match), T.stringof ~ " is not a Match subtype.");

            // Found a match type
            static if (is(T : Match)) {

                MatchCaptureTypes = AliasSeq!(MatchCaptureTypes, T.Capture);

            }

        }

    }

    /// Check the match type returned by `match` for the given pattern.
    template MatchType(pattern...) {

        static if (pattern.length == 1) {

            alias MatchType = typeof(matchImpl!pattern(Input.init));

        }

        // Note: We return the type of (match!pattern) rather than the tuple of match types.
        else alias MatchType = MatchCapture!(PatternCaptureTypes!pattern);

    }

    /// Get an alias seq of capture types, similarly to `PatternCaptureTypes`, but based on a pattern for `match`.
    alias PatternCaptureTypes(pattern...) = MatchCaptureTypes!(staticMap!(MatchType, pattern));

    /// Iterate on the given pattern tuple.
    struct CaptureTupleIterator(patterns...) {

        static int opApply(scope int delegate(size_t patternIndex, size_t captureIndex) dg) @system {

            size_t patternIndex, captureIndex;

            // Check each item in the pattern
            static foreach (pattern; patterns) {{

                alias T = MatchType!pattern;

                static assert(is(T : Match),
                    pattern.stringof ~ " does not return a valid Match instance but a " ~ T.stringof);

                // Return the pattern and capture indices
                if (auto result = dg(patternIndex, captureIndex)) {

                    return result;

                }

                // Bump them
                patternIndex++;
                captureIndex += T.Capture.length;

            }}

            return 0;

        }

    }

    /// Wrap the tuple in a std.typecons.Tuple if its length isn't 1, otherwise return the sole item.
    template TupleWrap(items...) {

        import std.typecons : Tuple;

        // One item, return it
        static if (items.length == 1) alias TupleWrap = items[0];

        // Different count, wrap in a tuple
        else alias TupleWrap = Tuple!items;

    }


    static assert(is(Match == MatchCapture!()));
    static assert(is(MatchCapture!int == MatchCapture!(MatchCaptureTypes!(Match, MatchCapture!int))));

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
    /// A matcher function will be tried with each of the following:
    ///
    /// * `fun(input, capture)` if the function returns capture data, to pass on data from its previous iterations.
    /// * `fun(input)` to perform a regular match.
    /// * `basicMatcher!fun(input)` as a convenience call for the chosen `basicMatcher`; for example, the default
    ///   `basicMatcher` for any dchar range would allow using strings as match patterns:
    ///   `match!"struct"("struct Foo")`
    /// * If none of those can compile, `match` will output the errors of `fun(input)` and `basicMatcher!fun(input)`.
    ///
    /// Params:
    ///     pattern = Pattern that has to be matched.
    ///     input   = Source to parse.
    ///     context = Optionally, previous match result if used within repetitive matchers. Must be successful.
    template match(pattern...)
    if (is(MatchType!pattern)) {

        import std.typecons;

        // If compatible with the pattern
        alias match = (Input input, MatchType!pattern context = MatchType!pattern.init) {

            assert(context, "Given context match has failed");

            alias Return = MatchType!pattern;

            Input source = input;
            Return result = context;

            // This function cannot match less than matched by the context
            scope (exit) assert(result.consumed >= context.consumed);

            // Evaluate each matcher
            try static foreach (index, captureIndex; CaptureTupleIterator!pattern) {{

                alias Local = MatchType!(pattern[index]);

                // Get the last capture for this matcher, if any
                auto lastCapture = result.capture[captureIndex..captureIndex + Local.Capture.length];

                // Try the match
                Local local = matchImpl!(pattern[index])(source, lastCapture);

                // Combine data
                auto data = supply(result.data, local.data);

                // If the match failed
                if (!local) {

                    // Return its match with updated data
                    return Return.fail(local.matched.source, local.error, data);

                }

                // Success, expand the match to contain the full result
                result.matched = result.matched.source.take(result.consumed + local.consumed);

                // Add user data
                result.data = data;
                result.capture[captureIndex .. captureIndex + Local.Capture.length] = local.capture;

                assert(result);

                // Advance the input range
                source.popFrontN(local.consumed);

            }}

            // Expand any caught critical exceptions with context info
            catch (ParserException exc) {

                throw exc.extend(result.data);

            }

            return result;

        };

    }

    // Try to alias to std.sumtype.match to help with namespace clash
    template match(handlers...)
    if (!is(MatchType!handlers)) {

        alias match = std.sumtype.match!handlers;

    }

    /// Try the overloads of the function to perform the match.
    private template matchImpl(alias fun) {

        // Overload 1: regular match
        alias matchImpl = (Input input) {

            // Try to run the matcher
            static if (is(typeof(fun(input)) : Match)) {

                return fun(input);

            }

            // It's not callable. But maybe basicMatcher can handle it?
            else static if (is(typeof(basicMatcher!fun(input)) : Match)) {

                // Alias to basicMatcher
                return basicMatcher!fun(input);

            }

            // Try both to see what the errors are
            else {

                Match local = fun(input);
                local = basicMatcher!fun(input);

            }

            assert(false);

        };

        alias Capture = typeof(matchImpl(Input.init)).Capture;

        // Overload 2, if no.1 has a capture, pass the previous capture data into it, to allow things like matchRepeat
        // keep context.
        static if (Capture.length != 0)
        alias matchImpl = (Input input, Capture capture) {

            alias stringofone(funs...) = funs.stringof;

            // Special overload supported
            static if (is(typeof(fun(input, capture)) : Match)) {

                static assert(is(typeof(fun(input, capture)) == typeof(fun(input))),
                    format!("`%s %s(input, capture)` return type doesn't match the one of `%s %2$s(input)`")
                           (typeof(fun(input, capture)).stringof, stringofone!fun, typeof(fun(input)).stringof));

                return fun(input, capture);

            }

            // Run the default overload otherwise
            else return matchImpl(input);

        };

    }

    /// Match one of the given items.
    alias matchOr(pattern...) = (Input input) {

        alias ReturnMatch = MatchOr!pattern;

        // Evaluate each matcher
        foreach (fun; pattern) {

            // Return the result of the first one succeeding
            if (auto result = match!fun(input)) {

                // Cast the capture to our expected type
                ReturnMatch.Capture capture = result.capture;

                return ReturnMatch(result.matched, result.data, capture);

            }

        }

        return ReturnMatch.fail(input, "No match found for matchOr!(" ~ pattern.stringof ~ ")");

    };

    /// Return the `matchOr` return value for the given pattern.
    template MatchOr(pattern...) {

        import std.meta;
        import std.typecons : Tuple;

        // Collect possible values for the capture.
        alias Types = AliasSeq!();

        // Check each pattern
        static foreach (fun; pattern) {

            // Add it to the type list, wrap in a tuple if item count != 1
            Types = AliasSeq!(Types, TupleWrap!(MatchType!fun.Capture));

        }

        // Get rid of duplicates
        Types = NoDuplicates!Types;


        // No types, return a plain match
        static if (Types.length == 0 || is(Types[0] == Tuple!()))
            alias MatchOr = Match;

        // One type only, return it
        else static if (Types.length == 1)
            alias MatchOr = MatchCapture!Types;

        // Multiple types, make it a sum type
        else alias MatchOr = MatchCapture!(SumType!Types);

    }

    /// Repeat the token sequence (as in `match`) zero to infinity times
    alias matchRepeat(pattern...) = (Input input, MatchType!pattern context = MatchType!pattern.init)

        => matchRepeatMin!(0, pattern)(input, context);


    /// Repeat the token sequence at least once.
    alias matchRepeatMinOnce(pattern...) = (Input input, MatchType!pattern context = MatchType!pattern.init)

        => matchRepeatMin!(1, pattern)(input, context);


    alias matchRepeatMin(size_t minMatches, pattern...) = (Input input,
        MatchType!pattern context = MatchType!pattern.init)

        => matchRepeatRange!pattern(input, context, minMatches);


    /// Repeat the token sequence (as in match) `minMatches` to `maxMatches` times
    alias matchRepeatRange(size_t minMatches, size_t maxMatches, pattern...) = (Input input,
        MatchType!pattern context = MatchType!pattern.init)

        => matchRepeatRange!pattern(input, context, minMatches, maxMatches);


    /// ditto
    template matchRepeatRange(pattern...)
    if (!is(typeof(pattern[0]) : size_t)) {

        alias matchRepeatRange = (Input input, MatchType!pattern context = MatchType!pattern.init,
            size_t minMatches = 0, size_t maxMatches = size_t.max)
        {

            size_t matches;

            while (true) {

                MatchType!pattern local;

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
                ? context.fail(input, "matchRepeat didn't match enough times")
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

            MatchType!pattern context;

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
    /// Returns: `Match` or if the value contains captures, `MatchCapture!(Nullable!Match)`.
    alias matchOptional(pattern...) = (Input input) {

        import std.typecons;

        // Match the token
        auto ret = match!pattern(input);

        // No capture
        static if (ret.Capture.length == 0) {

            return ret
                ? ret
                : Match.ok(input, 0);

        }

        // Capture on, add a nullable value
        else {

            alias Capture = Nullable!(TupleWrap!(ret.Capture));
            alias Return = MatchCapture!Capture;

            return ret
                ? Return(ret.matched, ret.data, Capture(ret.capture))
                : Return(input, 0, Capture());

        }

    };

    /// Construct `T` by matching given `pattern`.
    ///
    /// Returns: `MatchCapture!T` holding the constructed value.
    alias matchCapture(T, pattern...) = (Input input, T value = T.init) {

        // This function is an ugly mess. I don't blame myself. I don't see how it could've been done better.

        alias Result = MatchCapture!T;

        Input source = input;
        auto result = Result(input, 0, value);

        /// Union to hold results of each pattern match
        union LastResult {

            // Create storage for each value
            static foreach (ptrdiff_t i, fun; pattern) {

                static if (!is(ByIndex!i == void))
                mixin("ByIndex!i c", i, ";");

                else mixin("typeof(null) c", i, ";");

            }

            /// Type of the value at given index.
            template ByIndex(ptrdiff_t i) {

                // Defined
                static if (__traits(compiles, mixin("c", i))) alias ByIndex = typeof(byIndex!i);

                // Not defined yet
                else {

                    alias ByIndex = typeof(
                        matchCaptureImpl!(T, pattern[i], typeof(byIndex!(i-1)()))(source, result.capture, byIndex!(i-1))
                    );

                }

            }

            /// Get a stored value by its index.
            auto ref byIndex(ptrdiff_t i)() @trusted {

                static if (i < 0) return null;
                else return mixin("c", i);

            }

        }

        LastResult lastResult;

        /// Run the rule at given index. Does not store the result.
        auto run(ptrdiff_t i)(LastResult lastResult, Input input, ref Result match) {

            // This must have no context other than pattern[i]

            auto last = lastResult.byIndex!(i-1);

            // Run this rule
            return matchCaptureImpl!(T, pattern[i], typeof(last))(input, match.capture, last);

        }

        import core.lifetime;

        // Evaluate each rule
        try static foreach (i, fun; pattern) {{

            // If this is a matcher
            static if (is(lastResult.ByIndex!i : Match)) {

                // Run the rule
                auto local = run!i(lastResult, source, result);

                // Combine data
                auto data = supply(result.data, local.data);

                // Match failed
                if (!local) {

                    return Result.fail(local.matched.source, local.error, data);

                }

                // Success
                result = Result(
                    result.matched.source.take(result.consumed + local.consumed),
                    data,
                    result.capture,
                );

                // Advance the input range
                source.popFrontN(local.consumed);

                // Move into the register
                move(local, lastResult.byIndex!i());

            }

            // Void
            else static if (is(lastResult.ByIndex!i == void)) {

                run!i(lastResult, source, result);

            }

            // Non-void, run the function and read the value
            else lastResult.byIndex!i = run!i(lastResult, source, result);

        }}

        // Expand any caught exceptions with context info
        catch (ParserException exc) {

            throw exc.extend(result.data);

        }

        return result;

    };

    private alias matchCaptureImpl(T, alias fun, LastResult) = (Input input, ref T value, LastResult lastResult) {

        // TODO maybe this function should unconditionally return a match?

        // Got a match
        static if (is(LastResult : Match)) {

            auto capture = lastResult.capture;

        }


        // Option 1, (LastResult, ref T)
        static if (__traits(compiles, fun(lastResult, value))) {

            return fun(lastResult, value);

        }

        // Option 2, (LastLastResult.capture, ref T)
        else static if (__traits(compiles, fun(capture, value))) {

            return fun(capture, value);

        }

        // Option 3, matchImpl(input)
        else static if (__traits(compiles, matchImpl!fun(input))) {

            return matchImpl!fun(input);

        }

        else {

            fun(capture, value);
            static assert(false, stringofone!fun ~ " is not a valid matchCapture predicate; "
                ~ "LastResult = `" ~ LastResult.stringof ~ "`");

        }

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
            : result.fail(input, message);

    };

    /// Check if the pattern matches, but do not supply it.
    alias lookAhead(pattern...) = (Input input) {

        auto result = match!pattern(input);

        // Return empty match on success, the result if failed
        return result
            ? result.ok(input, 0, result.capture)
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
