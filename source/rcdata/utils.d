module rcdata.utils;

import std.ascii;
import std.stdio;
import std.range;
import std.string;
import std.algorithm;
import std.exception;


/// Write a hexdump for debugging. Writes to stdout.
void hexDump(ubyte[] bytes, int perLine = 16) {

    hexDump(stdout, bytes, perLine);

}

/// Write a hexdump for debugging.
void hexDump(File file, ubyte[] bytes, int perLine = 16) {


    file.writefln("%s bytes:", bytes.length);
    foreach (value; bytes.chunks(perLine)) {

        const byteContent = format!"%(%0.2x %) "(value);

        file.write(leftJustify(byteContent, perLine * 3));
        file.writefln!"%(%c%)"(value.map!(a => a.isGraphical ? cast(char) a : '.'));

    }

}

/// Stringof improved to better handle callbacks
template stringofone(alias symbol) {

    import std.traits;

    enum string name = mixin(__traits(compiles, symbol.stringof)
        ? q{ symbol.stringof }
        : q{ __traits(identifier, symbol) });


    static if (__traits(compiles, __traits(getLocation, symbol))) {

        enum stringofone = format!"`%s` at %s:%s"(name, __traits(getLocation, symbol)[0..2]);

    }

    else enum stringofone = name;

}

unittest {

    void foo(string x) { }
    alias bar = (string x) { };
    alias baz = (x) { };

}

/// Run a map on a tuple at runtime.
auto tupleMap(alias fun, Args...)(Args args) {

    import std.meta, std.typecons;

    auto mapItem(alias symbol)() {

        return fun(symbol);

    }

    return tuple(staticMap!(mapItem, args));

}

/// Base type for all rcdata exceptions.
abstract class RCDataException : Exception {

    mixin basicExceptionCtors;

}

/// Check if the two objects are equal. Resolve any pointers when comparing. `class`es use regular comparison and
/// respects `opEquals`.
bool equalPtr(T)(T a, T b) pure @safe
out(r) {

    if (!r) {

        import std.stdio;
        debug writefln!"T %s: %s != %s"(T.stringof, a, b);

    }

}
do {

    import std.sumtype;

    // Pointer
    static if (is(T == X*, X)) {

        if (a is b) return true;
        if (a is null || b is null) return false;

        return equalPtr(*a, *b);

    }

    // Array
    else static if (is(T == X[], X)) {

        return equal!equalPtr(a, b);

    }

    // SumType
    else static if (isSumType!T) {

        return match!(
            (suba, subb) {

                static if (is(typeof(suba) == typeof(subb))) {

                    return equalPtr(suba, subb);

                }

                else return false;

            }
        )(a, b);

    }

    // Struct
    else static if (is(T == struct) && !__traits(hasMember, T, "opEquals")) {

        static foreach (i, Item; T.tupleof) {

            if (!equalPtr(a.tupleof[i], b.tupleof[i])) {

                return false;

            }

        }

        return true;

    }

    // Anything else
    else return a == b;


}
