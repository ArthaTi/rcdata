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
enum stringofEx(foo...) = foo.stringof;

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
