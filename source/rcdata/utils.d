module rcdata.utils;

import std.ascii;
import std.stdio;
import std.range;
import std.string;
import std.algorithm;

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
