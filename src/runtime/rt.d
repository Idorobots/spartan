import std.stdio;

int main(string[] args) {
    foreach(arg; args[1..$]) {
        writefln("Hello %s!", arg);
    }
    return 0;
}
