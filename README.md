# Strux
Strux is a language created for [COMS W4115 Programming Languages and Translators](http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/index.html) at Columbia University. It is a general purpose language with a focus on data structure visualization, and compiles into LLVM.

A detailed report on Strux is available [here](http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/reports/Strux.pdf).

## Compiling & Running
Strux requires LLVM and its development libraries, the m4 macro preprocessor, opam, and clang. Useful instructions for installing these on your operating system can be found in the MicroC README.

Inside Strux's root folder, type `make`. This first creates the Strux to LLVM compiler, called `strux.native`. It then calls a script, `linkStrux.sh`, that converts the C code to LLVM bytecode.

**N.B.** As of this writing (December 2017), macOS High Sierra introduces a compatibility problem with the LLVM bitreader. We were unable to run Strux on a machine running High Sierra, but earlier versions of macOS should run perfectly. Strux was also tested on Ubuntu 16.04.

### Run Tests
To run Strux's test suite, simply call the test script from the root directory:
```sh
$ ./testall.sh
```
This will iterate through all files in the `tests/` directory, indicate whether they passed, and log their output in case of failure. `testall.sh` is based on the MicroC test script.

### Run a new program
The easiest way to run a new program is to call it via the `testall.sh` script. Although the test script will expect a `.out` file to compare against, the linking will be handled automatically. You can inspect the `<your\_filename>.out` file in the root directory to see what was printed.
```sh
$ ./testall.sh <your_filename>.strux
```

## A simple Strux program
All basic Strux programs include a `main()` function that accepts no arguments and returns 0. They are named `<filename>.strux`. A simple “Hello World” program therefore looks like this:

```
int main() {
    print("Hello, World!");
    return 0;
}
```
