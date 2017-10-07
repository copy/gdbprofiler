GDB profiler
============

This is a fork of Ygrek's [ocaml-gdb](https://github.com/ygrek/ocaml-gdb), a
gdb-based profiler for native OCaml programs. Support for writing cpuprofile
files has been added, which can be viewed in Chromium's DevTools, which can be
opened by pressing F12 or navigating to [this
page](chrome-devtools://devtools/bundled/inspector.html?panel=js_profiler&v8only=1)
(you may need to open [this
page](chrome-devtools://devtools/bundled/inspector.html) first).
It also supports writing callgrind files, which can be viewed using
[kachegrind](https://kcachegrind.github.io/html/Home.html).

Additionally, support for lldb has been added (by passing the `--use-lldb`
command line argument). This is experimental and requires the `lldb-mi`
executable.

gdbprofiler works on OCaml 4.02.3 through 4.06.

Quick start
-----------

gdbprofiler doesn't require instrumentation. Compile your code to native binaries. Optionally, add
`-g` in ocamlc or `-tag debug` in ocamlbuild to see source code locations.

Installation: `opam install gdbprofiler`

Usage: `gdbprofiler -p <pid> [--use-lldb] [--debugger path] [--cpuprofile path] [--callgrind path]`

Example:  ```gdbprofiler -p `pidof my_example_program.native` --cpuprofile example.cpuprofile --callgrind callgrind.out```

If you're getting a "not permitted" error on Linux, run the following: `su -c
'sysctlkernel.yama.ptrace_scope=0'` ([more
infos](https://rajeeshknambiar.wordpress.com/2015/07/16/attaching-debugger-and-ptrace_scope/))

The output file must have a `.cpuprofile` extension, otherwise Chromium refuses to load it.

<hr>

![Bottom Up](https://i.imgur.com/smIR1tZ.png)

Bottom up view in Chromium.

<hr>

![Top Down](https://i.imgur.com/3EA26XM.png)

Top down view in Chromium.

<hr>

![Chart](https://i.imgur.com/8QEV98Y.png)

Chart view in Chromium.

<hr>

![Tree Map](https://i.imgur.com/2d4dvi7.png)

[Tree Map](https://kcachegrind.github.io/html/TreeMap.html) in kcachegrind.

<hr>

![Call Graph](https://i.imgur.com/pr7IDdH.png)

[Call Graph](https://kcachegrind.github.io/html/CallGraph.html) in kcachegrind.


Introduction
------------

RMP is a stack sampling profiler based on ocaml-gdb library for communication
with GDB machine interface (GDB/MI).

Stack sampling profiler works by taking many stack traces of the profiled process
and aggregating similar traces together, providing empirical evidence of program
behaviour with statistical estimation of percentage of time spent in different code paths.
This means, that profiler will show not only cpu-bound hotspots, but also
all places where the code is waiting for the kernel call to finish (e.g. sleep, blocking IO, etc).
One nice property of stack sampling approach is that the profiled program needn't be modified
or restarted - profiler only needs the permission to attach to the running process and collect
stack traces for the short period of time.

See http://poormansprofiler.org/ for general description of this technique.

Rationale
---------

RMP attaches GDB to the profiled program once and then instructs it to probe the stack of the
inferior process periodically. The traces are aggregated and displayed/updated immediately. It has access
to all the information that GDB provides (static stack unwinding, debug symbols, etc). Interactive mode
allows to stop/resume profiling at any time (e.g. when enough information is available or waiting for
the process to engage into some specific activity).

RMP was devised to fill the gap between existing solutions :

* gprof

  Instrumenting profiler, requires recompilation, instrumentation influences code behaviour significantly
  (inflating execution time for small functions).

* pmp

  Poor man's profiler shell script works by calling gdb several times each time attaching to the process,
  collecting trace and detaching. This extremely simple approach works everywhere and yields tons of
  useful information. Unfortunately, it also incurs significant pauses in the profiled process and takes
  quite a bit of time to collect the traces, also doesn't have interactive mode.

* linux perf

  Works only on linux, needs to be compiled with the matching version of kernel headers. Can profile both
  kernel and userland, but only the code that saves frame pointer on the stack. This is true for the majority
  of C code, but doesn't hold for the OCaml native code (in default mode).
