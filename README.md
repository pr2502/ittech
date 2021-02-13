ittech
======

Impulse Tracker module file parser and writer. Currently still a work in
progress, the parser is already particularly usable however the API is very
unstable and changes with every commit (no version commitment yet). Writer is
planned after the data representation is deemed stable enough.

The [render example] can be already used to play simple module files,
however if it uses any effects it will probably sound really bad.
The [dump example] is useful for demonstrating the current parsing capabilities
of the library.

[render example]: https://github.com/ametisf/ittech/blob/main/examples/render.rs
[dump example]: https://github.com/ametisf/ittech/blob/main/examples/dump.rs

You can run the examples using cargo:

```shell
cargo run --example dump -- module_file.it
cargo run --example render -- module_file.it out.wav
```

The render example might be a bit slow in debug mode, the renderer is very
primitive and unoptimized.


documentation
-------------

One of the main focuses of the library is to document the file format as best
as possible and to point out differences in the multitude of implementations
there are. The implementations which are mainly being used as a reference are
[OpenMPT] and [Schism Tracker]. Schism Tracker aims to be a faithful
reimplementation of the original Impulse Tracker whereas OpenMPT does much
more. If you want to dive into the codebase the Schism Tracker source is much
more readable since it only supports one module format, OpenMPT however in our
opinion produces better sound quality.

[OpenMPT]: https://github.com/OpenMPT/openmpt
[Schism Tracker]: https://github.com/schismtracker/schismtracker


name
----

The name comes from the [ITTECH.TXT] document which defines the structure of
the .IT file format and provides some information about effect handling as
well.

[ITTECH.TXT]: https://github.com/schismtracker/schismtracker/wiki/ITTECH.TXT


license
-------

This software is licensed under the terms of GPLv3 or later.
