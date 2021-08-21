ittech
======

Impulse Tracker module file parser and writer. Currently still a work in
progress, the parser is already practically usable however the API is still
unstable and may change often. Writer is not implemented yet.

The [render example] can be already used to play IT module files, however the
example itself implements no effects so most tracks will probably sound really
bad.  The [dump example] is useful for demonstrating the current parsing
capabilities of the library.

[render example]: https://github.com/pr2502/ittech/blob/main/examples/render.rs
[dump example]: https://github.com/pr2502/ittech/blob/main/examples/dump.rs

You can run the examples using cargo:

```shell
cargo run --example dump -- module_file.it
cargo run --example render -- module_file.it out.wav
```

The render example might be a bit slow in debug mode, the renderer is very
primitive and unoptimized.


documentation
-------------

Documentation can be seen at [docs.rs/ittech](https://docs.rs/ittech).


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


license
-------

This software is licensed under the terms of GPLv3 or later.
