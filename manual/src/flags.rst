Invoking the Compiler
#####################

.. warning::

    The features of ``cryptol-compiler`` are currently early in
    development and as such are incomplete and subject to change.


Running the Compiler
====================

Generating Examples
-------------------

The default mode of the compiler is to generate a crate that compiles a ``cryptol``
single expression (the cryptol value named `main`) and produces a Rust crate that computes
this value and prints it to stdout.  This is used for testing/demonstration purposes.

Suppose we have a file named ``simple.cry`` with the contents:

.. code-block:: cryptol

    main = 0x10 + 0x01

We can invoke ``cryptol-compiler`` on it to produce a crate:

.. code-block:: bash

    cryptol-compiler --crate=simple --output=simple_out simple.cry


Which will produce a crate named ``simple`` in the ``simple_out`` subdirectory
of the current directory that can be built and run using ``cargo``:

.. code-block:: bash

    cd simple_out
    cargo run

Which, after building, prints the value ``0x11`` to the console.


Flags
=====

.. data:: --help

    Display help for compiler flags.


.. data:: --output=PATH, -o

    Specifies the output directory for the code the compiler generates.  Defaults to ``cry-rust``
    in the current working directory.


.. data:: --crate=NAME

    Specifies the name of the generated rust crate.  Defaults to ``cry-rust``.


.. data:: --entry-module=MODULE

    By default the compiler generates only dependencies needed to support evaluating
    the cryptol value ``main`` in the generated crate.  The ``entry-module`` flag
    also causes the compiler to generate the functions in the specified module along
    with any needed dependencies.



Debugging Flags
===============

These flags are mainly useful for compiler development and will likely not be useful to a typical user.

.. data:: --dbg-list-primitives

    List declared primitives.

.. data:: --enable-warnings, -w

    Enable output (to stderr) of ``cryptol`` compilation warnings during compilation.
