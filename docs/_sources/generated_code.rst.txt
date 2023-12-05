Generated Code
==============

This section outlines how Cryptol specifications are mapped to Rust code.

.. warning::

   This is an early version of the compiler, so the information here is
   likely to change!


Modules
-------

At present the compiler will generate a Rust crate for a Cryptol specification.
The crate will contain a library, with a single Rust module for each
top-level Cryptol module in the original specification.
The names of the Rust modules are derived from the Cryptol ones,
escaping special characters, and converting the result to snake case.

At present we do not generate nested Rust modules: instead all modules
reside at the top level of a crate, and Cryptol modules in
sub-directories (e..g, ``A::B``) get names like ``a_colon_colon_b``.
This is likely to change, as these names are quite long and not very
nice to work with.

Cryptol also supports sub-modules, but these do not manifest in the generated
Rust code.  Instead, declarations in sub-modules become part of the top-level
modules that contain them.   We take care to avoid name collisions due to
such flattening.  In future versions of the compiler, we may change the
names that we choose for declarations in nested modules.


Declarations
------------

At present some of the modules in a Cryptol specification are considered to
be the *entry points* for the specification (specified via a compiler flag).
We only generate code for the public entries of these modules, and any other
declarations that are required by the public entry points (transitively).

Each Cryptol declaration is compiled to one or more Rust declaration with
names derived from the Cryptol specification.  Monomorphic Cryptol functions
are mapped to a single Rust function, however, polymorphic ones will result
in multiple functions, depending on the number of *specializations*
the compiler chooses for a function.   Each specialization allows calling
the Cryptol function using a different representation for the arguments.

At present, specializations may vary in the following dimensions:

  * representation of numeric type parameters, and
  * representation of sequences.

Cryptol has two kinds of type arguments, numeric ones (kind ``#``) and
value ones (kind ``*``).  Parameters of kind ``*`` get mapped to Rust generics,
while parameters of kind ``#`` are mapped to concrete value parameters.
The compiler performs an analysis to choose the representation for the
additional ``#`` parameters:  it will either use ``usize`` or an unbounded
unsigned integer, depending on an analysis that determines the possible
values for the parameter (we assume that sequences never have sizes that
may exceed ``usize``).   Functions with unbounded numeric parameters may
get different specializations---one for when they are applied with a smaller
parameter, and one when they are applied to a really large one.  Finally,
numeric parameters that may be infinite always result in a separate
specialization where the parameter is not explicitly passed in, but rather
is assumed to be ``inf`` in the implementation of the specialization.

We also generate specializations for Cryptol functions that manipulate
sequences.  At present, we support three representations of sequences:
``dword``, which is used for finite sequences of bits, ``Vec``, which is used
for other finite sequences, and *streams* which correspond to Rust's iterators,
and are presently used for infinite streams.  Choosing the correct
representation for a sequence can have a big impact on the performance
on the generated code.  At present, we employ a fairly simple algorithm,
that generates instantiations based on the types of a function---one for
each possible case.  There is a lot more work that can be done here, and
future versions of the compiler are likely to support both more sequence
representations (e.g., use specialized types when the sizes of a sequence
is statically known, which is *very* common), and also a mechanism where
users can give hints to the compiler on what specializations to generate.

In addition to the parameters corresponding to ``#`` types,
the Rust implementations of some functions may also get some additional
*length* parameters.  These are needed for polymorphic functions that may
create values of different lengths.  For example, consider Cryptol's
``zero`` function, which has the following type: ``{a} Zero a => a``.
We can use this function to create values of all kinds of types, including
sequences of various lengths.  So when we call this function in Rust,
we need to specify the *length* for the value we want to create.  For
this reason, the generated Rust function for ``zero`` has a type like this:

.. code::

  zero<A: Zero>(len: A::Length) -> A { ... }

For types that do need a length, ``A::Length`` is simply defined to be
a dummy ``()`` parameter, but for a type such as ``dword`` it will be
``usize``, and for a vector it will be ``(usize, T::Length)`` where the
first element of the pair is the length of the vector, and the second is
the length to be used for the elements of the vector.




















































