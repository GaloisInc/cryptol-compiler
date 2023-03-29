Overview
========

Consider a Cryptol function with the following type:

    spl : {m,n} : (fin n) => [m * n] a -> [m][n]a

The execution of this function may depend on the *values* of the numeric
type parameters `m` and `n` (i.e., the polymorphism here is *not* parametric).
Because of this, we need to pass `m` and `n` as arguments at runtime.
Furthermore, the values of `m` and `n` may affect the *representation*
of the sequence types---for example, if `m` is infinite, then the result of
the function would be a *stream* of values, rather than a vector.

To implement this, we use type specialization, where we generate different
instances of `spl` for different values of `m` and `n`.  However, we do not
want to generate an instance for each *concrete* value of `m` and `n` as
this would result in a lot of duplicated code and we would not be able
to support code that is polymorphic in the parameters.  Instead, we only
generate a new instance of the function, if the values of `m` and `n` would
affect the representation types of the sequences.

Example 1:
Consider an invocation where `m = 16` and `n = 8`.  We would use the
following instance:

    spl_fin_fin :
      (m : size_t, n : size_t, xs : vec{m*n} a) -> vec{m} (vec{n} a)

and we would pass `16` and `8` as the first two parameters.


Example 2:
Consider an invocation where `m = inf` and `n = 8`.  We would use an instance
like this:

    spl_inf_fin :
      (n : size_t, xs : stream a) -> stream (vec{n} a)

and we would pass `8` as the first argument.


Example 3:
The function `spl` is interesting because it cannot be *completely*
polymorphic in `n` when `m = inf`.  To see this, consider the case
where `m = inf` and `n = 0`. Here we need a 3rd instance:

    spl_inf_0 :
      (xs : vec{0} a) -> stream (vec{0} a)

Note that for internal functions (i.e., functions that may be called by
other Cryptol functions, but are not directly callable by the user)
we would like to generate the instances lazily (i.e., only if they are needed).
For example, if `spl` is always used with finite values, we'd only ever
generate the code for the instance in Example 1.


Code for Invocations
====================

Note that even though we do not need to generate all possible instances
of a function, we want to *know* their types, so that when generating code
for an invocation we need to know how to compile the arguments.
For example, consider the following invcation:

    spl`{inf,n} e

We have to know that depending on `n` we should either compile `e`
as a `stream`, or as a `vec{0}`.  When faced with such situation,
the most general solution is to generate multiple instances of the encolsing
function---one where `n = 0`, and one where it is not.

In some case, however, we could avoid having an extra instance, and do
a dynamic check instead. Our example falls in this category, because the result
type of all matching instances (`spl_inf_0` and `spl_inf_Inf`) uses `vec`
as the represenation, so we could generate the call like this instead:

    if n == 0
      then spl_inf_0    ( [| compile e as vec{0} |] )
      else spl_inf_inf  ( [| compile e as stream |] )

This also illustrates the observation that it is not sufficient to just
look at the type signature of a function to determine its possible instances---
we have to also analyze its body, in case we need more instances due
to calls to other functions.

Compilcations
-------------

In general, some situations may require pretty complicated instantiations.
Consider for example:

    spl`{inf, m - n}

If we were to generate a separete instance (rather than a dynamic check),
we'd need to have cases for where `m == n` vs not, and that seems hard to
check in general.   Perhaps, for now we do not support such cases.


`Z` Types
==========

Besides sequences, `Z` types also use numeric types as indexes, so we
may also pass numeric types in code that uses `Z`.  Unlike sequence lengths,
it is not uncommon for the numeric types used as `Z` indexes to be very
large---larger than would fit in a 64-bit word.   So in those cases,
we may need additional cases in the numeric specialization where the
numeric types are passed in as a large integer, rather then a `size_t`.
