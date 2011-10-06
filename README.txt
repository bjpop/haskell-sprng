Haskell-sprng, Haskell bindings to the SPRNG library
----------------------------------------------------

Note: Version 4 of the SPRNG library is written in C++. This makes
it frustrating to use via the Haskell FFI because it means we have to
use a C++ compiler to link the program (due to C++'s name mangling).
Also, SPRNG is not the easiest library to build, which also complicates
its use. At address these issues we have made a C version of the
Modified Addidative Lagged Fibonacci generator from SPRNG, and included
that in this src bundle. This makes it easy to use with the FFI and
it builds automatically with cabal.

SPRNG version 4 is released under version 2 of the GPL.

How to build
------------

cabal install

Testing
-------

How to enable testing
---------------------

How to run the unit tests
-------------------------

How to run standalone tests
---------------------------

License and Copyright
---------------------

Author(s): Bernie Pope, Copyright ... 2011

Contact information
-------------------

Email Bernie Pope:

   florbitous <at> gmail <dot> com

History
-------
