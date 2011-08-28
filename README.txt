Haskell-sprng, Haskell bindings to the SPRNG library
----------------------------------------------------

How to build
------------

cabal install --extra-lib-dirs=/path/to/lib/ --extra-include-dirs=/path/to/include/

Note: when you use the Haskell-SPRNG library in your own Haskell program you must
link it with the same C++ compiler that was used to build the library.

For example:

   ghc --make -O2 Foo.hs -pgml /path/to/c++/compiler

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

Haskell-sprng is distributed as open source software under the terms of the BSD 
License (see the file LICENSE in the top directory).

Author(s): Bernie Pope, Copyright ... 2011

Contact information
-------------------

Email Bernie Pope:

   florbitous <at> gmail <dot> com

History
-------
