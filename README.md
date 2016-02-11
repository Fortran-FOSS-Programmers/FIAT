[![Build Status](https://travis-ci.org/Fortran-FOSS-Programmers/FIAT.svg?branch=master)](https://travis-ci.org/Fortran-FOSS-Programmers/FIAT)
[![codecov.io](https://codecov.io/github/Fortran-FOSS-Programmers/FIAT/coverage.svg?branch=master)](https://codecov.io/github/Fortran-FOSS-Programmers/FIAT?branch=master)

# FIAT: Fortran Implemented Abstract Types
This library aims to implement the following data structures in Fortran.
These will be implemented in a generic manner using the `container_type`
found in the [PolyCon](https://github.com/cmacmackin/PolyCon) library.
PolyCon may ultimately be folded into FIAT.

## Data Structures
A list of data structures which could be implemented is provided below.

- [ ] Queue
- [ ] Double Ended Queue
- [ ] Priority Queue
- [ ] Stack
- [ ] List
  - [ ] Linked List
  - [ ] Array List
- [ ] Set
  - [ ] Hash Set
  - [ ] Tree Set
- [ ] Multiset
  - [ ] Hash Multiset
  - [ ] Tree Multiset
- [ ] Map
  - [ ] Hash Map
  - [ ] Tree Map
- [ ] Multimap
  - [ ] Hash Multimap
  - [ ] Tree Multimap
- [ ] Tree
- [ ] Graph
- [ ] Digraph

This list, while not necessarily meant to be exhaustive, does represent a
long-term goal. Initial goals would be to implement queues, stacks, and lists.

## Inheritance Structure
While the absence of interfaces (of the type found in Java) or multiple
inheritance (as found in C++) somewhat limits the flexibility of these data
structures, thought has been put into how best to make these entities useful
in a polymorphic way. A potential inheritance structure has been mapped out
in UML using the software [Dia](https://wiki.gnome.org/Apps/Dia). A Dia
file is provided in the repository. (Note that,
as the author of this file is inexperienced with UML, the direction of
inheritance in the diagram may have accidentally been backwards. In any case,
the intended meaning should be clear from the context.) An SVG version of
the UML has also been produced, called `datastruct.svg`. Apologies are
given for the massive size of the diagram and for the uneven level of
implementation details which have been added.

## License
FIAT is licensed under the GNU Lesser General Public License (LGPL) v3.0 or
later. The terms are provided in the file `LICENSE`. The LGPL make reference
to the GNU General Public License (GPL), which is provided in the file `GPL`.
In brief, the LGPL allows this library to be linked to software under any
license (with a few, minor, restrictions). However, should a modified version
of the _library itself_ be released, it must be licensed under the terms of
the LGPL or GPL.
