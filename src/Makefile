# libraries needed for linking, unused in the examples
SOURCES := $(shell find -name '*.f90')
SOURCESPP := $(shell find -name '*.F90')
OBJS = $(SOURCES:%.f90=%.o) $(SOURCESPP:%.F90=%.o)

# The compiler
F90 = gfortran-5

# List of executables to be built within the package
INC = ../mod
FCFLAGS = -Og -g -I$(INC) -J$(INC) -fprofile-arcs -ftest-coverage
LDFLAGS = -Og -g

ARCHIVE = libfiat.a

# "make" builds all
all: lib

lib: init $(ARCHIVE)

$(ARCHIVE): $(OBJS)
	ar rcs $@ $^

container.o: container.f90 abstract_container.o

node.o: node.f90 abstract_container.o
linked_node.o: linked_node.f90 node.o
bidir_node.o: bidir_node.f90 linked_node.o

iterator.o: iterator.f90 abstract_container.o
iterable.o: iterable.f90 abstract_container.o iterator.o
countable.o: countable.f90 iterable.o
ordered.o: ordered.F90 abstract_container.o iterator.o iterable.o countable.o
stack.o: stack.F90 ordered.o abstract_container.o linked_node.o
queue.o: queue.f90 ordered.o
deque.o: deque.f90 queue.o abstract_container.o
list.o: list.f90 deque.o abstract_container.o
data_set.o: data_set.f90 array_list.o countable.o abstract_container.o
dictionary.o: dictionary.f90 array_list.o data_set.o abstract_container.o
dynamic_set.o: dynamic_set.F90 data_set.o iterable.o abstract_container.o
map.o: map.f90 dictionary.o abstract_container.o
multimap.o: multimap.f90 map.o dictionary.o array_list.o
multiset.o: multiset.f90 dynamic_set.o 

array_list.o: array_list.f90 list.o iterator.o ordered.o abstract_container.o


gcov:
	gcov *.gcno -pb

%: $(ODIR)/%.o
	$(F90) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(F90) $(FCFLAGS) -o $@ -c $<

%.o: %.F90
	$(F90) $(FCFLAGS) -o $@ -c $<

clean:
	/bin/rm  -rf *.o ./tmp $(INC)/*.mod *.gcno *.gcda *.gcov *.s *~

init:
	mkdir -p $(INC)
