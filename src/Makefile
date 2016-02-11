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
countable.o: countable.f90 iterator.o
ordered.o: ordered.f90 iterator.o countable.o abstract_container.o
stack.o: stack.F90 ordered.o abstract_container.o linked_node.o

%: $(ODIR)/%.o
	$(F90) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(F90) $(FCFLAGS) -o $@ -c $<

%.o: %.F90
	$(F90) $(FCFLAGS) -o $@ -c $<

clean:
	/bin/rm  -rf *.o ./tmp $(INC)/*.mod $(ARCHIVE)

init:
	mkdir -p $(INC)
