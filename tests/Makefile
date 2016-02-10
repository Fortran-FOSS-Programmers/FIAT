#PFUNIT = /opt/pfunit-gfortran-5
F90 = gfortran-5
include $(PFUNIT)/include/base.mk

# flags for debugging or for maximum performance, comment as necessary
INC = ./mod
LIBINC = ../mod
FCFLAGS = -Og -g -I$(LIBINC) -I$(INC) -I$(PFUNIT)/mod -J$(INC)
LDFLAGS = -Og -g

#LIBS = -L. -lpolycon

# List of executables to be built within the package
PFFILES := $(shell find $(SOURCEDIR) -name '*.pf')
TESTS := $(PFFILES:%.pf=%.o)

# "make" builds all

#.PRECIOUS: %.F90

all: tests

tests: init $(TESTS)

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%.o: %.f90
	$(F90) $(FCFLAGS) -o $@ -c $<

%.o: %.F90
	$(F90) $(FCFLAGS) -o $@ -c $<

clean:
	/bin/rm  -rf $(ODIR)/*.o ./tmp $(INC)/*.mod test $(PFFILES:%.pf=%.F90)

init:
	mkdir -p $(INC)
