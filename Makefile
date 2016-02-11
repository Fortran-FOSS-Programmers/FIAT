#PFUNIT = /opt/pfunit-gfortran-5
export PFUNIT

# The compiler
F90 = gfortran-5

# flags for debugging or for maximum performance, comment as necessary
INC = ../mod
FCFLAGS = -Og -g -I$(INC) -J$(INC)
LDFLAGS = -Og -g 

ARCHIVE = libfiat.a
SRC = ./src
TEST = ./tests
EXE = tests.x
INC = ./mod
TESTINC = $(TEST)/mod

# "make" builds all
all: lib

$(ARCHIVE): lib

lib:
	make -C $(SRC) lib
	cp $(SRC)/$(ARCHIVE) .

tests: $(EXE)
	./$(EXE) 

SUT: lib
	make -C $(TEST) tests


$(EXE): SUT 
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -I$(INC) \
		-I$(TEST) -I$(TESTINC) \
		$(PFUNIT)/include/driver.F90 $(TEST)/*.o $(FCFLAGS) \
		 -L. -lfiat -L$(PFUNIT)/lib -lpfunit -fprofile-arcs

gcov: tests
	make -C $(SRC) gcov

%: $(ODIR)/%.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.F
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.F95
	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	make -C $(SRC) clean
	make -C $(TEST) clean
	rm $(ARCHIVE)
	rm *.gcov

init:
	mkdir -p $(ODIR)
	mkdir -p $(INC)
