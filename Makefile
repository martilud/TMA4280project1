# Start of makefile
# Note, this is definitely a bad and at places redundant makefile, and is the result of me
# fiddling around with different options
# Defining variables
FC=gfortran
FFLAGS=-03 -fdefault-real-8 -fdefault-integer-8
SRCDIR = src
BUILDDIR = build
SRC = $(wildcard $(SRCDIR)/*.f90)
OBJ = $(patsubst $(SRCDIR)/%.f90,$(BUILDDIR)/%.o,$(SRC))

%.o: %.f90
	$(FC) -o $(FFLAGS) $<

$(info $(OBJ))
#all: $(OBJECTS)
#
$(OBJ): $(BUILDDIR)/%.o 
	$(FC) -o $< $@

all: $(OBJ) 

#mach0: src/mach0.f90
#	$(FC) -o $(FLAGS) build/mach0 src/mach0.f90

#zeta0: src/zeta0.f90
#	$(FC) -o $(FLAGS) build/zeta0 src/zeta0.f90
