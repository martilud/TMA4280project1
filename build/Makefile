# Start of makefile
# Note, this is definitely a bad and at places redundant makefile, and is the result of me
# fiddling around with different options
# Defining variables
FC		= gfortran
FFLAGS	= -fdefault-real-8 -fdefault-integer-8
SRCDIR 	= src
OBJDIR  = object
BUILDDIR= build
TARGET  = zeta0 mach0


# Compile every .f90 file from src into build
all: $(TARGET)
	@echo Compiled everything sucessfully

# Rule specifying how to compile .f90 files from src to build
zeta0: ../zeta0/zeta0.f90
	$(FC) $(FFLAGS) -o zeta0 ../zeta0/zeta0.f90

mach0: ../mach0/mach0.f90
	$(FC) $(FFLAGS) -o mach0 ../mach0/mach0.f90

# This used to work but does not work anymore
#%: %/%.f90
#	$(FC) $(FFLAGS) -o $(BUILDDIR)/$@ $<

zeta0utest:
	./zeta0 utest

mach0utest:
	./mach0 utest

utest: zeta0utest mach0utest

zeta0vtest:
	./zeta0 vtest
	mv zeta0.txt ../results/zeta0.txt

mach0vtest:
	./mach0 vtest
	mv mach0.txt ../results/mach0.txt

vtest: zeta0vtest mach0vtest

clean:
	rm $(BUILDDIR)/*
	@echo Cleaning Complete
