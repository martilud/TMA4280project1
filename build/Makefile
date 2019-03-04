# Start of makefile
# Note, this is definitely a bad and at places redundant makefile, and is the result of me
# fiddling around with different options
# Defining variables
FC		= gfortran
MPIFC	= mpif90
FFLAGS	= #-fdefault-real-8 
BUILDDIR= build
TARGET  = zeta0 mach0 zeta1 mach1 zeta2 mach2 zeta3 mach3 zeta4 mach4


# Compile every .f90 file from src into build
all: $(TARGET)
	@echo Compiled everything sucessfully

# Ensure results folder exists
results:
	mkdir ../results

# Rule specifying how to compile .f90 files
zeta0: ../zeta0/zeta0.f90
	$(FC) $(FFLAGS) -o zeta0 ../zeta0/zeta0.f90

mach0: ../mach0/mach0.f90
	$(FC) $(FFLAGS) -o mach0 ../mach0/mach0.f90

zeta1: ../zeta1/zeta1.f90
	$(MPIFC) $(FFLAGS) -o zeta1 ../zeta1/zeta1.f90

mach1: ../mach1/mach1.f90
	$(MPIFC) $(FFLAGS) -o mach1 ../mach1/mach1.f90

zeta2: ../zeta2/zeta2.f90
	$(MPIFC) $(FFLAGS) -o zeta2 ../zeta2/zeta2.f90

mach2: ../mach2/mach2.f90
	$(MPIFC) $(FFLAGS) -o mach2 ../mach2/mach2.f90

zeta3: ../zeta3/zeta3.f90
	$(FC) $(FFLAGS) -fopenmp -o zeta3 ../zeta3/zeta3.f90

mach3: ../mach3/mach3.f90
	$(FC) $(FFLAGS) -fopenmp -o mach3 ../mach3/mach3.f90

zeta4: ../zeta4/zeta4.f90
	$(MPIFC) $(FFLAGS) -fopenmp -o zeta4 ../zeta4/zeta4.f90

mach4: ../mach4/mach4.f90
	$(MPIFC) $(FFLAGS) -fopenmp -o mach4 ../mach4/mach4.f90

# Unit tests
zeta0utest: zeta0
	./zeta0 utest

mach0utest: mach0
	./mach0 utest

zeta1utest: zeta1
	mpirun -np 4 ./zeta1 utest

mach1utest: mach1
	mpirun -np 4 ./mach1 utest

zeta2utest: zeta2
	mpirun -np 4 ./zeta2 utest

mach2utest: mach2
	mpirun -np 4 ./mach2 utest

zeta3utest: zeta3
	./zeta3 utest

mach3utest: mach3
	./mach3 utest

zeta4utest: zeta4
	mpirun -np 4 ./zeta4 utest

mach4utest: mach4
	mpirun -np 4 ./mach4 utest

utest: zeta0utest mach0utest zeta1utest mach1utest zeta2utest mach2utest zeta3utest mach3utest zeta4utest mach4utest

zeta0vtest: zeta0 results
	./zeta0 vtest
	mv zeta0.txt ../results/zeta0.txt

mach0vtest: mach0 results
	./mach0 vtest
	mv mach0.txt ../results/mach0.txt

zeta2vtest: zeta2 ../zeta2/vtest results
	../zeta2/vtest
	mv zeta2.txt ../results/zeta2.txt

mach2vtest: mach2 ../mach2/vtest results
	../mach2/vtest
	mv mach2.txt ../results/mach2.txt

zeta3vtest: zeta3 results
	./zeta3 vtest
	mv zeta3.txt ../results/zeta3.txt

mach3vtest: mach3 results
	./mach3 vtest
	mv mach3.txt ../results/mach3.txt

zeta4vtest: zeta4 ../zeta4/vtest results
	../zeta4/vtest
	mv zeta4.txt ../results/zeta4.txt

mach4vtest: mach4 ../mach4/vtest results
	../mach4/vtest
	mv mach4.txt ../results/mach4.txt

vtest: zeta0vtest mach0vtest zeta2vtest mach2vtest zeta3vtest mach3vtest zeta4vtest mach4vtest

clean:
	rm zeta* mach*
