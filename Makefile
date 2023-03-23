FC=gfortran
TARGET=read_dat

obj = string.o tecplot.o read_dat.o

.SUFFIXES: .o .f90

%.o: %.f90
	$(FC) -c -O3 $(FFLAGS) $<

$(TARGET): $(obj)
	$(FC) $^ -o $(TARGET)

clean:
	rm -rf *.o *.mod $(TARGET) *~ *.plt

