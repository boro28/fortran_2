PFUNIT = /opt/pfunit/pfunit-serial
F90_VENDOR = Intel
F90=ifort

include $(PFUNIT)/include/base.mk

FFLAGS = -std08 -warn all  -funroll-loops -pedantic -I$(PFUNIT)/mod
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)

%.f90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: %.f90
	$(F90) $(FFLAGS) -c $<

build:
	$(F90) matrix.f90 main.f90 -o main $(FLAGS)

test: testSuites.inc matrix.o $(OBJS)
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

clean:
	find . -name "*genmod*" -type f -delete
