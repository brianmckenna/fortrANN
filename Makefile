#Makefile for fortrANN

OBJECTS = $(FOBJECTS) $(COBJECTS)
MODULES = $(SMODULES) $(FMODULES)
MODOBJS = $(SMODOBJS) $(FMODOBJS) 

# Network modules
ANNMODULES   = m_network.mod m_epoch.mod
ANNOBJECTS   = m_network.o m_epoch.o

# Data Modules
DATAMODULES = m_data.mod
DATAOBJECTS = m_data.o

# Stat modules
#STATMODULES = m_normalize.mod
#STATOBJECTS = m_normalize.o

# Error Modules
ERRORMODULES = m_moment.mod m_error.mod m_normalize.mod
ERROROBJECTS = m_moment.o m_error.o m_normalize.o

# Train modules
TRAINMODULES = m_levenberg_marquardt.mod m_backprop.mod
TRAINOBJECTS = m_levenberg_marquardt.o m_backprop.o

FMODULES = $(ANNMODULES) $(DATAMODULES) $(INITMODULES) $(ERRORMODULES) $(STATMODULES) $(FCSTMODULES) $(TRAINMODULES)
FMODOBJS = $(ANNOBJECTS) $(DATAOBJECTS) $(INITOBJECTS) $(ERRORMODULES) $(STATMODULES) $(FCSTOBJECTS) $(TRAINOBJECTS)

SMODULES = m_types.mod m_constants.mod m_file_util.mod
SMODOBJS = m_types.o m_constants.o m_file_util.o

FOBJECTS = ann_train.o
COBJECTS = 

# Fortan/C Mix Libraries
#FCLIBS = -L/usr/local/pgi/linux86/lib -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl -lm -lpgc -lgcc
#LIBS = $(FCLIBS)

.SUFFIXES : .F90 .f90 .c .mod

#COMPILERS
CC = gcc      # -- GNU C Compiler
FC = gfortran # -- GNU Fortran
#CC = pgcc     # -- Portland Group C Compiler
#FC = pgf90    # -- Portland Group Fortran on Linux

#COMPILE FLAGS
#FFLAGS = -O3 
#FFLAGS = -O2 -tp p7 -Kieee    # -- Portland Group Fortran on Linux
#FFLAGS = -O2 -tp p7 -Kieee -g # -- Portland Group Fortran on Linux DEBUG

#FFLAGS = -O2 -tp p7
#FFLAGS = -O2 -Ktrap=inv,divz,ovf  -Kieee
#FFLAGS = -Mbounds

%.mod : %.o
	@if [! -f $@ ]; then \
		rm $< \
		$(FC) $(FFLAGS) -c $< \
	fi

.c.o :
	$(CC) $(CFLAGS) -c $<

.f90.o :
	$(FC) $(FFLAGS) -c $<

.F90.o :
	$(FC) $(FFLAGS) -c $<

ann: $(MODOBJS) $(OBJECTS) $(INCLUDES)
	$(FC) $(FFLAGS) -o ann $(MODOBJS) $(OBJECTS) $(LIBS)

code: objects $(INCLUDES)

modules: $(MODOBJS)

objects: modules $(OBJECTS)

clean:
	rm -f ann $(MODOBJS) $(MODULES) $(OBJECTS)

