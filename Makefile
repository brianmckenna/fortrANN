#Makefile for fortrANN

OBJECTS = $(FOBJECTS) $(COBJECTS)
MODULES = $(SMODULES) $(FMODULES)
MODOBJS = $(SMODOBJS) $(FMODOBJS) 

# Network modules
ANNMODULES   = m_network.mod
ANNOBJECTS   = m_network.o

# Data Modules
DATAMODULES = m_data.mod
DATAOBJECTS = m_data.o

# Weight Initialization modules
INITMODULES = m_initial.mod
INITOBJECTS = m_initial.o

# Error Modules
ERRORMODULES = m_mse.mod
ERROROBJECTS = m_mse.o
#ERRORMODULES = module_rmse.mod module_mse.mod
#ERROROBJECTS = module_rmse.o   module_mse.o

# Train modules
TRAINMODULES = m_levenberg_marquardt.mod m_backprop.mod
TRAINOBJECTS = m_levenberg_marquardt.o   m_backprop.o

# Forecast modules
FCSTMODULES = m_forecast.mod
FCSTOBJECTS = m_forecast.o

# Stat modules
STATMODULES = m_moment.mod m_normalize.mod m_diagnostics.mod
STATOBJECTS = m_moment.o   m_normalize.o   m_diagnostics.o

FMODULES = $(ANNMODULES) $(DATAMODULES) $(INITMODULES) $(ERRORMODULES) $(STATMODULES) $(FCSTMODULES) $(TRAINMODULES)
FMODOBJS = $(ANNOBJECTS) $(DATAOBJECTS) $(INITOBJECTS) $(ERROROBJECTS) $(STATOBJECTS) $(FCSTOBJECTS) $(TRAINOBJECTS)

SMODULES = m_types.mod m_constants.mod m_file_util.mod
SMODOBJS = m_types.o   m_constants.o   m_file_util.o

FOBJECTS = main_ann.o
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

