#> monolis_utils Makefile

##> compiler setting
FC     = mpif90
FFLAGS = -O2 -mtune=native -march=native -std=legacy -Wno-missing-include-dirs
CC     = mpicc
CFLAGS = -O2

##> directory setting
MOD_DIR = -J ./include
INCLUDE = -I /usr/include
BIN_DIR = ./bin
SRC_DIR = ./src
OBJ_DIR = ./obj
LIB_DIR = ./lib
LIBRARY = libmonolis_utils
CPP     = -cpp $(FLAG_DEBUG)

##> option setting
ifdef FLAGS
	comma:= ,
	empty:=
	space:= $(empty) $(empty)
	DFLAGS = $(subst $(comma), $(space), $(FLAGS))

	ifeq ($(findstring DEBUG, $(DFLAGS)), DEBUG)
		FFLAGS  = -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow -Wno-missing-include-dirs
	endif

	ifeq ($(findstring INTEL, $(DFLAGS)), INTEL)
		FC      = mpiifort
		FFLAGS  = -O2 -align array64byte
		CC      = mpiicc 
		CFLAGS  = -O2 -no-multibyte-chars
		MOD_DIR = -module ./include
	endif

	ifeq ($(findstring OPENMP, $(DFLAGS)), OPENMP)
		FC += -fopenmp
	endif
endif

##> other commands
MAKE = make
CD   = cd
RM   = rm -r
AR   = - ar ruv

##> lib target
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_DEFINE = \
def_prm.f90

SRC_STD = \
std_sort_I.f90 \
std.f90

SRC_SHAPE = \
shape_C2D3.f90

SRC_ALL = \
$(addprefix define/, $(SRC_DEFINE)) \
$(addprefix std/, $(SRC_STD))
#$(addprefix shape/, $(SRC_SHAPE))

##> objs
SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL))
OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(SOURCES:.f90=.o))
OBJS    = $(OBJSt:.c=.o)

##> target
all: $(LIB_TARGET)
lib: $(LIB_TARGET)

$(LIB_TARGET): $(OBJS)
	$(AR) $@ $(OBJS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

clean:
	$(RM) $(OBJS) \
	$(LIB_TARGET) \
	./include/*.mod ./bin/*

.PHONY: clean
