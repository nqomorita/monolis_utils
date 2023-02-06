#> monolis_utils Makefile

##> compiler setting
FC     = mpif90
#FFLAGS = -fPIC -O2 -mtune=native -march=native -std=legacy -Wno-missing-include-dirs
FFLAGS = -fPIC -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow -Wno-missing-include-dirs
CC     = mpicc
CFLAGS = -fPIC -O2

##> directory setting
MOD_DIR = -J ./include
INCLUDE = -I /usr/include
BIN_DIR = ./bin
SRC_DIR = ./src
OBJ_DIR = ./obj
LIB_DIR = ./lib
TST_DIR = ./test
LIBRARY = libmonolis_utils.a
CPP     = -cpp $(FLAG_DEBUG)

##> option setting
ifdef FLAGS
	comma:= ,
	empty:=
	space:= $(empty) $(empty)
	DFLAGS = $(subst $(comma), $(space), $(FLAGS))

	ifeq ($(findstring DEBUG, $(DFLAGS)), DEBUG)
		FFLAGS  = -fPIC -O2 -std=legacy -fbounds-check -fbacktrace -Wuninitialized -ffpe-trap=invalid,zero,overflow -Wno-missing-include-dirs
	endif

	ifeq ($(findstring INTEL, $(DFLAGS)), INTEL)
		FC      = mpiifort
		FFLAGS  = -fPIC -O2 -align array64byte
		CC      = mpiicc 
		CFLAGS  = -fPIC -O2 -no-multibyte-chars
		MOD_DIR = -module ./include
	endif
endif

##> other commands
MAKE = make
CD   = cd
RM   = rm -r
AR   = - ar ruv

##> **********
##> target (1)
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_DEFINE = \
def_prm.f90

SRC_ALLOC = \
error.f90 \
alloc.f90 \
sys.f90

SRC_COM = \
def_com.f90

SRC_STD = \
std_test.f90 \
std_sort_I.f90 \
std_sort_R.f90 \
std_algebra.f90 \
stdlib.f90

SRC_KDTREE = \
aabb.f90 \
kdtree.f90

SRC_HASH = \
hash.f90

SRC_SHAPE = \
shape_C2D3.f90 \
shape_C2D4.f90 \
shape_C2D6.f90 \
shape_C3D10.f90 \
shape_C3D4.f90 \
shape_C3D8.f90 \
shape_util.f90

SRC_MPI = \
mpi_util.f90 \
mpi.f90

SRC_IO = \
io_arg.f90 \
io_file_name.f90 \
io_mtx.f90 \
io_com.f90 \
io.f90

SRC_ALL = \
$(addprefix define/, $(SRC_DEFINE)) \
$(addprefix sys/, $(SRC_ALLOC)) \
$(addprefix std/, $(SRC_STD)) \
$(addprefix kdtree/, $(SRC_KDTREE)) \
$(addprefix hash/, $(SRC_HASH)) \
$(addprefix com/, $(SRC_COM)) \
$(addprefix mpi/, $(SRC_MPI)) \
$(addprefix io/, $(SRC_IO)) \
$(addprefix shape/, $(SRC_SHAPE)) \
monolis_utils.f90

##> lib objs
LIB_SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL))
LIB_OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(LIB_SOURCES:.f90=.o))
LIB_OBJS    = $(LIB_OBJSt:.c=.o)

##> **********
##> target (2)
TEST_TARGET = $(TST_DIR)/monolis_utils_test

##> lib objs
TST_SOURCES = $(addprefix $(TST_DIR)/, $(SRC_ALL))
TST_OBJSt   = $(subst $(TST_DIR), $(OBJ_DIR), $(TST_SOURCES:.f90=_test.o))
TST_OBJS    = $(TST_OBJSt:.c=_test.o)

##> target
all: $(LIB_TARGET) $(TEST_TARGET)
lib: $(LIB_TARGET)

$(LIB_TARGET): $(LIB_OBJS)
	$(AR) $@ $(LIB_OBJS)

$(TEST_TARGET): $(TST_OBJS)
	$(FC) $(FFLAGS) -o $@ $(TST_OBJS) -L./lib -lmonolis_utils

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

clean:
	$(RM) \
	$(LIB_OBJS) \
	$(TST_OBJS) \
	$(LIB_TARGET) \
	$(TEST_TARGET) \
	./include/*.mod \
	./bin/*

.PHONY: clean
