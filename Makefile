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

##> **********
##> target (1)
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_DEFINE = \
def_prm.f90

SRC_ALLOC = \
alloc.f90

SRC_STD = \
std_test.f90 \
std_error.f90 \
std_sort_I.f90 \
std.f90

SRC_SHAPE = \
shape_C2D3.f90

SRC_ALL = \
$(addprefix define/, $(SRC_DEFINE)) \
$(addprefix alloc/, $(SRC_ALLOC)) \
$(addprefix std/, $(SRC_STD)) \
monolis_utils.f90

##> lib objs
LIB_SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL))
LIB_OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(LIB_SOURCES:.f90=.o))
LIB_OBJS    = $(LIB_OBJSt:.c=.o)

##> **********
##> target (2)
TEST_TARGET = $(TST_DIR)/monolis_utils_test

##> test file define
SRC_ALLOC_TEST = \
alloc_test.f90

SRC_STD_TEST = \
std_test_test.f90 \
std_sort_I_test.f90

SRC_TEST_ALL = \
$(addprefix alloc/, $(SRC_ALLOC_TEST)) \
$(addprefix std/, $(SRC_STD_TEST)) \
test.f90

##> lib objs
TST_SOURCES = $(addprefix $(TST_DIR)/, $(SRC_TEST_ALL))
TST_OBJSt   = $(subst $(TST_DIR), $(OBJ_DIR), $(TST_SOURCES:.f90=.o))
TST_OBJS    = $(TST_OBJSt:.c=.o)

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
