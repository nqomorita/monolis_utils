#> monolis_utils Makefile

##> compiler setting
FC     = mpif90
FFLAGS = -fPIC -O2 -mtune=native -march=native -std=legacy -Wno-missing-include-dirs
CC     = mpicc
CFLAGS = -fPIC -O2

##> directory setting
MOD_DIR = -J ./include
INCLUDE = -I /usr/include -I ./include
BIN_DIR = ./bin
SRC_DIR = ./src
OBJ_DIR = ./obj
LIB_DIR = ./lib
TST_DIR = ./test
DRV_DIR = ./driver
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
RM   = rm -rf
AR   = - ar ruv

##> **********
##> target (1)
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_DEFINE1 = \
def_prm.f90

SRC_DEFINE2 = \
def_com.f90

SRC_ALLOC = \
error.f90 \
alloc.f90 \
sys.f90

SRC_COM = \
comm_par_util.f90 \
comm_ser_util.f90 \
comm_table.f90

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
io_util.f90 \
io.f90

SRC_DRIVE = \
driver_util.f90 \
extract_util.f90 \
refiner_util.f90

SRC_ALL = \
$(addprefix define/, $(SRC_DEFINE1)) \
$(addprefix sys/, $(SRC_ALLOC)) \
$(addprefix define/, $(SRC_DEFINE2)) \
$(addprefix std/, $(SRC_STD)) \
$(addprefix kdtree/, $(SRC_KDTREE)) \
$(addprefix hash/, $(SRC_HASH)) \
$(addprefix mpi/, $(SRC_MPI)) \
$(addprefix com/, $(SRC_COM)) \
$(addprefix io/, $(SRC_IO)) \
$(addprefix shape/, $(SRC_SHAPE)) \
$(addprefix driver/, $(SRC_DRIVE)) \
monolis_utils.f90

##> lib objs
LIB_SOURCES = $(addprefix $(SRC_DIR)/, $(SRC_ALL))
LIB_OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(LIB_SOURCES:.f90=.o))
LIB_OBJS    = $(LIB_OBJSt:.c=.o)

##> **********
##> driver target (2)
DRIVE1 = $(BIN_DIR)/monolis_dbc_all_surf_hex
DRIVE2 = $(BIN_DIR)/monolis_dbc_all_surf_tet
DRIVE3 = $(BIN_DIR)/monolis_extract_all_surf_hex
DRIVE4 = $(BIN_DIR)/monolis_extract_all_surf_tet
DRIVE5 = $(BIN_DIR)/monolis_h_refiner_hex
DRIVE6 = $(BIN_DIR)/monolis_h_refiner_tet
DRIVE7 = $(BIN_DIR)/monolis_p_refiner_tet

DRV_OBJS1 = ./obj/dbc_all_surf_hex.o
DRV_OBJS2 = ./obj/dbc_all_surf_tet.o
DRV_OBJS3 = ./obj/extract_all_surf_hex.o
DRV_OBJS4 = ./obj/extract_all_surf_tet.o
DRV_OBJS5 = ./obj/h_refiner_hex.o
DRV_OBJS6 = ./obj/h_refiner_tet.o
DRV_OBJS7 = ./obj/p_refiner_tet.o

##> **********
##> test target (3)
TEST_TARGET = $(TST_DIR)/monolis_utils_test

##> lib objs
TST_SRC_ALL = driver/driver.f90 $(SRC_ALL)
TST_SOURCES = $(addprefix $(TST_DIR)/, $(TST_SRC_ALL))
TST_OBJSt   = $(subst $(TST_DIR), $(OBJ_DIR), $(TST_SOURCES:.f90=_test.o))
TST_OBJS    = $(TST_OBJSt:.c=_test.o)

##> target
all: \
	$(LIB_TARGET) \
	$(DRIVE1) \
	$(DRIVE2) \
	$(DRIVE3) \
	$(DRIVE4) \
	$(DRIVE5) \
	$(DRIVE6) \
	$(DRIVE7) \
	$(TEST_TARGET)

lib: \
	$(LIB_TARGET)

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

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.c
	$(CC) $(CFLAGS) $(CPP) $(INCLUDE) -o $@ -c $<

$(DRIVE1): $(DRV_OBJS1)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS1) -L./lib -lmonolis_utils

$(DRIVE2): $(DRV_OBJS2)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS2) -L./lib -lmonolis_utils

$(DRIVE3): $(DRV_OBJS3)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS3) -L./lib -lmonolis_utils

$(DRIVE4): $(DRV_OBJS4)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS4) -L./lib -lmonolis_utils

$(DRIVE5): $(DRV_OBJS5)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS5) -L./lib -lmonolis_utils

$(DRIVE6): $(DRV_OBJS6)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS6) -L./lib -lmonolis_utils

$(DRIVE7): $(DRV_OBJS7)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS7) -L./lib -lmonolis_utils

clean:
	$(RM) \
	$(LIB_OBJS) \
	$(TST_OBJS) \
	$(DRV_OBJS1) \
	$(LIB_TARGET) \
	$(TEST_TARGET) \
	$(DRIVE1) \
	$(DRIVE2) \
	$(DRIVE3) \
	$(DRIVE4) \
	$(DRIVE5) \
	$(DRIVE6) \
	$(DRIVE7) \
	./include/*.mod \
	./bin/*

.PHONY: clean
