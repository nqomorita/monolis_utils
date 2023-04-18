#> monolis_utils Makefile

##> compiler setting
FC     = mpif90
FFLAGS = -fPIC -O2 -mtune=native -march=native -std=legacy -Wno-missing-include-dirs
CC     = mpicc -std=c99
CFLAGS = -fPIC -O2

##> directory setting
MOD_DIR = -J ./include
INCLUDE = -I /usr/include -I ./include
BIN_DIR = ./bin
SRC_DIR = ./src
TST_DIR = ./src_test
OBJ_DIR = ./obj
LIB_DIR = ./lib
WRAP_DIR= ./wrapper
TST_WRAP_DIR = ./wrapper_test
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
		CFLAGS  = -fPIC -O2 -g -ggdb
	endif

	ifeq ($(findstring INTEL, $(DFLAGS)), INTEL)
		FC      = mpiifort
		FFLAGS  = -fPIC -O2 -align array64byte -nofor-main
		CC      = mpiicc 
		CFLAGS  = -fPIC -O2 -no-multibyte-chars
		MOD_DIR = -module ./include
	endif
endif

##> other commands
MAKE = make
CD   = cd
CP   = cp
RM   = rm -rf
AR   = - ar ruv

##> **********
##> target (1)
LIB_TARGET = $(LIB_DIR)/$(LIBRARY)

##> source file define
SRC_DEFINE1 = \
def_prm.f90

SRC_MPI1 = \
mpi_util.f90

SRC_DEFINE2 = \
def_com.f90

SRC_MPI2 = \
mpi.f90

SRC_SYS = \
error.f90 \
alloc.f90 \
palloc.f90 \
sys.f90

SRC_COM = \
comm_par_util.f90 \
comm_ser_util.f90 \
comm_table.f90

SRC_DEFINE3 = \
def_com_init.f90

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

##> C wrapper section
SRC_DEFINE_C = \
monolis_def_com_c.c \
monolis_def_com_init_c.c

SRC_SYS_C = \
monolis_alloc_c.c

SRC_STD_C = \
std_sort_I_wrap.f90 \
std_test_wrap.f90

SRC_IO_C = \
monolis_io_util_c.c \
monolis_io_file_name_c.c \
monolis_io_com_c.c \
monolis_io_c.c

SRC_COM_C = \
comm_par_util_wrap.f90 \
monolis_comm_table_c.c

SRC_MPI_C = \
mpi_util_wrap.f90 \
mpi_wrap.f90 \
monolis_mpi_c.c

SRC_ALL_C = \
$(addprefix define/, $(SRC_DEFINE_C)) \
$(addprefix sys/, $(SRC_SYS_C)) \
$(addprefix std/, $(SRC_STD_C)) \
$(addprefix io/, $(SRC_IO_C)) \
$(addprefix com/, $(SRC_COM_C)) \
$(addprefix mpi/, $(SRC_MPI_C))

##> all targes
SRC_ALL = \
$(addprefix define/, $(SRC_DEFINE1)) \
$(addprefix mpi/, $(SRC_MPI1)) \
$(addprefix sys/, $(SRC_SYS)) \
$(addprefix define/, $(SRC_DEFINE2)) \
$(addprefix mpi/, $(SRC_MPI2)) \
$(addprefix std/, $(SRC_STD)) \
$(addprefix kdtree/, $(SRC_KDTREE)) \
$(addprefix hash/, $(SRC_HASH)) \
$(addprefix com/, $(SRC_COM)) \
$(addprefix io/, $(SRC_IO)) \
$(addprefix define/, $(SRC_DEFINE3)) \
$(addprefix shape/, $(SRC_SHAPE)) \
$(addprefix driver/, $(SRC_DRIVE))

##> lib objs
LIB_SOURCES = \
$(addprefix $(SRC_DIR)/,  $(SRC_ALL)) \
$(addprefix $(WRAP_DIR)/, $(SRC_ALL_C)) \
./src/monolis_utils.f90
LIB_OBJSt   = $(subst $(SRC_DIR), $(OBJ_DIR), $(LIB_SOURCES:.f90=.o))
LIB_OBJS    = $(subst $(WRAP_DIR), $(OBJ_DIR), $(LIB_OBJSt:.c=.o))

##> **********
##> driver target (2)
DRIVE1 = $(BIN_DIR)/monolis_dbc_all_surf_hex_R
DRIVE2 = $(BIN_DIR)/monolis_dbc_all_surf_tet_R
DRIVE3 = $(BIN_DIR)/monolis_dbc_all_surf_hex_C
DRIVE4 = $(BIN_DIR)/monolis_dbc_all_surf_tet_C
DRIVE5 = $(BIN_DIR)/monolis_extract_all_surf_hex
DRIVE6 = $(BIN_DIR)/monolis_extract_all_surf_tet
DRIVE7 = $(BIN_DIR)/monolis_h_refiner_hex
DRIVE8 = $(BIN_DIR)/monolis_h_refiner_tet
DRIVE9 = $(BIN_DIR)/monolis_p_refiner_tet

DRV_OBJS1 = ./obj/dbc_all_surf_hex_R.o
DRV_OBJS2 = ./obj/dbc_all_surf_tet_R.o
DRV_OBJS3 = ./obj/dbc_all_surf_hex_C.o
DRV_OBJS4 = ./obj/dbc_all_surf_tet_C.o
DRV_OBJS5 = ./obj/extract_all_surf_hex.o
DRV_OBJS6 = ./obj/extract_all_surf_tet.o
DRV_OBJS7 = ./obj/h_refiner_hex.o
DRV_OBJS8 = ./obj/h_refiner_tet.o
DRV_OBJS9 = ./obj/p_refiner_tet.o

##> **********
##> test target for fortran (3)
TEST_TARGET = $(TST_DIR)/monolis_utils_test

##> lib objs
TST_SRC_ALL = driver/driver.f90 $(SRC_ALL) monolis_utils.f90
TST_SOURCES = $(addprefix $(TST_DIR)/, $(TST_SRC_ALL))
TST_OBJS    = $(subst $(TST_DIR), $(OBJ_DIR), $(TST_SOURCES:.f90=_test.o))

##> **********
##> test target for C (4)
TEST_C_TARGET = $(TST_WRAP_DIR)/monolis_utils_c_test

##> lib objs
SRC_DEFINE_C_TEST = \
monolis_def_com_c_test.c \
monolis_def_com_init_c_test.c

SRC_SYS_C_TEST = \
monolis_alloc_c_test.c

SRC_STD_C_TEST = \
std_sort_I_wrap_test.c

SRC_IO_C_TEST = \
monolis_io_file_name_c_test.c \
monolis_io_com_c_test.c \
monolis_io_c_test.c

SRC_COM_C_TEST = \
monolis_comm_table_c_test.c

SRC_MPI_C_TEST = \
monolis_mpi_util_c_test.c \
monolis_mpi_c_test.c

SRC_ALL_C_TEST = \
$(addprefix define/, $(SRC_DEFINE_C_TEST)) \
$(addprefix sys/, $(SRC_SYS_C_TEST)) \
$(addprefix io/, $(SRC_IO_C_TEST)) \
$(addprefix std/, $(SRC_STD_C_TEST)) \
$(addprefix com/, $(SRC_COM_C_TEST)) \
$(addprefix mpi/, $(SRC_MPI_C_TEST))

TST_SRC_C_ALL = $(SRC_ALL_C_TEST) monolis_utils_c_test.c
TST_C_SOURCES = $(addprefix $(TST_WRAP_DIR)/, $(TST_SRC_C_ALL))
TST_C_OBJS    = $(subst $(TST_WRAP_DIR), $(OBJ_DIR), $(TST_C_SOURCES:.c=.o))

##> target
all: \
	cp_header \
	$(LIB_TARGET) \
	$(DRIVE1) \
	$(DRIVE2) \
	$(DRIVE3) \
	$(DRIVE4) \
	$(DRIVE5) \
	$(DRIVE6) \
	$(DRIVE7) \
	$(DRIVE8) \
	$(DRIVE9) \
	$(TEST_TARGET) \
	$(TEST_C_TARGET)

lib: \
	cp_header \
	$(LIB_TARGET)

$(LIB_TARGET): $(LIB_OBJS)
	$(AR) $@ $(LIB_OBJS)

$(TEST_TARGET): $(TST_OBJS)
	$(FC) $(FFLAGS) $(INCLUDE) -o $@ $(TST_OBJS) -L./lib -lmonolis_utils

$(TEST_C_TARGET): $(TST_C_OBJS)
	$(FC) $(FFLAGS) $(INCLUDE) -o $@ $(TST_C_OBJS) -L./lib -lmonolis_utils

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(WRAP_DIR)/%.f90
	$(FC) $(FFLAGS) $(CPP) $(INCLUDE) $(MOD_DIR) -o $@ -c $<

$(OBJ_DIR)/%.o: $(WRAP_DIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) -o $@ -c $<

$(OBJ_DIR)/%.o: $(TST_WRAP_DIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) -o $@ -c $<

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

$(DRIVE8): $(DRV_OBJS8)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS8) -L./lib -lmonolis_utils

$(DRIVE9): $(DRV_OBJS9)
	$(FC) $(FFLAGS) -o $@ $(DRV_OBJS9) -L./lib -lmonolis_utils

cp_header:
	$(CP) ./wrapper/mpi/monolis_mpi_util_c.h ./include/
	$(CP) ./wrapper/mpi/monolis_mpi_c.h ./include/
	$(CP) ./wrapper/define/monolis_def_prm_c.h ./include/
	$(CP) ./wrapper/define/monolis_def_com_c.h ./include/
	$(CP) ./wrapper/define/monolis_def_com_init_c.h ./include/
	$(CP) ./wrapper/io/monolis_io_file_name_c.h ./include/
	$(CP) ./wrapper/io/monolis_io_com_c.h ./include/
	$(CP) ./wrapper/io/monolis_io_util_c.h ./include/
	$(CP) ./wrapper/io/monolis_io_c.h ./include/
	$(CP) ./wrapper/std/monolis_std_sort_I_c.h ./include/
	$(CP) ./wrapper/std/monolis_std_test_c.h ./include/
	$(CP) ./wrapper/sys/monolis_alloc_c.h ./include/
	$(CP) ./wrapper/com/monolis_comm_par_util_c.h ./include/
	$(CP) ./wrapper/com/monolis_comm_table_c.h ./include/
	$(CP) ./wrapper/monolis_utils.h ./include/

clean:
	$(RM) \
	$(LIB_OBJS) \
	$(TST_OBJS) \
	$(TST_C_OBJS) \
	$(DRV_OBJS1) \
	$(DRV_OBJS2) \
	$(DRV_OBJS3) \
	$(DRV_OBJS4) \
	$(DRV_OBJS5) \
	$(DRV_OBJS6) \
	$(DRV_OBJS7) \
	$(DRV_OBJS8) \
	$(DRV_OBJS9) \
	$(LIB_TARGET) \
	$(TEST_TARGET) \
	$(TEST_C_TARGET) \
	$(DRIVE1) \
	$(DRIVE2) \
	$(DRIVE3) \
	$(DRIVE4) \
	$(DRIVE5) \
	$(DRIVE6) \
	$(DRIVE7) \
	$(DRIVE8) \
	$(DRIVE9) \
	./include/*.h \
	./include/*.mod \
	./bin/*

.PHONY: clean
