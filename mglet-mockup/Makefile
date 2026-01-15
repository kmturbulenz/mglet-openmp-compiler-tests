SRC       ?= src/const_mod.F90 src/grids_mod.F90 src/basefield_mod.F90 src/realfield_mod.F90 src/fields_mod.F90 src/compute_mod.F90 src/main.F90
BUILD_DIR  ?= build
TARGET     ?= main

FFLAGS     ?=

MKDIR_CMD  := @mkdir -p $(BUILD_DIR)
REQ_FFLAGS = $(if $(strip $(FFLAGS)),,$(error FFLAGS is required for this target))

.PHONY: intel amd nvidia gnu llvm cray clean run

intel:
	$(MKDIR_CMD)
	$(REQ_FFLAGS)
	ifx -O2 -fiopenmp $(FFLAGS) -module $(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

amd:
	@echo "AMD test is not setup"

nvidia:
	$(MKDIR_CMD)
	nvfortran -O2 -mp=gpu $(FFLAGS) -module $(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

gnu:
	$(MKDIR_CMD)
	$(REQ_FFLAGS)
	gfortran -O2 -fopenmp $(FFLAGS) -J$(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

llvm:
	$(MKDIR_CMD)
	$(REQ_FFLAGS)
	flang -O2 -fopenmp $(FFLAGS) -fopenmp-version=52 -J$(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

cray:
	$(MKDIR_CMD)
	ftn -D_INDEX_ -h omp -O2 $(FFLAGS) -J $(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

clean:
	rm -rf ./$(BUILD_DIR)

run:
	OMP_TARGET_OFFLOAD=mandatory $(BUILD_DIR)/$(TARGET)
