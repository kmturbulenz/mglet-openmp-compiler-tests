SRC        ?= main.F90
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
	$(MKDIR_CMD)
	$(REQ_FFLAGS)
	amdflang -O2 -fopenmp  $(FFLAGS) -module-dir $(BUILD_DIR) -I $(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

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
	ftn -h omp -O2 $(FFLAGS) -J $(BUILD_DIR) $(SRC) -o $(BUILD_DIR)/$(TARGET)

clean:
	rm -rf ./$(BUILD_DIR)

run:
	OMP_TARGET_OFFLOAD=mandatory $(BUILD_DIR)/$(TARGET)
