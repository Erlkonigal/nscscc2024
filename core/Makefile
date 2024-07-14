PROJECT_DIR = $(shell pwd)
MODULES_DIR = $(PROJECT_DIR)/modules
BUILD_DIR = $(PROJECT_DIR)/build
SIM_DIR = $(PROJECT_DIR)/src
TOP = top
IMAGE =

VERILATOR_FLAGS = --cc --exe --build -j 0 -Wno-UNUSEDSIGNAL -Wno-DECLFILENAME --trace \
	-O3 --x-assign fast --x-initial fast --noassert \
	--timescale "1ns/1ns" --no-timing --autoflush
VERILATOR = verilator

CXXFLAGS = -Iinclude -I$(BUILD_DIR)/obj_dir
CXXFLAGS += $(subst -D__STDC_FORMAT_MACROS,, $(shell llvm-config --cxxflags)) -fPIE -g
CXXFLAGS += -DCONFIG_WAVE

LDFLAGS = $(shell llvm-config --libs) -lreadline

CSRCS = $(shell find $(SIM_DIR) -name "*.cpp" -or -name "*.cc" -or -name "*.c")
VSRCS = $(shell find $(MODULES_DIR) -name "*.v")

verilator:
	@mkdir -p $(BUILD_DIR)
	$(VERILATOR) $(VERILATOR_FLAGS) \
		--top-module $(TOP) \
		$(VSRCS) $(CSRCS) \
		$(addprefix -CFLAGS , $(CXXFLAGS)) $(addprefix -LDFLAGS , $(LDFLAGS)) \
		--Mdir $(BUILD_DIR)/obj_dir \
		-o $(BUILD_DIR)/$(TOP)

verilog:
	mill -i __.runMain Elaborate --target-dir $(MODULES_DIR)

run:
	make -C ../bsp
	make -C ../program IMAGE=$(IMAGE)
	$(BUILD_DIR)/$(TOP) ../program/build/$(IMAGE).bin

clean:
	@rm -rf $(BUILD_DIR)