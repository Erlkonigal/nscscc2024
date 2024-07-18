COMPILER = ../../loongarch32r-linux-gnusf-2022-05-20/bin/loongarch32r-linux-gnusf-

GCC = $(COMPILER)gcc
LD = $(COMPILER)ld
OBJCOPY = $(COMPILER)objcopy
OBJDUMP = $(COMPILER)objdump

CFLAGS = -mstrict-align -nostdinc -nostdlib -fno-builtin -mabi=ilp32s -Iinclude
LDFLAGS = -Tlinker.ld

LIB = 
#../bsp/build/bsp.a

BUILD_DIR = build
SRC_DIR = src

IMAGE = 

SRCS = $(shell find $(SRC_DIR) -name "$(IMAGE).*")

all: $(BUILD_DIR)/$(IMAGE).bin $(BUILD_DIR)/$(IMAGE).txt

$(BUILD_DIR)/$(IMAGE).txt: $(BUILD_DIR)/$(IMAGE).elf
	mkdir -p $(BUILD_DIR)
	$(OBJDUMP) -D $< > $@

$(BUILD_DIR)/$(IMAGE).bin: $(BUILD_DIR)/$(IMAGE).elf
	mkdir -p $(BUILD_DIR)
	$(OBJCOPY) -O binary $< $@

$(BUILD_DIR)/$(IMAGE).elf: $(BUILD_DIR)/$(IMAGE).o $(LIB)
	mkdir -p $(BUILD_DIR)
	$(LD) $(LDFLAGS) $^ -o $@

$(BUILD_DIR)/$(IMAGE).o: $(SRCS)
	mkdir -p $(BUILD_DIR)
	$(GCC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)
