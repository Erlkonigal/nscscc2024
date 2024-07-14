COMPILER = ../loongarch32r-linux-gnusf-2022-05-20/bin/loongarch32r-linux-gnusf-
BUILD_DIR = build
SRC_DIR = src

GCC = $(COMPILER)gcc
AR = $(COMPILER)ar

CFLAGS = -mstrict-align -nostdinc -nostdlib -fno-builtin -mabi=ilp32s -Iinclude

SRCS = $(shell find src -type f)
OBJS = $(addprefix $(BUILD_DIR)/, $(addsuffix .o, $(basename $(SRCS))))

$(BUILD_DIR)/bsp.a: $(OBJS)
	$(AR) rcs $@ $^

$(BUILD_DIR)/%.o: %.S
	@mkdir -p $(dir $@)
	$(GCC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	$(GCC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)

