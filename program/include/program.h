#pragma once

#include "../../bsp/include/uart.h"
#include "../../bsp/include/bsp.h"

#define LENGTH(arr) (sizeof(arr) / sizeof(arr[0]))
#define check(expr) assert(expr)
#define bool int