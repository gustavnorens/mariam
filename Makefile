CC = gcc
CFLAGS = -Wall -Wextra -Werror -g
LDFLAGS = 

SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

SRC = $(wildcard $(SRC_DIR)/*.c)

ABS_OBJ = Abs/Parser.o Abs/Printer.o Abs/Lexer.o Abs/Absyn.o
OBJ = $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR)/%.o, $(SRC)) $(ABS_OBJ)

DEPS = src/Value.h src/Env.h src/Interpret.h Abs/Absyn.h Abs/Parser.h Abs/Printer.h

TARGET = $(BIN_DIR)/interpreter

all: $(TARGET)

$(TARGET): $(OBJ) | $(BIN_DIR)
	$(CC) $(LDFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c $(DEPS) | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJ_DIR) $(BIN_DIR):
	mkdir -p $@

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

.PHONY: all clean
