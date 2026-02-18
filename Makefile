# CLatter Makefile
# Builds a standalone executable using SBCL
# Pure ANSI version - no ncurses dependency

SBCL := sbcl
TARGET := clatter
BUILD_SCRIPT := build.lisp

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(BUILD_SCRIPT) clatter.asd $(wildcard src/*.lisp) $(wildcard src/**/*.lisp)
	$(SBCL) --non-interactive --load $(BUILD_SCRIPT)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)
	rm -rf ~/.cache/common-lisp/sbcl-*//home/glenn/SourceCode/CLatter

# Development: run without building executable
dev:
	$(SBCL) --eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :clatter)" \
	        --eval "(clatter:main)"
