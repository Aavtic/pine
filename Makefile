PREFIX ?= /usr/local/pine
BINARY = pinec
BIN_DIR = $(PREFIX)/bin
LIB_DIR = $(PREFIX)/lib/stdlib
SYMLINK = /usr/local/bin/$(BINARY)

# Added 'clean' to the list
.PHONY: build debug_build install debug_install uninstall clean

build:
	cargo build --release

debug_build:
	touch tools/pinec/src/main.rs
	cargo build

install: build
	sudo install -d $(BIN_DIR)
	sudo install -d $(LIB_DIR)
	sudo install -p -m 755 target/release/$(BINARY) $(BIN_DIR)/$(BINARY)
	sudo cp -rn stdlib/* $(LIB_DIR)/
	sudo ln -sfv $(BIN_DIR)/$(BINARY) $(SYMLINK)

debug_install: debug_build
	sudo install -d $(BIN_DIR)
	sudo install -d $(LIB_DIR)
	sudo install -p -m 755 target/debug/$(BINARY) $(BIN_DIR)/$(BINARY)
	sudo cp -rn stdlib/* $(LIB_DIR)/
	sudo ln -sfv $(BIN_DIR)/$(BINARY) $(SYMLINK)

update_stdlib:
	sudo cp -r stdlib/* $(LIB_DIR)/

clean:
	cargo clean

uninstall:
	sudo rm -f $(BIN_DIR)/$(BINARY)
	sudo rm -rf $(LIB_DIR)
	sudo rm -f $(SYMLINK)
