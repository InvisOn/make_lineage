# ==============================================================================
# test_graph.mk — Makefile for exercising a make-to-dot analyzer
# ==============================================================================

CC      := gcc
SRCS    := foo.c bar.c baz.c
OBJS    := $(SRCS:.c=.o)
HEADERS := foo.h bar.h common.h

.PHONY: all clean test lint docs install uninstall check

all: app docs

clean:
	@echo "Cleaning..."

test: check lint
	@echo "All tests passed."

lint: $(OBJS)
	@echo "Linting..."

docs: api-docs user-docs

install: app
	@echo "Installing..."

uninstall:
	@echo "Uninstalling..."

# ── Linear chain ──────────────────────────────────────────────────────────────
app: link
	@echo "Building app..."

link: compile
	@echo "Linking..."

compile: preprocess
	@echo "Compiling..."

preprocess:
	@echo "Preprocessing..."

# ── Diamond dependency ────────────────────────────────────────────────────────
check: unit integration
	@echo "Checking..."

unit: fixtures
	@echo "Running unit tests..."

integration: fixtures
	@echo "Running integration tests..."

fixtures:
	@echo "Setting up fixtures..."

# ── Fan-out ───────────────────────────────────────────────────────────────────
codegen: schema
	@echo "Generating code..."

proto: schema
	@echo "Generating protos..."

docs-schema: schema
	@echo "Generating schema docs..."

schema:
	@echo "Compiling schema..."

# ── Fan-in ────────────────────────────────────────────────────────────────────
release: app docs package sign
	@echo "Releasing..."

package: app
	@echo "Packaging..."

sign: package
	@echo "Signing..."

# ── Source file stubs (satisfy prerequisites without touching the filesystem) ──
foo.c bar.c baz.c:
	@touch $@

# ── Header dependencies ───────────────────────────────────────────────────────
common.h:
	@echo "Generating common.h..." && touch $@

foo.h: common.h
	@echo "Generating foo.h..." && touch $@

bar.h: common.h
	@echo "Generating bar.h..." && touch $@

# ── Pattern rule ──────────────────────────────────────────────────────────────
%.o: %.c $(HEADERS)
	$(CC) -c $< -o $@

# ── Explicit object targets ───────────────────────────────────────────────────
foo.o: foo.c foo.h common.h
	$(CC) -c foo.c -o foo.o

bar.o: bar.c bar.h common.h
	$(CC) -c bar.c -o bar.o

baz.o: baz.c common.h
	$(CC) -c baz.c -o baz.o

# ── Docs sub-graph ────────────────────────────────────────────────────────────
api-docs: $(OBJS)
	@echo "Building API docs..."

user-docs: docs-schema
	@echo "Building user docs..."

# ── Conditional branch ────────────────────────────────────────────────────────
ifeq ($(CI),true)
check: coverage
endif

coverage: unit
	@echo "Generating coverage report..."

# ── Isolated sub-graph ────────────────────────────────────────────────────────
.PHONY: bench bench-setup bench-teardown bench-run

bench: bench-setup bench-run bench-teardown
	@echo "Benchmark complete."

bench-setup:
	@echo "Setting up benchmark environment..."

bench-run: bench-setup
	@echo "Running benchmarks..."

bench-teardown:
	@echo "Tearing down benchmark environment..."

