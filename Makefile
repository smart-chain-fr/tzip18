SHELL := /bin/bash

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

ifndef LIGO
LIGO=docker run --platform linux/amd64 --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:0.70.1
endif
# ^ use LIGO en var bin if configured, otherwise use Docker

project_root=--project-root .
# ^ required when using packages

compile = $(LIGO) compile contract $(project_root) ./src/$(1) -o ./compiled/$(2) $(3)
# ^ compile contract to michelson or micheline

test = $(LIGO) run test $(project_root) ./test/$(1)
# ^ run given test file

all: clean compile test

compile: ## compile contracts
	@if [ ! -d ./compiled ]; then mkdir ./compiled ; fi
	@echo "Compiling contracts..."
	@$(call compile,fa12.mligo,fa12.tz)
	@$(call compile,fa12.mligo,fa12.json,--michelson-format json)
	@$(call compile,fa2.mligo,fa2.tz)
	@$(call compile,fa2.mligo,fa2.json,--michelson-format json)
	@$(call compile,proxy.mligo,proxy.tz)
	@$(call compile,proxy.mligo,proxy.json,--michelson-format json)

clean: ## clean up
	@rm -rf compiled/*

.PHONY: test
test: ## run tests (SUITE=propose make test)
ifndef SUITE
	@$(call test,proxy.test.mligo)
else
	@$(call test,$(SUITE).test.mligo)
endif
