ifndef LIGO
LIGO=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:0.44.0
endif

json=--michelson-format json
tsc=npx tsc

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

install: ## install dependencies
	@if [ ! -f ./.env ]; then cp .env.dist .env ; fi

compile: ## compile contracts
	@if [ ! -d ./compiled ]; then mkdir ./compiled ; fi
	@$(LIGO) compile contract src/main.mligo -o compiled/proxy.tz

.PHONY: test
test: ## test contracts
	@$(LIGO) run test test/main.test.mligo

deploy: ## deploy
	@npx ts-node ./scripts/deploy.ts

sandbox-start: ## start sandbox
	@./scripts/run-sandbox

sandbox-stop: ## stop sandbox
	@docker stop sandbox
