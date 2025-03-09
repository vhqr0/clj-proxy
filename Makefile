.PHONY: test
test:
	clj -M:dev:test

.PHONY: lint
lint:
	cljfmt check src
	clj-kondo --lint src
