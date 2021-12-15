probeme: probeme.rkt req-id.rkt
	raco exe -o $@ $^

.PHONY: build
build: probeme

.PHONY: rebuild
rebuild:
	$(MAKE) clean
	$(MAKE) build

.PHONY: test
test:
	raco test probeme.rkt

.PHONY: clean
clean:
	rm -f probeme
