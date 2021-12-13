probeme: probeme.rkt
	raco exe -o $@ $^

.PHONY: test
test:
	raco test probeme.rkt

.PHONY: clean
clean:
	rm -f probeme
