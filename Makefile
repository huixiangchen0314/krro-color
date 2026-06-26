.PHONY: test jar install deploy clean

test:
	clojure -M:test

jar:
	clojure -T:build jar

install:
	clojure -T:build install

deploy:
	clojure -T:build deploy

clean:
	rm -rf target