docs:
	elm make --optimize --docs=docs.json

.PHONY: examples

examples:
	$(MAKE) -C examples

publish: examples
	cd examples/gh-pages; git add .; git ci -mupdate; git push
