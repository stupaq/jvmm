DOCS_MD := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))

all: docs
	
docs: $(DOCS_MD)

$(DOCS_MD): %.pdf : %.md
	pandoc $< -o $@

clean:
	-rm -f $(DOCS_MD)

.PHONY: clean
