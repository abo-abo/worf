emacs ?= emacs

update:
	cook :emacs install ace-link hydra swiper zoutline

compile:
	cook :emacs byte_compile worf.el

plain:
	@echo "Using $(shell which $(emacs))..."
	@echo "Version: $(shell $(emacs) --version)"
	$(emacs) -Q -l elpa.el -l targets/interactive.el README.org

clean:
	rm -f *.elc

.PHONY: plain
