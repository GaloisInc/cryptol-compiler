# Minimal makefile for Sphinx documentation

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = src
BUILDDIR      = build
CABAL         = $(shell which cabal)

.PHONY: github help Makefile

github:
	cd .. && ./scripts/haddock-install && cd manual
	make html
	mkdir -p ../docs
	cp -r build/html/* ../docs

help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#
# After extracting the examples and building the docs, we run daedalus
# on the extracted example files to ensure that they're valid. This assumes daedalus
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

