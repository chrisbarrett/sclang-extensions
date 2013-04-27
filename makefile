CARTON     = carton
EMACS      = emacs --batch -q -l package
EMACS_D    = $(shell $(EMACS) --eval '(print user-emacs-directory)')
VERSION    = $(shell carton version)

PACKAGE_DIR = sclang-extensions-$(VERSION)
PACKAGE_TAR = $(abspath sclang-extensions-$(VERSION).tar)
PKG_EL      = $(abspath sclang-extensions-pkg.el)
SRCS        = $(filter-out $(wildcard *-pkg.el), $(wildcard *.el))
PACKAGE_INCLUDES = $(SRCS) $(PKG_EL)

# ============================================================================

.PHONY: default deps uninstall install

default : uninstall deps install clean-package

# Installs the package to .emacs.d/elpa
install : package
	$(EMACS) -f package-initialize \
		--eval "(package-install-file \"$(PACKAGE_TAR)\")"

# Deletes all installed instances in .emacs.d/elpa
uninstall :
	rm -rf $(EMACS_D)/elpa/sclang-extensions-*

# Install package dependencies.
deps :
	$(CARTON) install
	$(CARTON) update

# ----------------------------------------------------------------------------
# Cleaning tasks

.PHONY: clean clean-elc clean-deps clean-package
clean : clean-elc clean-deps clean-package

clean-elc     :; rm -f *.elc
clean-deps    :; rm -rf elpa
clean-package :; rm -rf $(PACKAGE_DIR) $(PKG_EL) $(PACKAGE_TAR)

# ----------------------------------------------------------------------------
# Build tasks

.PHONY: package

# Create a package tar and clean up.
package : clean-package $(PKG_EL) $(PACKAGE_INCLUDES)
	mkdir -p $(PACKAGE_DIR)
	cp -f $(PACKAGE_INCLUDES) $(PACKAGE_DIR)
	tar cf $(PACKAGE_TAR) $(PACKAGE_DIR)
	rm -rf $(PACKAGE_DIR)

# Generate package file
$(PKG_EL) :
	$(CARTON) package

# Byte-compile Elisp files
%.elc : .%el
	$(CARTON) exec $(EMACS) $(patsubst %,-l %, $(SRCS)) -f	\
		batch-byte-compile $<
