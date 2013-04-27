CARTON     = carton
EMACS      = emacs --batch -q -l package
EMACS_D    = $(shell $(EMACS) --eval '(princ (expand-file-name user-emacs-directory))')
VERSION    = $(shell carton version)

PACKAGE_DIR = sclang-extensions-$(VERSION)
PACKAGE_TAR = $(abspath sclang-extensions-$(VERSION).tar)
MANIFEST    = $(abspath sclang-extensions-pkg.el)
SRCS        = $(filter-out $(wildcard *-pkg.el), $(wildcard *.el))
PACKAGE_INCLUDES = $(SRCS) $(MANIFEST)

LOAD_EL     = $(patsubst %,-l %, $(SRCS))
TEST_RUNNER = $(abspath test/test-runner.el)

# ============================================================================

.PHONY: default
default : uninstall deps install clean-package

# Installs the package to .emacs.d/elpa
.PHONY : install
install : package
	$(EMACS) -f package-initialize \
		--eval "(package-install-file \"$(PACKAGE_TAR)\")"

# Deletes all installed instances in .emacs.d/elpa
.PHONY : uninstall
uninstall :
	rm -rf $(EMACS_D)elpa/sclang-extensions-*

# Install package dependencies.
.PHONY : deps
deps :
	$(CARTON) install
	$(CARTON) update

# ----------------------------------------------------------------------------
# Cleaning tasks

.PHONY: clean
clean : clean-elc clean-deps clean-package

.PHONY: clean-elc
clean-elc :
	rm -f *.elc

.PHONY: clean-deps
clean-deps :
	rm -rf elpa

.PHONY: clean-package
clean-package :
	rm -rf $(PACKAGE_DIR) $(MANIFEST) $(PACKAGE_TAR)

# ----------------------------------------------------------------------------
# Build tasks

# Create a package tar and clean up.
.PHONY: package
package : clean-package $(MANIFEST) $(PACKAGE_INCLUDES)
	mkdir -p  $(PACKAGE_DIR)
	cp    -f  $(PACKAGE_INCLUDES) $(PACKAGE_DIR)
	tar   cf  $(PACKAGE_TAR) $(PACKAGE_DIR)
	rm    -rf $(PACKAGE_DIR)

# Generate package file
$(MANIFEST) :
	$(CARTON) package

# Byte-compile Elisp files
%.elc : .%el
	$(CARTON) exec $(EMACS) $(LOAD_EL) -f batch-byte-compile $<

# ----------------------------------------------------------------------------
# Tests

.PHONY: test
test :
	$(CARTON) exec $(EMACS) -l $(TEST_RUNNER) -f 'scl:run-tests-batch'
