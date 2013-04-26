emacs_d    = ~/.emacs.d
emacs      = emacs
emacs_args = --batch -q -l package
src        = sclang-ac-mode.el
tests      = sclang-ac-mode-tests.el

reinstall : uninstall install

# Install the package into the emacs elpa directory.
install :
	$(emacs) $(emacs_args) --eval \
	   "(progn                    \
	     (package-initialize)     \
	     (package-install-file \"$(src)\"))"

# Remove installed instances from the emacs elpa directory.
uninstall :
	rm -r $(emacs_d)/elpa/sclang-ac-mode-*

# Run integration tests.
test:
	$(emacs) $(emacs_args) -l $(tests) -f run-tests

# Delete compiled elisp files.
clean :
	rm *.elc
