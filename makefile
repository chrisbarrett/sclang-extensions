emacs_d = ~/.emacs.d
emacs   = emacs
src     = sclang-ac-mode.el
tests   = sclang-ac-mode-tests.el

reinstall : uninstall install

# Install the package into the emacs elpa directory.
install :
	$(emacs) --batch -q -l package --eval \
	   "(progn                            \
	     (package-initialize)             \
	     (package-install-file \"$(src)\"))"

# Remove installed instances from the emacs elpa directory.
uninstall :
	rm -r $(emacs_d)/elpa/sclang-ac-mode-*

test:
	$(emacs) --batch -q -l $(tests) -f run-tests

# Delete compiled elisp files.
clean :
	rm *.elc
