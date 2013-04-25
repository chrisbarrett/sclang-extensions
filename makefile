emacs_d = ~/.emacs.d
emacs   = emacs
file    = sclang-ac-mode.el

# Install the package into the emacs elpa directory.
install :
	$(emacs) --batch -q -l package --eval \
	   "(progn                            \
	     (package-initialize)             \
	     (package-install-file \"$(file)\"))"

# Remove installed instances from the emacs elpa directory.
uninstall :
	rm -r $(emacs_d)/elpa/sclang-ac-mode-*

# Delete compiled elisp files.
clean :
	rm *.elc
