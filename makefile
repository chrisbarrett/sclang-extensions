emacs_d    = ~/.emacs.d
emacs      = emacs
emacs_args = --batch -q -l package
pkg        = sclang-extensions-pkg.el

reinstall : uninstall install

# Install the package into the emacs elpa directory.
install : deps package
	$(emacs) $(emacs_args) --eval                   \
	   "(progn (package-initialize)                 \
		   (package-install-file \"$(pkg)\"))"

# Remove installed instances from the emacs elpa directory.
uninstall :
	rm -r $(emacs_d)/elpa/sclang-extensions*

deps    :; carton install
package :; carton package

# Delete compiled elisp files.
clean   :; rm -f *.elc
