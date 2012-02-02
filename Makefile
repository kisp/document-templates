PREFIX = /usr/local

version := $(shell SBCL_HOME=/usr/lib/sbcl sbcl --noinform --end-runtime-options --no-userinit --eval '(with-open-file (in "version.lisp-expr") (princ (read in)) (quit))' --end-toplevel-options)

document-templates: document-templates.lisp document-templates.asd
	buildapp --output document-templates \
	--eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
	--eval "(push '*default-pathname-defaults* asdf:*central-registry*)" \
	--load-system document-templates \
	--eval "(setq document-templates::*template-directory* #p\"${PREFIX}/share/document-templates/templates/\")" \
	--entry document-templates::sbcl-main

.PHONY:install
install: document-templates
	mkdir -p ${PREFIX}/bin
	install document-templates ${PREFIX}/bin/document-templates
	mkdir -p ${PREFIX}/share/document-templates
	cp -r templates ${PREFIX}/share/document-templates

.PHONY:uninstall
uninstall:
	rm -f ${PREFIX}/bin/document-templates
	rm -rf ${PREFIX}/share/document-templates

.PHONY:clean
clean:
	git clean -f -x -d

.PHONY:paco-uninstall
paco-uninstall:
	paco -r --batch document-templates

.PHONY:paco-install
paco-install:
	paco -l -D -- make install

stow-dir := /var/lib/stow
stow-prefix := $(stow-dir)/document-templates-$(version)

.PHONY:stow-uninstall-all
stow-uninstall-all:
	cd $(stow-dir) && ( [ ! -d document-templates* ] || stow -D document-templates* ) && rm -rf document-templates*

.PHONY:stow-install
stow-install:
	cd $(stow-dir) && [ ! -d document-templates* ] || stow -D document-templates*
	mkdir -p $(stow-prefix)/bin
	install document-templates $(stow-prefix)/bin/document-templates
	mkdir -p $(stow-prefix)/share/document-templates
	cp -r templates $(stow-prefix)/share/document-templates
	cd $(stow-dir) && stow document-templates-$(version)
