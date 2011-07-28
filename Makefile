PREFIX = /usr/local

document-templates: document-templates.lisp document-templates.asd
	buildapp --output document-templates \
	--eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
	--eval "(push '*default-pathname-defaults* asdf:*central-registry*)" \
	--load-system document-templates \
	--eval "(setq document-templates::*template-directory* #p\"${PREFIX}/share/document-templates/templates/\")" \
	--entry document-templates::main

.PHONY:install
install: document-templates
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
