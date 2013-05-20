TEMPLATES_REPO = git@pwgl.homeip.net:repos/document-templates-data.git

document-templates: document-templates.asd document-templates.lisp package.lisp version.lisp-expr
	sbcl --script scripts/build.lisp

.PHONY: clean
clean:
	git clean -f -x -d

.PHONY: checkout-templates
checkout-templates:
	git clone ${TEMPLATES_REPO} templates
