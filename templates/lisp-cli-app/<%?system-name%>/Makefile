PREFIX = /usr/local

<%?system-name%>: <%?system-name%>.lisp <%?system-name%>.asd
	buildapp --output <%?system-name%> \
	--asdf-path ~/.clnk/asd \
	--asdf-path ~/.nkzs-reg/asd \
	--load-system <%?system-name%> \
	--entry uh::main

.PHONY:test
test: <%?system-name%>
	cucumber

.PHONY:install
install: <%?system-name%>
	install -c <%?system-name%> ${PREFIX}/bin/<%?system-name%>

.PHONY:uninstall
uninstall:
	rm -f ${PREFIX}/bin/<%?system-name%>

.PHONY:clean
clean:
	git clean -f -x -d
