EMACS=emacs
LOADPATH=-L . -L ../flx
CURL=curl --silent
AUTOLOAD=--eval '(let ((generated-autoload-file (expand-file-name "$@")) \
	(make-backup-files nil)) \
  (mapcar (function update-file-autoloads) command-line-args-left) \
  (save-buffers-kill-emacs t))'
ERT_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3

all: compile autoloads

.PHONY: ert test test-batch compile autoloads

package: *.el
	@ver=`grep -o "Version: .*" company.el | cut -c 10-`; \
	tar cjvf company-$$ver.tar.bz2 --mode 644 $$(find . -name \*.el)

elpa: *.el
	@version=`grep -o "Version: .*" company.el | cut -c 10-`; \
	dir=company-$$version; \
	mkdir -p "$$dir"; \
	cp $$(find . -name \*.el) company-$$version; \
	echo "(define-package \"company\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/company-pkg.el; \
	tar cvf company-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf company-*/ company-*.tar company-*.tar.bz2 *.elc ert.el

clean-all: clean
	@rm loaddefs.el

test:
	${EMACS} -Q -nw ${LOADPATH} -l test/all.el \
	--eval "(let (pop-up-windows) (ert t))"

test-batch:
	${EMACS} -Q --batch ${LOADPATH} -l test/all.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

compile:
	-@${EMACS} -Q --batch ${LOADPATH} -f batch-byte-compile company.el company-*.el

autoloads: loaddefs.el

loaddefs.el: company*.el
	${EMACS} -Q --batch ${LOADPATH} $(AUTOLOAD) $?
