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
	tar cjvf company-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

elpa: *.el
	@version=`grep -o "Version: .*" company.el | cut -c 10-`; \
	dir=company-$$version; \
	mkdir -p "$$dir"; \
	cp `git ls-files '*.el' | xargs` company-$$version; \
	echo "(define-package \"company\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/company-pkg.el; \
	tar cvf company-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf company-*/ company-*.tar company-*.tar.bz2 *.elc ert.el

clean-all: clean
	@rm loaddefs.el

test:
	${EMACS} -Q -nw ${LOADPATH} -l company-tests.el \
	--eval "(let (pop-up-windows) (ert t))"

test-batch:
	${EMACS} -Q --batch ${LOADPATH} -l company-tests.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

downloads:
	${EMACS} -Q --batch -l ert || \
	${CURL} ${ERT_URL} > ert.el

compile:
	-@${EMACS} -Q --batch ${LOADPATH} -f batch-byte-compile company.el company-*.el

autoloads: loaddefs.el

loaddefs.el: company*.el
	${EMACS} -Q --batch ${LOADPATH} $(AUTOLOAD) $?
