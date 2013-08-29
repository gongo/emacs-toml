CASK ?= cask
export EMACS ?= emacs

test: elpa
	${CASK} exec ${EMACS} -Q --batch \
		--load toml.el \
		--load toml-test.el \
		-f ert-run-tests-batch-and-exit

elpa: Cask
	${CASK} install
	touch $@
