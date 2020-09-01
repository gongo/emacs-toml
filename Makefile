export EMACS ?= emacs

test:
	${EMACS} -Q --batch \
		--load toml.el \
		--load toml-test.el \
		-f ert-run-tests-batch-and-exit
