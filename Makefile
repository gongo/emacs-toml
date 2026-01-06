export EMACS ?= emacs

test:
	${EMACS} -Q --batch \
		--load toml.el \
		--load toml-test.el \
		-f ert-run-tests-batch-and-exit

test-official:
	${EMACS} -Q --batch \
		--load toml.el \
		--load toml-test-official.el \
		-f ert-run-tests-batch-and-exit
