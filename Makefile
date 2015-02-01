EMACS ?= emacs
CASK ?= cask

install:
	${CASK} install

.PHONY: test
test:
	${CASK} exec ert-runner

compile:
	${CASK} build

clean-elc:
	rm -f browse-at-remote.elc
