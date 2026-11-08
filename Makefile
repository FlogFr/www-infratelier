# Makefile for infratelier.com (Pelican static site).
# Run `make help` for the list of targets.

PY ?= python3
PELICAN ?= pelican
PELICANOPTS ?=

BASEDIR = $(CURDIR)
INPUTDIR = $(BASEDIR)/content
OUTPUTDIR = $(BASEDIR)/output
CONFFILE = $(BASEDIR)/pelicanconf.py
PUBLISHCONF = $(BASEDIR)/publishconf.py

GITHUB_PAGES_BRANCH = gh-pages
PORT ?= 8000

DEBUG ?= 0
ifeq ($(DEBUG), 1)
	PELICANOPTS += -D
endif

.PHONY: help install html clean serve devserver publish check github

help:
	@echo 'Makefile for infratelier.com'
	@echo ''
	@echo '  make install     create .venv and install build dependencies'
	@echo '  make html        build the site (dev settings) into output/'
	@echo '  make devserver   rebuild on change and serve on http://localhost:$(PORT)'
	@echo '  make serve       serve output/ on http://localhost:$(PORT)'
	@echo '  make publish     build with production settings (publishconf.py)'
	@echo '  make check       production build + quality checks (scripts/check_site.py)'
	@echo '  make github      check, then push output/ to the gh-pages branch (manual deploy)'
	@echo '  make clean       remove output/'
	@echo ''
	@echo 'Set DEBUG=1 for verbose Pelican output.'

install:
	$(PY) -m venv .venv
	.venv/bin/pip install -r requirements.txt

html:
	"$(PELICAN)" "$(INPUTDIR)" -o "$(OUTPUTDIR)" -s "$(CONFFILE)" $(PELICANOPTS)

clean:
	[ ! -d "$(OUTPUTDIR)" ] || rm -rf "$(OUTPUTDIR)"

serve:
	"$(PELICAN)" -l "$(INPUTDIR)" -o "$(OUTPUTDIR)" -s "$(CONFFILE)" $(PELICANOPTS) -p $(PORT)

devserver:
	"$(PELICAN)" -lr "$(INPUTDIR)" -o "$(OUTPUTDIR)" -s "$(CONFFILE)" $(PELICANOPTS) -p $(PORT)

publish:
	"$(PELICAN)" "$(INPUTDIR)" -o "$(OUTPUTDIR)" -s "$(PUBLISHCONF)" $(PELICANOPTS) --fatal warnings

check: publish
	$(PY) scripts/check_site.py "$(OUTPUTDIR)"

# Manual deploy. The normal path is a push to main: GitHub Actions
# (.github/workflows/deploy.yml) builds, checks and publishes the site.
github: check
	ghp-import -n -m "Publish site from $$(git rev-parse --short HEAD)" -b $(GITHUB_PAGES_BRANCH) "$(OUTPUTDIR)"
	git push origin $(GITHUB_PAGES_BRANCH)
