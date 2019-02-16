SHELL := /bin/bash
POSIXCUBE_BIN ?= ~/Projects/posixcube/posixcube.sh

# Check that given variables are set and all have non-empty values,
# die with an error otherwise.
#
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
      $(error Undefined $1$(if $2, ($2))))

.PHONY: ghcid
ghcid: clean
	# http://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html
	ghcid --command "cabal new-repl infratelier" --test "main"

.PHONY: buildbackend
buildbackend: clean
	cabal new-configure -O0 -j4
	cabal new-build all

.PHONY: run
run:
	cabal new-run infratelier

.PHONY: repl
repl:
	cabal new-repl

.PHONY: test
test:
	pg_virtualenv -s $(MAKE) fulltest

.PHONY: fulltest
fulltest:
	./bin/setup_database.sh
	stack test

.PHONY: clean
clean:
	cabal new-clean

.PHONY: deb
deb: buildfrontend buildbackend
	dpkg-buildpackage -us -uc

.PHONY: buildfrontend
buildfrontend:
	$(MAKE) -C front build

.PHONY: build-docker-frontend
build-docker-frontend: buildfrontend
	docker build -t infratelier-frontend front

.PHONY: build-docker-backend
build-docker-backend:
	docker build -t infratelier-backend .

.PHONY: build
build: build-docker-backend build-docker-frontend
	@echo "Built done"

.PHONY: push
push:
	docker tag infratelier-backend registry.gitlab.com/flogfr/infratelier/infratelier-backend
	docker push registry.gitlab.com/flogfr/infratelier/infratelier-backend
	docker tag infratelier-frontend registry.gitlab.com/flogfr/infratelier/infratelier-frontend
	docker push registry.gitlab.com/flogfr/infratelier/infratelier-frontend

.PHONY: deploy
deploy:
	kubectl apply -f <( helm template charts/infratelier/ )
