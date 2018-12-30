POSIXCUBE_BIN ?= ~/Projects/posixcube/posixcube.sh

.PHONY: ghcid
ghcid: clean
	# http://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html
	ghcid --command "cabal new-repl haskraft" --test "main"

.PHONY: build
build:
	cabal new-configure -O0 -j4
	cabal new-build all

.PHONY: run
run:
	cabal new-run haskraft

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
deb: clean
	dpkg-buildpackage -us -uc

.PHONY: deploybaseconfiguration
deploybaseconfiguration:
	${POSIXCUBE_BIN} -u root -h haskraft.fr -c ./cubes/base_configuration

.PHONY: deploydehydrated
deploydehydrated:
	${POSIXCUBE_BIN} -u root -h haskraft.fr -e ./env/production.env -c ./cubes/dehydrated

.PHONY: deployfirewall
deployprimarydb:
	@echo "Deploying firewall on ${ENV_NAME}"
	${POSIXCUBE_BIN} -u flog -h ${DOMAIN} -e ./env/haskraft.${ENV_NAME}.env -c ./cubes/debian_firewall

.PHONY: deployprimarydb
deployprimarydb:
	@echo "Deploying primary DB on ${ENV_NAME}"
	${POSIXCUBE_BIN} -u flog -h ${DOMAIN} -e ./env/haskraft.${ENV_NAME}.env -c ./cubes/postgresql

.PHONY: deploynginx
deploynginx:
	@echo "Setup nginx on ${ENV_NAME}"
	${POSIXCUBE_BIN} -u flog -h ${DOMAIN} -e ./env/haskraft.${ENV_NAME}.env -c ./cubes/nginx

BACKEND_BIN=$(stack exec -- which backend-exe)

.PHONY: configure
configure:
	@echo "Configuring server on ${ENV_NAME}"
	${POSIXCUBE_BIN} -u flog -h ${DOMAIN} -e ./env/haskraft.${ENV_NAME}.env -c ./cubes/configure

.PHONY: deploy
deploy:
	@echo "Building the debian package"
	dpkg-buildpackage -us -uc
	@echo "Deploying on ${ENV_NAME}"
