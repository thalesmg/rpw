UID := $(shell id -u)
DOCKER_IMAGE := thalesmg/rpw_test:v2.0

build: app/Main.hs
	docker-compose -f ext/docker/rpw.yml up -d rpw
	docker exec docker_rpw_1 ./build_docker.sh

.PHONY: test
test: build
	docker exec -e userid=$(UID) docker_rpw_1 /mnt/run_tests
