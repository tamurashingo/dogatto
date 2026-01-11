CLAILS_BRANCH ?= develop


.PHONY: build
build:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env build --build-arg CLAILS_BRANCH=${CLAILS_BRANCH}
	chmod +x docker/run-dev.sh

.PHONY: rebuild
rebuild:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env build --no-cache --build-arg CLAILS_BRANCH=${CLAILS_BRANCH}

.PHONY: up down
up:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env up -d

down:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env down

# New commands for main docker-compose.yml
.PHONY: dev-start dev-stop dev-restart dev-logs
dev-start:
	docker compose up -d

dev-stop:
	docker compose down

dev-restart:
	docker compose restart

dev-logs:
	docker compose logs -f

.PHONY: console
console:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env run --rm -it --entrypoint /bin/bash dogatto-app

.PHONY: logs logs.mysql
logs:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env logs -f dogatto-app

logs.mysql:
	docker compose -f docker/docker-compose.dev.yml --env-file docker/dev.env logs -f mysql-dev



.PHONY: db.create db.migrate db.rollback db.seed
db.create:
	docker compose run --rm --entrypoint clails dogatto-app db:create

db.migrate:
	docker compose run --rm --entrypoint clails dogatto-app db:migrate

db.rollback:
	docker compose run --rm --entrypoint clails dogatto-app db:rollback

db.seed:
	docker compose run --rm --entrypoint clails dogatto-app db:seed

