.PHONY: build
build:
	docker compose build --build-arg CLAILS_BRANCH=${CLAILS_BRANCH}

.PHONY: rebuild
rebuild:
	docker compose build --no-cache --build-arg CLAILS_BRANCH=${CLAILS_BRANCH}

.PHONY: up down
up:
	docker compose up -d

down:
	docker compose down

.PHONY: restart
restart:
	docker compose restart

.PHONY: logs
logs:
	docker compose logs -f dogatto-app

.PHONY: logs.mysql
logs.mysql:
	docker compose logs -f mysql

.PHONY: logs.redis
logs.redis:
	docker compose logs -f redis

.PHONY: console
console:
	docker compose run --rm -it --entrypoint /bin/bash dogatto-app

.PHONY: db.create db.migrate db.rollback db.seed
db.create:
	docker compose run --rm --entrypoint clails dogatto-app db:create

db.migrate:
	docker compose run --rm --entrypoint clails dogatto-app db:migrate

db.rollback:
	docker compose run --rm --entrypoint clails dogatto-app db:rollback

db.seed:
	docker compose run --rm --entrypoint clails dogatto-app db:seed

.PHONY: db.test.create db.test.migrate
db.test.create:
	docker compose run --rm -e CLAILS_ENV=test --entrypoint clails dogatto-app db:create

db.test.migrate:
	docker compose run --rm -e CLAILS_ENV=test --entrypoint clails dogatto-app db:migrate

.PHONY: test
test:
	docker compose exec -e CLAILS_ENV=test dogatto-app clails test


.PHONY: front-dev front-build front-preview
front-dev:
	cd front && npm run dev

front-build:
	cd front && npm run build

front-preview:
	cd front && npm run preview

