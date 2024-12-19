#!/usr/bin/env just --justfile
# SETTINGS
# set dotenv-load := true
export DB_PASSWORD:="mySuperSecretPassword!" # Remember to update the DATABASE_URL
export DB_PORT:="6433" # Remember to update the DATABASE_URL
export DB_TAG:="wisp_kv_sessions"
export DB_HOST:="127.0.0.1"
export DB_USER:="postgres"
export DB_NAME:="postgres"
# export DATABASE_URL :="postgres://postgres:mySuperSecretPassword!@localhost:6433/postgres?sslmode=disable"

watch_test:
    @just db_run &>/dev/null&
    @just wait-for-db
    watchexec --restart --verbose --clear --wrap-process=session --stop-signal SIGTERM --exts gleam --watch ./ -- "gleam test"

test:
    @just db_run &>/dev/null&
    @just wait-for-db
    gleam test


# DB
db_run:
    docker run \
     --rm \
     -p $DB_HOST_PORT:5432 \
     -e POSTGRES_PASSWORD=$DB_PASSWORD \
     -e POSTGRES_USER=$DB_USER \
     --name $DB_TAG \
     postgres

wait-for-db:
    until PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -U "$DB_USER" -p "$DB_PORT" -c '\q' 2>/dev/null; do echo "Waiting for database..."; sleep 2; done; echo "Database is up!"

