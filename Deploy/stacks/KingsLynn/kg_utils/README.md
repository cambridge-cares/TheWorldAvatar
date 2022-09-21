# Knowledge Graph helper scripts

## Spinning up a password protected Blazegraph as Docker container

Create file called `bg_password` in `secrets` subfolder (relative to location where this README and the `docker-compose.yml` are located). Populate this file with a single word password, which you want to set.

Spin up Docker container using the provided `docker-compose.yml` file.This shall bring up Blazegraph at endpoint http://localhost:4999/blazegraph/ .
```
docker-compose -f "docker-compose.yml" up
```

## Exporting online Blazegraph namespace

The `export_triples.py` script can be used to export all triples from a specified Blazegraph endpoint and serialse them as Turtle in the `outputs` folder (file name will be `triples.ttl`)