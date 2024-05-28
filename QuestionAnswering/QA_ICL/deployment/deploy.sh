docker compose -f docker-compose.yaml up -d --build
docker exec -it fastapi_app sh ingest.sh