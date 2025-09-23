docker compose -f docker-compose.yaml -p marie up -d --build
docker exec -it fastapi_app python -m ingest_data --redis_host redis-stack --text_embedding_backend triton --text_embedding_url "triton:8001"
