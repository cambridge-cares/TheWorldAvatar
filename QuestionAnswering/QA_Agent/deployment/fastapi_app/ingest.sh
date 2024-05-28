RETRY_NUM=5
WAIT_INTERVAL=15
n=0

until [ $n -ge $RETRY_NUM ]; do
    echo "Attempt $n"
    python ingest_entities.py && python ingest_examples.py && python ingest_schema.py && break
    n=$((n + 1))
    sleep $WAIT_INTERVAL
done

if [ $n -ge 5 ]; then
    echo "Ingestion fails after 5 attempts"
else
    echo "Ingestion successful"
fi
