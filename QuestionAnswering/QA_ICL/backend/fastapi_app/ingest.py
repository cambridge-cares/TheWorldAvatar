import time

from services.ingest import load_ingest_args
import ingest_entities
import ingest_examples
import ingest_schema

ATTEMPT_LIMIT = 5
WAIT_INTERVAL = 15

if __name__ == "__main__":
    args = load_ingest_args()
    
    for i in range(ATTEMPT_LIMIT):
        print("Attempt num: " + str(i))
        try:
            ingest_entities.main(args)
            ingest_examples.main(args)
            ingest_schema.main(args)
            break
        except Exception as e:
            print(e)
            time.sleep(WAIT_INTERVAL)

    if i >= ATTEMPT_LIMIT:
        print("Ingestion fails after {num} attempts".format(num=ATTEMPT_LIMIT))
    else:
        print("Ingestion successful")