import time
import traceback

from services.ingest import load_insert_then_index_args
import ingest_data.entities as entities
import ingest_data.nlq2datareq_examples as nlq2datareq_examples
import ingest_data.properties as properties

ATTEMPT_LIMIT = 5
WAIT_INTERVAL = 15

if __name__ == "__main__":
    args = load_insert_then_index_args()

    for i in range(ATTEMPT_LIMIT):
        print("Attempt num: " + str(i))
        try:
            entities.main(args)
            nlq2datareq_examples.main(args)
            properties.main(args)
            break
        except Exception as e:
            print(traceback.format_exc())
            time.sleep(WAIT_INTERVAL)

    if i >= ATTEMPT_LIMIT:
        print("Ingestion fails after {num} attempts".format(num=ATTEMPT_LIMIT))
    else:
        print("Ingestion successful")
