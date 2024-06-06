from argparse import ArgumentParser
from importlib import resources

from pydantic import TypeAdapter
from redis import Redis


from model.qt_recog import QTRECOG_EXAMPLES_KEY_PREFIX, QtRecogExample


QTRECOG_EXAMPLES_DIRNAME = "qtRecog_examples"


def main(redis_host: str, purge: bool):
    redis_client = Redis(host=redis_host, decode_responses=True)

    if not purge and redis_client.exists(QTRECOG_EXAMPLES_KEY_PREFIX + "0"):
        print("Examples already exist. Skip insertion.")
        return

    if purge:
        print("Deleting existing examples...")
        redis_client.eval(
            "return redis.call('del', unpack(redis.call('keys', ARGV[1])))",
            0,
            "prefix:*",
        )
        print("Done")

    print("Discovering example files...")
    files = [
        f
        for f in resources.files("data").joinpath(QTRECOG_EXAMPLES_DIRNAME).iterdir()
        if f.is_file() and f.name.endswith(".json")
    ]
    print("{num} files discovered.\n".format(num=len(files)))

    adapter = TypeAdapter(list[QtRecogExample])
    offset = 0
    print("Inserting data into Redis...")
    for file in files:
        data = adapter.validate_json(file.read_text())

        pipeline = redis_client.pipeline()
        for i, datum in enumerate(data):
            pipeline.set(
                QTRECOG_EXAMPLES_KEY_PREFIX + str(i + offset), datum.model_dump_json()
            )
        pipeline.execute()

        offset += len(data)
    print("Done")


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--redis_host", help="Hostname of Redis server")
    parser.add_argument(
        "--purge",
        action="store_true",
        help="Whether to delete all existing examples before re-insert them",
        default=False,
    )
    args = parser.parse_args()

    main(redis_host=args.redis_host, purge=args.purge)
