from argparse import ArgumentParser
import importlib.resources
import json
import math
import os
from typing import Callable, Generic, List, Literal, Tuple, Type, TypeVar

from pydantic import BaseModel, TypeAdapter
import pydantic
from redis import Redis
from redis.commands.search.field import Field
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from tqdm import tqdm

from utils.itertools_recipes import batched


class IngestArgs(BaseModel):
    redis_host: str
    text_embedding_server: Literal["triton", "openai"]
    text_embedding_url: str
    drop_index: bool
    invalidate_cache: bool


def load_ingest_args():
    parser = ArgumentParser()
    parser.add_argument("--redis_host", required=True, type=str)
    parser.add_argument(
        "--text_embedding_backend", choices=["triton", "openai"], default="triton"
    )
    parser.add_argument("--text_embedding_url")
    parser.add_argument(
        "--drop_index",
        action="store_true",
        default=False,
        help="Whether to drop existing index in Redis server if exists",
    )
    parser.add_argument(
        "--invalidate_cache",
        action="store_true",
        default=False,
        help="Whether to invalidate on-disk cache of processed data if exists",
    )
    args = parser.parse_args()
    return IngestArgs(
        redis_host=args.redis_host,
        text_embedding_server=args.text_embedding_server,
        text_embedding_url=args.text_embedding_url,
        drop_index=args.drop_index,
        invalidate_cache=args.invalidate_cache,
    )


UT = TypeVar("UT", bound=BaseModel)
PT = TypeVar("PT", bound=BaseModel)


class DataIngester(Generic[UT, PT]):
    def __init__(
        self,
        dirname: str,
        invalidate_cache: bool,
        unprocessed_type: Type[UT],
        processed_type: Type[PT],
        process_func: Callable[[List[UT]], List[PT]],
        process_batchsize: int,
        redis_client: Redis,
        redis_key_prefix: str,
        redis_preinsert_transform: Callable[[List[PT]], List[dict]],
        redis_insert_batchsize: int,
        redis_index_name: str,
        redis_ft_schema: Tuple[Field, ...],
    ):
        self.data_dir = importlib.resources.files("data").joinpath(dirname)
        self.invalidate_cache = invalidate_cache
        self.adapter_list_ut = TypeAdapter(List[unprocessed_type])
        self.adapter_list_pt = TypeAdapter(List[processed_type])
        self.process_func = process_func
        self.process_batchsize = process_batchsize
        self.redis_client = redis_client
        self.redis_key_prefix = redis_key_prefix
        self.redis_preinsert_transform = redis_preinsert_transform
        self.redis_insert_batchsize = redis_insert_batchsize
        self.redis_index_name = redis_index_name
        self.redis_ft_schema = redis_ft_schema

    def discover_data_files(self):
        print("Discovering data files...")
        files = [
            f
            for f in self.data_dir.iterdir()
            if f.is_file() and f.name.endswith(".json")
        ]
        print("{num} data files discovered.\n".format(num=len(files)))
        return files

    def load_processed_data_from_cache(self, filename: str):
        text = self.data_dir.joinpath(".cache").joinpath(filename).read_text()
        data = self.adapter_list_pt.validate_json(text)

        print("Found cache of processed data for {name}.\n".format(name=filename))
        return data

    def load_processed_data(self, filename: str):
        if not self.invalidate_cache:
            try:
                return self.load_processed_data_from_cache(filename)
            except:
                print(
                    "No cache of processed data for {cls} found.".format(cls=filename)
                )

        print("Loading unprocessed data {cls} into memory...".format(cls=filename))
        unprocessed_data = self.adapter_list_ut.validate_json(
            self.data_dir.joinpath(filename).read_text()
        )
        print(
            "{num} rows of data loaded into memory.\n".format(num=len(unprocessed_data))
        )

        print(
            "Processing data with batch size {batchsize}...".format(
                batchsize=self.process_batchsize
            )
        )
        processed_data: List[PT] = []
        for batch in tqdm(
            batched(unprocessed_data, self.process_batchsize),
            total=math.ceil(len(unprocessed_data) / self.process_batchsize),
        ):
            processed_data.extend(self.process_func(batch))
        print("Processing complete.\n")

        print("Saving processed data to on-disk cache...")
        path = self.data_dir.joinpath(".cache").joinpath(filename)
        path = str(path)
        dirpath = os.path.dirname(path)
        if dirpath:
            os.makedirs(dirpath, exist_ok=True)
        with open(str(path), "wb") as f:
            f.write(self.adapter_list_pt.dump_json(processed_data))
        print("Done saving to disk.")

        return processed_data

    def insert_data(self, offset: int, data: List[PT]):
        print(
            "Inserting data into Redis with batch size {batchsize}...".format(
                batchsize=self.redis_insert_batchsize
            )
        )

        for batch in tqdm(
            batched(data, self.redis_insert_batchsize),
            total=math.ceil(len(data) / self.redis_insert_batchsize),
        ):
            pipeline = self.redis_client.pipeline()

            keys = [self.redis_key_prefix + str(offset + i) for i in range(len(batch))]
            docs = self.redis_preinsert_transform(batch)
            for key, doc in zip(keys, docs):
                pipeline.json().set(key, "$", doc)

            pipeline.execute()
            offset += len(batch)

        print("Insertion done.\n")
        return offset

    def index_data(self):
        print("Indexing data...")

        definition = IndexDefinition(
            prefix=[self.redis_key_prefix], index_type=IndexType.JSON
        )
        res = self.redis_client.ft(self.redis_index_name).create_index(
            fields=self.redis_ft_schema, definition=definition
        )

        if res:
            print(res)
            print("Index {index} created\n".format(index=self.redis_index_name))
        else:
            print("FAILURE:", res)

    def ingest(self):
        files = self.discover_data_files()

        offset = 0
        for file in files:
            processed_data = self.load_processed_data(file.name)
            offset = self.insert_data(offset=offset, data=processed_data)

        self.index_data()
