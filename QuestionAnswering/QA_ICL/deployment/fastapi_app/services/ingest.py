import importlib.resources
import json
import math
import os
from typing import Callable, List, Tuple, Type, TypeVar

from pydantic import BaseModel, TypeAdapter
from redis import Redis
from redis.commands.search.field import Field
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from tqdm import tqdm

from utils.itertools_recipes import batched


UT = TypeVar("UT", bound=BaseModel)
PT = TypeVar("PT", bound=BaseModel)


class DataIngester:
    def __init__(
        self,
        dirname: str,
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
        self.unprocessed_type = unprocessed_type
        self.processed_type = processed_type
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

    def load_processed_data(self, filename: str):
        text = self.data_dir.joinpath(".cache").joinpath(filename).read_text()
        data = self.adapter_list_pt.validate_json(text)

        print("Found cache of processed data for {name}.\n".format(name=filename))
        return data

    def save_processed_data(self, filename: str, data: List[PT]):
        path = self.data_dir.joinpath(".cache").joinpath(filename)
        path = str(path)

        dirpath = os.path.dirname(path)
        if dirpath:
            os.makedirs(dirpath, exist_ok=True)

        with open(str(path), "wb") as f:
            f.write(self.adapter_list_pt.dump_json(data))

    def load_unprocessed_data(self, filename: str):
        print("Loading unprocessed data {cls} into memory...".format(cls=filename))

        data = [
            self.unprocessed_type.model_validate(datum)
            for datum in json.loads(self.data_dir.joinpath(filename).read_text())
        ]

        print("{num} rows of data loaded into memory.\n".format(num=len(data)))
        return data

    def process_data_by_batch(self, data: List[UT]):
        print(
            "Processing data with batch size {batchsize}...".format(
                batchsize=self.process_batchsize
            )
        )

        processed_entries: List[PT] = []
        for batch in tqdm(
            batched(data, self.process_batchsize),
            total=math.ceil(len(data) / self.process_batchsize),
        ):
            processed_entries.extend(self.process_func(batch))

        print("Processing complete.\n")
        return processed_entries

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
            try:
                data = self.load_processed_data(file.name)
            except:
                print(
                    "No cache of processed data for {cls} found.".format(cls=file.name)
                )
                data = self.load_unprocessed_data(file.name)
                data = self.process_data_by_batch(data)
                self.save_processed_data(filename=file.name, data=data)
            offset = self.insert_data(offset=offset, data=data)

        self.index_data()
