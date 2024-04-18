from functools import cached_property
import regex
from typing import Iterable
from dataclasses import asdict

from pydantic.dataclasses import dataclass
from redis import Redis
from redis.commands.search.field import TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
import rapidfuzz

from services.core.redis import does_index_exist


@dataclass
class ShipIdentifier:
    IRI: str
    MMSI: str
    name: str


class ShipLinker:
    KEY_PREFIX = "sg:ships:"
    INDEX_NAME = "idx:sg:ships"

    SCHEMA = (
        TagField("$.name", as_name="name"),
        TagField("$.MMSI", as_name="MMSI"),
        TagField("$.IRI", as_name="IRI"),
    )

    def __init__(self, redis_client: Redis, ships: Iterable[ShipIdentifier]):
        self.redis_client = redis_client
        self._ships = ships

    @cached_property
    def ships(self):
        return list(self._ships)

    def _create_index(self):
        pipeline = self.redis_client.pipeline()
        for i, binding in enumerate(self.ships):
            name = self.KEY_PREFIX + str(i)
            pipeline.json().set(name, "$", asdict(binding))
        pipeline.execute()

        definition = IndexDefinition(
            prefix=[self.KEY_PREFIX], index_type=IndexType.JSON
        )
        self.redis_client.ft(self.INDEX_NAME).create_index(
            fields=self.SCHEMA, definition=definition
        )

    def _all_names(self):
        if not does_index_exist(self.redis_client, self.INDEX_NAME):
            self._create_index()

        # TODO: accumulate pages from Redis to ensure all labels are retrieved
        docs = (
            self.redis_client.ft(self.INDEX_NAME)
            .search(Query("*").return_field("name").paging(0, 10000))
            .docs
        )
        names = [str(doc.name) for doc in docs]
        return list(set(names))

    def lookup_by_name(self, name: str):
        choices = self._all_names()
        closest_name, _, _ = rapidfuzz.process.extractOne(
            name,
            choices,
            scorer=rapidfuzz.fuzz.WRatio,
            processor=rapidfuzz.utils.default_process,
        )

        docs = (
            self.redis_client.ft(self.INDEX_NAME)
            .search(
                Query(
                    "@name:{{{name}}}".format(
                        name=regex.escape(closest_name, special_only=False)
                    )
                ).return_fields("IRI", "MMSI", "name")
            )
            .docs
        )

        return [
            ShipIdentifier(IRI=doc.IRI, MMSI=doc.MMSI, name=doc.name) for doc in docs
        ]

    def lookup_by_mmsi(self, MMSI: str):
        if not does_index_exist(self.redis_client, self.INDEX_NAME):
            self._create_index()

        docs = (
            self.redis_client.ft(self.INDEX_NAME)
            .search(
                Query(
                    "@MMSI:{{{mmsi}}}".format(
                        mmsi=regex.escape(MMSI, special_only=False)
                    )
                )
                .return_fields("IRI", "MMSI", "name")
                .paging(0, 1)
            )
            .docs
        )

        if not docs:
            return None

        doc = docs[0]
        print("find: ", MMSI, "; found:", doc)
        return ShipIdentifier(IRI=doc.IRI, MMSI=doc.MMSI, name=doc.name)
