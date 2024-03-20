from functools import cached_property
import json
import re
from typing import Iterable, List
from dataclasses import asdict

from pydantic.dataclasses import dataclass
from redis import Redis
from redis.commands.search.field import TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
import rapidfuzz

from services.core.redis import does_index_exist


@dataclass
class IRIWithLabels:
    IRI: str
    labels: List[str]


class LabelsStore:
    SCHEMA = (
        TagField("$.labels.*", as_name="labels"),
        TagField("$.IRI", as_name="IRI"),
    )

    def __init__(
        self,
        redis_client: Redis,
        key_prefix: str,
        index_name,
        bindings: Iterable[IRIWithLabels],
    ):
        # TODO: validate the bindings are not None
        self.redis_client = redis_client
        self.key_prefix = key_prefix
        self.index_name = index_name
        self._bindings = bindings

    @cached_property
    def bindings(self):
        return list(self._bindings)

    def _create_index(self):
        pipeline = self.redis_client.pipeline()
        for i, binding in enumerate(self.bindings):
            name = self.key_prefix + str(i)
            pipeline.json().set(name, "$", asdict(binding))
        pipeline.execute()

        definition = IndexDefinition(
            prefix=[self.key_prefix], index_type=IndexType.JSON
        )
        self.redis_client.ft(self.index_name).create_index(
            fields=self.SCHEMA, definition=definition
        )

    def _all_labels(self):
        if not does_index_exist(self.redis_client, self.index_name):
            self._create_index()

        # TODO: accumulate pages from Redis to ensure all labels are retrieved
        docs = (
            self.redis_client.ft(self.index_name)
            .search(Query("*").return_field("$.labels", as_field="labels_serialized").paging(0, 10000))
            .docs
        )
        labels = [label for doc in docs for label in json.loads(doc.labels_serialized)]
        return list(set(labels))

    def _lookup_iris(self, label: str):
        if not does_index_exist(self.redis_client, self.index_name):
            self._create_index()

        docs = (
            self.redis_client.ft(self.index_name)
            .search(
                Query("@labels:{{{label}}}".format(label=re.escape(label))).return_field("IRI")
            )
            .docs
        )
        iris = [str(doc.IRI) for doc in docs]
        return list(set(iris))

    def link_entity(self, name: str):
        choices = self._all_labels()
        closest_label, _, _ = rapidfuzz.process.extractOne(
            name,
            choices,
            scorer=rapidfuzz.fuzz.WRatio,
            processor=rapidfuzz.utils.default_process,
        )
        return self._lookup_iris(closest_label)

    def lookup_labels(self, IRI: str):
        if not does_index_exist(self.redis_client, self.index_name):
            self._create_index()

        labels = [
            label
            for doc in self.redis_client.ft(self.index_name)
            .search(
                Query("@IRI:{{iri}}".format(iri=IRI)).return_field(
                    "$.labels", as_field="labels_serialized"
                )
            )
            .docs
            for label in json.loads(doc.labels_serialized)
        ]
        return list(set(labels))
