from collections import defaultdict
from functools import cache
from importlib.resources import files
import json
from typing import Annotated, Dict, Iterable, List, Literal, Optional, Tuple

from fastapi import Depends
import numpy as np
from pydantic import TypeAdapter
import rapidfuzz
from redis import Redis
from redis.commands.search.field import TextField, VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
from pydantic.dataclasses import dataclass
import regex

from config import QAEngineName, get_qa_engine_name
from core.embed import IEmbedder, get_embedder
from core.redis import does_index_exist, get_redis_client
from services.utils.itertools_recipes import batched

EntityIRI = str
EntityLabel = str
ELStrategy = Literal["fuzzy", "semantic"]


@dataclass(frozen=True)
class ELConfigEntry:
    clsname: str
    el_strategy: ELStrategy


@dataclass
class LexiconEntry:
    iri: str
    clsname: str
    label: str
    surface_forms: List[str]


class EntityLinker:
    LEXICON_FILE_SUFFIX = "_lexicon.json"
    KEY_PREFIX = "entities:"
    INDEX_NAME = "idx:entities"

    @classmethod
    def _load_lexicon(cls, config: Tuple[ELConfigEntry, ...]):
        return [
            LexiconEntry(
                iri=obj["iri"],
                clsname=entry.clsname,
                label=obj["label"],
                surface_forms=obj["surface_forms"],
            )
            for entry in config
            for obj in json.loads(
                files("resources.lexicon")
                .joinpath(entry.clsname + cls.LEXICON_FILE_SUFFIX)
                .read_text()
            )
        ]

    @classmethod
    def _insert_entries_and_create_index(
        cls,
        redis_client: Redis,
        embedder: IEmbedder,
        entries: Iterable[LexiconEntry],
    ):
        offset = 0
        vector_dim = None
        for chunk in batched(entries, 10):
            chunk = list(chunk)
            embeddings = (
                embedder([", ".join(entry.surface_forms) for entry in chunk])
                .astype(np.float32)
                .tolist()
            )
            if vector_dim is None:
                vector_dim = len(embeddings[0])

            pipeline = redis_client.pipeline()
            for i, (entry, embedding) in enumerate(zip(chunk, embeddings)):
                redis_key = cls.KEY_PREFIX + str(offset + i)
                if entry.label not in entry.surface_forms:
                    entry.surface_forms.append(entry.label)
                doc = dict(
                    iri=entry.iri,
                    clsname=entry.clsname,
                    label=entry.label,
                    surface_forms=entry.surface_forms,
                    surface_forms_embedding=embedding,
                )
                pipeline.json().set(redis_key, "$", doc)
            pipeline.execute()

            offset += len(chunk)

        if vector_dim is None:
            raise ValueError(
                f"Index {cls.INDEX_NAME} is not found and must be created. Therefore, `entries` must not be None."
            )

        schema = (
            TagField("$.iri", as_name="iri"),
            TagField("$.clsname", as_name="clsname"),
            TextField("$.label", as_name="label"),
            TextField("$.surface_forms.*", as_name="surface_forms"),
            VectorField(
                "$.surface_forms_embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(prefix=[cls.KEY_PREFIX], index_type=IndexType.JSON)
        redis_client.ft(cls.INDEX_NAME).create_index(
            fields=schema, definition=definition
        )

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        config: Tuple[ELConfigEntry, ...],
    ):
        if not does_index_exist(redis_client, self.INDEX_NAME):
            lexicon = self._load_lexicon(config)
            self._insert_entries_and_create_index(
                redis_client=redis_client,
                embedder=embedder,
                entries=lexicon,
            )

        self.redis_client = redis_client
        self.embedder = embedder
        self.clsname2strategy: Dict[str, ELStrategy] = {
            entry.clsname: entry.el_strategy for entry in config
        }

    def link_exact(self, surface_form: str) -> List[str]:
        """Performs exact matching over canonical labels."""
        inverse_label_query = (
            Query('@label:"{label}"'.format(label=surface_form))
            .return_field("$.iri", as_field="iri")
            .dialect(2)
        )
        res = self.redis_client.ft(self.INDEX_NAME).search(inverse_label_query)
        return [doc.iri for doc in res.docs]

    def _make_filter_query(self, clsname: Optional[str]):
        if clsname is None:
            return "*"
        else:
            return "@clsname:{{{clsname}}}".format(
                clsname=regex.escape(clsname, special_only=False)
            )

    def link_semantic(
        self, surface_form: str, clsname: Optional[str] = None, k: int = 3
    ):
        """Performs vector similarity search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form)

        k -= len(iris)
        if k >= 0:
            encoded_query = self.embedder([surface_form])[0].astype(np.float32)
            knn_query = (
                Query(
                    "({filter_query})=>[KNN {k} @vector $query_vector AS vector_score]".format(
                        filter_query=self._make_filter_query(clsname), k=k
                    )
                )
                .sort_by("vector_score")
                .return_field("$.iri", as_field="iri")
                .dialect(2)
            )
            res = self.redis_client.ft(self.INDEX_NAME).search(
                knn_query, {"query_vector": encoded_query.tobytes()}
            )
            iris.extend(doc.iri for doc in res.docs)

        return iris

    def _all_surface_forms(self, clsname: Optional[str]):
        # TODO: accumulate pages from Redis to ensure all labels are retrieved
        query = (
            Query(self._make_filter_query(clsname))
            .return_field("$.iri", as_field="iri")
            .return_field(
                "$.surface_forms", as_field="surface_forms_serialized"
            ).paging(0, 10000)
        )
        res = self.redis_client.ft(self.INDEX_NAME).search(query)
        # return [
        #     sf for doc in res.docs for sf in json.loads(doc.surface_forms_serialized)
        # ]
        return [(doc.iri, json.loads(doc.surface_forms_serialized)) for doc in res.docs]

    def _lookup_iris(self, surface_form: str) -> List[str]:
        query = Query(
            '@surface_forms:"{surface_form}"'.format(
                surface_form=regex.escape(surface_form, special_only=False)
            )
        ).return_field("iri")
        print(query._query_string)
        res = self.redis_client.ft(self.INDEX_NAME).search(query)
        iris = [doc.iri for doc in res.docs]
        return list(set(iris))

    def link_fuzzy(self, surface_form: str, clsname: Optional[str] = None, k: int = 3):
        """Performs fuzzy search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form)

        k -= len(iris)
        if k >= 0:
            # TODO: use a strategy to retrieve a subset of surface forms rather than all
            # choices = self._all_surface_forms(clsname)
            iri_and_sfs = self._all_surface_forms(clsname)

            sf2iris = defaultdict(list)
            for iri, sfs in iri_and_sfs:
                for sf in sfs:
                    sf2iris[sf].append(iri)
            choices = [sf for _, sfs in iri_and_sfs for sf in sfs]

            lst = rapidfuzz.process.extract(
                surface_form,
                choices,
                scorer=rapidfuzz.fuzz.WRatio,
                limit=k,
                processor=rapidfuzz.utils.default_process,
            )
            surface_forms = [sf for sf, _, _ in lst]
            # iris.extend([iri for sf in surface_forms for iri in self._lookup_iris(sf)])
            for sf in surface_forms:
                iris.extend(sf2iris[sf])

        return list(set(iris))

    def link(self, surface_form: str, clsname: Optional[str] = None, k: int = 3):
        strategy = self.clsname2strategy.get(clsname, "fuzzy") if clsname else "fuzzy"

        if strategy == "fuzzy":
            return self.link_fuzzy(surface_form, clsname, k)
        else:
            return self.link_semantic(surface_form, clsname, k)

    def lookup_label(self, iri: str) -> Optional[str]:
        query = Query(
            "@iri:{{{iri}}}".format(iri=regex.escape(iri, special_only=False))
        ).return_field("$.label", as_field="label")
        res = self.redis_client.ft(self.INDEX_NAME).search(query)
        return res.docs[0].label if len(res.docs) > 0 else None


@cache
def get_el_config(qa_engine: Annotated[QAEngineName, Depends(get_qa_engine_name)]):
    adapter = TypeAdapter(Tuple[ELConfigEntry, ...])
    return adapter.validate_json(
        files("resources." + qa_engine.value).joinpath("el_config.json").read_text()
    )


@cache
def get_entity_linker(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    config: Annotated[Tuple[ELConfigEntry, ...], Depends(get_el_config)],
):
    return EntityLinker(redis_client=redis_client, embedder=embedder, config=config)
