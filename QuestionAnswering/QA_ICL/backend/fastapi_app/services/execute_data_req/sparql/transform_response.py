from collections import defaultdict
from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends

from services.rdf_stores import get_rdfStores
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient
from utils.collections import FrozenDict, listofdict2dictoflist


class SparqlResponseTransformer:
    def __init__(
        self,
        stores: Sequence[Cls2NodeGetter],
    ):
        self.cls2getter = {
            cls: getter for store in stores for cls, getter in store.cls2getter.items()
        }

    def transform(
        self,
        sparql_client: SparqlClient,
        var2cls: dict[str, str],
        vars: list[str],
        bindings: list[dict[str, str | float]],
        pkeys: list[str],
    ):
        # TODO: perform aggregate before transform to reduce the complexity of
        # calling __hash__ on frozendict
        for var in vars:
            cls = var2cls.get(var)
            if not cls:
                continue

            data_getter = self.cls2getter.get(cls)
            if not data_getter:
                continue

            retrieved_data = data_getter(
                iris=[binding.get(var) for binding in bindings],
                sparql_client=sparql_client,
            )
            for binding, datum in zip(bindings, retrieved_data):
                if not datum:
                    continue
                binding[var] = FrozenDict.from_dict(datum.model_dump(exclude_none=True))

        pkeys = set(pkeys)
        pkeys = [var for var in vars if var in pkeys]
        non_pkeys = [var for var in vars if var not in pkeys]

        pkeyvals2data: defaultdict[tuple, list] = defaultdict(list)
        for binding in bindings:
            pkeyvals = tuple(binding[k] for k in pkeys if k in binding)
            datum = {k: binding[k] for k in non_pkeys if k in binding}
            pkeyvals2data[pkeyvals].append(datum)

        return [
            {
                **{pkey: val for pkey, val in zip(pkeys, pkeyvals)},
                **listofdict2dictoflist(data),
            }
            for pkeyvals, data in pkeyvals2data.items()
        ]


@cache
def get_sparqlRes_transformer(
    stores: Annotated[tuple[Cls2NodeGetter, ...], Depends(get_rdfStores)]
):
    return SparqlResponseTransformer(stores=stores)
