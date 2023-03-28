from typing import Iterable

from agent.kgutils import KGClient
from tests.integration_test.testconsts import prefix
from . import testconsts as C


def triples_to_insert_query(triples: Iterable[str]):
    return prefix + "INSERT DATA {\n" + '\n'.join(triples) + '}'


def init_kg_client(kg_client: KGClient, init_assets: Iterable[str]):
    triples = [C.SAMPLE_ONTOBIM_TRIPLESTORE[asset]
               for asset in init_assets
               if asset in C.SAMPLE_ONTOBIM_TRIPLESTORE]

    kg_client.execute_update(triples_to_insert_query(triples))
