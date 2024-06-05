from twa.JPSGateway import JPSGateway
from twa.data_model.base_ontology import (
    KnowledgeGraph,
    BaseOntology,
    BaseClass,
    ObjectProperty,
    DataProperty,
    TransitiveProperty,
    as_range,
)
from twa.kg_operations import (
    PySparqlClient,
)
from twa.data_model.iris import TWA_BASE_URL

__version__ = "0.0.2a0"
