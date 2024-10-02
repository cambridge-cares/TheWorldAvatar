from typing import Annotated

from fastapi import Depends

from services.rdf_stores.sg_ontop import SGOntopRDFStore, get_sgOntop_rdfStore
from services.rdf_stores.ontocompchem import (
    OntocompchemRDFStore,
    get_ontocompchem_rdfStore,
)
from services.rdf_stores.ontokin import OntokinRDFStore, get_ontokin_rdfStore
from services.rdf_stores.ontomops import OntomopsRDFStore, get_ontomops_rdfStore
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)
from services.rdf_stores.sg_plot import SGPlotRDFStore, get_sgPlot_rdfStore


def get_rdfStores(
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
    ontokin_store: Annotated[OntokinRDFStore, Depends(get_ontokin_rdfStore)],
    ontocompchem_store: Annotated[
        OntocompchemRDFStore, Depends(get_ontocompchem_rdfStore)
    ],
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
    ontomops_store: Annotated[OntomopsRDFStore, Depends(get_ontomops_rdfStore)],
    sgPlot_store: Annotated[SGPlotRDFStore, Depends(get_sgPlot_rdfStore)],
    sgOntop_store:  Annotated[SGOntopRDFStore, Depends(get_sgOntop_rdfStore)],
):
    return (
        ontospecies_store,
        ontokin_store,
        ontocompchem_store,
        ontozeolite_store,
        ontomops_store,
        sgPlot_store,
        sgOntop_store
    )
