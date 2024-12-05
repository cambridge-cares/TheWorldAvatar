from rdflib import RDF, RDFS, SKOS, Namespace
from rdflib.namespace import DefinedNamespace

# External namespaces
QUDT_UNIT = Namespace("http://qudt.org/vocab/unit/")
OM2 = Namespace("http://www.ontology-of-units-of-measure.org/resource/om-2/")
PERIODICTABLE = Namespace("http://www.daml.org/2003/01/periodictable/PeriodicTable#")
GC = Namespace("http://purl.org/gc/")
ONT_GEOSPARQL = Namespace("http://www.opengis.net/ont/geosparql#")
CITYGML_BUILDING = Namespace("http://www.opengis.net/citygml/building/2.0/")
CITYGML_CITYOBJECTGROUP = Namespace(
    "http://www.opengis.net/citygml/cityobjectgroup/2.0/"
)
FUNCTION_GEOSPARQL = Namespace("http://www.opengis.net/def/function/geosparql/")
BIBO = Namespace("http://purl.org/ontology/bibo/")

# TWA chemistry namespaces
ONTOPROVENANCE = Namespace(
    "http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#"
)
ONTOSPECIES = Namespace(
    "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
)
ONTOKIN = Namespace("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#")
ONTOCOMPCHEM = Namespace(
    "http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#"
)
ONTOCRYSTAL = Namespace("http://www.theworldavatar.com/kg/ontocrystal/")
ONTOZEOLITE = Namespace("http://www.theworldavatar.com/kg/ontozeolite/")
ONTOCAPE = Namespace(
    "http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#"
)
ONTOMOPS = Namespace("https://www.theworldavatar.com/kg/ontomops/")

# TWA BIM & GIS namespaces
ONTOBUILDINGSTRUCTURE = Namespace(
    "https://www.theworldavatar.com/kg/ontobuildingstructure/"
)
ONTOCARPARK = Namespace("https://www.theworldavatar.com/kg/ontocarpark/")
LANDPLOT = Namespace("https://www.theworldavatar.com/kg/landplot/")
ONTOBIM = Namespace("https://www.theworldavatar.com/kg/ontobim/")
ONTOBUILTENV = Namespace("https://www.theworldavatar.com/kg/ontobuiltenv/")
ONTOCOMPANY = Namespace("http://www.theworldavatar.com/kg/ontocompany/")
ONTOCHEMPLANT = Namespace("http://www.theworldavatar.com/kg/ontochemplant/")
ONTOPLANNINGREGULATION = Namespace(
    "https://www.theworldavatar.com/kg/ontoplanningregulation/"
)
ONTOPLOT = Namespace("https://www.theworldavatar.com/kg/ontoplot/")
ONTOZONING = Namespace("https://www.theworldavatar.com/kg/ontozoning/")
ONTOUBEMMP = Namespace("https://www.theworldavatar.com/kg/ontoubemmp/")
ONTODISPERSION = Namespace("https://www.theworldavatar.com/kg/ontodispersion/")

PREFIX2NAMESPACE: dict[str, Namespace | type[DefinedNamespace]] = {
    "rdf": RDF,
    "rdfs": RDFS,
    "skos": SKOS,
    "unit": QUDT_UNIT,
    "om": OM2,
    "pt": PERIODICTABLE,
    "gc": GC,
    "bldg": CITYGML_BUILDING,
    "geo": ONT_GEOSPARQL,
    "geof": FUNCTION_GEOSPARQL,
    "grp": CITYGML_CITYOBJECTGROUP,
    "os": ONTOSPECIES,
    "op": ONTOPROVENANCE,
    "okin": ONTOKIN,
    "ocape": ONTOCAPE,
    "occ": ONTOCOMPCHEM,
    "ocr": ONTOCRYSTAL,
    "zeo": ONTOZEOLITE,
    "mops": ONTOMOPS,
    "bs": ONTOBUILDINGSTRUCTURE,
    "carpark": ONTOCARPARK,
    "landplot": LANDPLOT,
    "ontobim": ONTOBIM,
    "obe": ONTOBUILTENV,
    "ontocompany": ONTOCOMPANY,
    "ontochemplant": ONTOCHEMPLANT,
    "ontoplanreg": ONTOPLANNINGREGULATION,
    "ontoplot": ONTOPLOT,
    "ontozoning": ONTOZONING,
    "disp": ONTODISPERSION
}

PREFIX2URI = {k: str(v) for k, v in PREFIX2NAMESPACE.items()}
