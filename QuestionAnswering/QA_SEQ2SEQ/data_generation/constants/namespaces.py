RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
OWL = "http://www.w3.org/2002/07/owl#"
XSD = "http://www.w3.org/2001/XMLSchema#"
SKOS = "http://www.w3.org/2004/02/skos/core#"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
GC = "http://purl.org/gc/"
DABGEO = "http://www.purl.org/oema/infrastructure/"

OS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
OKIN = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#"
OCC = "http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#"
OBE = "https://www.theworldavatar.com/kg/ontobuiltenv/"


NAMESPACE2PREFIX = {
    RDF: "rdf",
    RDFS: "rdfs",
    OWL: "owl",
    XSD: "xsd",
    SKOS: "skos",
    GC: "gc",
    DABGEO: "dabgeo",
    OM: "om",
    OS: "os",
    OKIN: "okin",
    OCC: "occ",
    "http://www.daml.org/2003/01/periodictable/PeriodicTable#": "pt",
    "http://semanticscience.org/resource/": "CHEMINF",
    "http://purl.obolibrary.org/obo/": "CHMO",
}

QUERY_PREFIXES = "\n".join([f"PREFIX {v}: <{k}>" for k, v in NAMESPACE2PREFIX.items()])
