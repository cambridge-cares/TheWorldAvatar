RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDF_TYPE = RDF + "type"

RDFS = "http://www.w3.org/2000/01/rdf-schema#"
RDFS_LITERAL = RDFS + "Literal"
RDFS_SUBCLASSOF = RDFS + "subClassOf"
RDFS_DOMAIN = RDFS + "domain"
RDFS_RANGE = RDFS + "range"
RDFS_COMMENT = RDFS + "comment"
RDFS_ISDEFINEDBY = RDFS + "isDefinedBy"

OWL = "http://www.w3.org/2002/07/owl#"
OWL_CLASS = OWL + "Class"
OWL_OBJECTPROPERTY = OWL + "ObjectProperty"
OWL_DATATYPEPROPERTY = OWL + "DatatypeProperty"
OWL_EQUIVALENTCLASS = OWL + "equivalentClass"

OS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
OS_GITCOMMITHASH = OS + "gitCommitHash"

OKIN = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#"

OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"

NAMESPACE2PREFIX = {
    OS: "os",
    OKIN: "okin",
    "http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#": "occ",
    RDF: "rdf",
    RDFS: "rdfs",
    "http://www.w3.org/2001/XMLSchema#": "xsd",
    OWL: "owl",
    "http://www.w3.org/2004/02/skos/core#": "skos",
    OM: "om",
    "http://www.daml.org/2003/01/periodictable/PeriodicTable#": "pt",
    "http://purl.org/gc/": "gc",
    "http://semanticscience.org/resource/": "CHEMINF",
    "http://purl.obolibrary.org/obo/": "CHMO",
}

QUERY_PREFIXES = "\n".join([f"PREFIX {v}: <{k}>" for k, v in NAMESPACE2PREFIX.items()])
