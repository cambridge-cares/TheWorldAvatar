PREFIX_ONTOMOPS = "PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>"
PREFIX_ONTOSPECIES = "PREFIX spec: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>"
PREFIX_ONTOSYN = "PREFIX syn: <https://www.theworldavatar.com/kg/OntoSyn/>"
PREFIX_RDFS = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"

def construct(mop_iri):
    query = f"""
    {PREFIX_ONTOMOPS}
    {PREFIX_ONTOSPECIES}
    {PREFIX_ONTOSYN}
    {PREFIX_RDFS}
    SELECT ?peakLocation ?peakStrength WHERE {{
        ?chemOut syn:isRepresentedBy {mop_iri} ;
                spec:hasFourierTransformSpectrum/spec:hasSpectraGraph ?spectrum .
        ?spectrum spec:hasPeak ?peak .
        ?peak spec:hasX1 ?peakLocation ;
                rdfs:comment ?peakStrength .
    }}
    """

    return query