def spin_inchi_query(inchi_string, spin_number):
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi ?Spin
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:inChI ?Inchi .
    FILTER((?Inchi) = "#INCHI")
    ?speciesIRI OntoSpecies:spinMultiplicity ?Spin .
	FILTER((?Spin) = "#SPIN")
    }

    """
    query = query.replace('#INCHI', inchi_string)
    query = query.replace('#SPIN', spin_number)
    return query