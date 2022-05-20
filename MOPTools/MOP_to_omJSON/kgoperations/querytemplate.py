def inchi_query(inchi_string):
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:inChI ?Inchi .
    FILTER((?Inchi) = "#INCHI").
    }
    """
    query = query.replace('#INCHI', inchi_string)
    return query

def mop_query(mop_weight, mop_shape, mop_charge):
    query = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX om2: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?MOP_Weight ?ShapeSymbol ?MOP_Charge
    WHERE
    {   
    ?mopIRI OntoSpecies:hasMolecularWeight ?MolecularWeight .
    ?MolecularWeight om2:hasValue ?MolecularWeightValue . 
    ?MolecularWeightValue om2:hasNumericalValue ?MOP_Weight .
    FILTER(?MOP_Weight = "#MOPWEIGHT")
	?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
    ?AssemblyModel OntoMOPs:hasPolyhedralShape ?PolyhedralShape .
    ?PolyhedralShape OntoMOPs:hasSymbol ?ShapeSymbol .
    FILTER(?ShapeSymbol = "#SHAPESYMBOL")
    ?mopIRI OntoSpecies:hasCharge ?Charge .
    ?Charge om2:hasValue ?ChargeValue .
    ?ChargeValue om2:hasNumericalValue ?MOP_Charge .
	FILTER(?MOP_Charge = "#MOPCHARGE")
    }
    """
    query = query.replace('#MOPWEIGHT', mop_weight)
    query = query.replace('#SHAPESYMBOL', mop_shape)
    query = query.replace('#MOPCHARGE', mop_charge)
    return query