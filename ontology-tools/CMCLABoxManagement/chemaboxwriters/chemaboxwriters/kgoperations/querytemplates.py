from chemaboxwriters.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from chemaboxwriters.kgoperations.querykg import querykg

def spec_inchi_query(inchi_string):
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:inChI ?Inchi .
    FILTER REGEX(str(?Inchi), REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(""" + '"' + inchi_string + '"' + """, "InChI=1S", "InChI=1"), "/t.+", ""), "/b.+", ""), "\\\\(", "\\\\\\\\("), "\\\\)", "\\\\\\\\)"), "i")
    }
    """
    #print(query)
    return query

def get_species_iri(inchi):
    #Query OntoSpecies to find Species IRI that corresponds to a given InChI.
    target = None
    results  = querykg(SPARQL_ENDPOINTS['ontospecies'], spec_inchi_query(inchi)) #query_endpoint(endpoint, spec_inchi_query(inchi))
    if results:
        if 'speciesIRI' in results[0].keys():
            target = results[0]['speciesIRI']
    return target


def mop_GBUs(mopIRI):
    """Queries and collects MOP data relating to the GBUs/CBUs.
    As every MOP has two GBUs, returns back information on both. """
    queryStr = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?mopIRI ?MOPFormula ?CBUFormula ?NumberValue ?Planarity ?Modularity ?Symmetry ?MOPReference ?CBUType ?speciesIRI ?OuterCoordination ?CBUFunctionalGroup ?Direction
    WHERE
    {   
    ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
    ?mopIRI OntoMOPs:hasProvenance ?Provenance .
    ?Provenance OntoMOPs:hasReferenceDOI ?MOPReference . 
    ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
    ?AssemblyModel OntoMOPs:hasSymmetryPointGroup ?Symmetry .
    ?AssemblyModel OntoMOPs:hasGenericBuildingUnitNumber ?GBUNumber .
    ?GBUNumber OntoMOPs:isNumberOf ?GBU .
    ?GBU OntoMOPs:hasPlanarity ?Planarity .
    ?GBU OntoMOPs:hasModularity ?Modularity . 
	?GBUNumber OntoSpecies:value ?NumberValue .
    ?mopIRI OntoMOPs:hasChemicalBuildingUnit ?CBU .
    ?CBU OntoMOPs:isFunctioningAs ?GBU .
    ?CBU OntoMOPs:hasCBUFormula ?CBUFormula .
    ?CBU OntoMOPs:hasBindingSite ?CBUBindingSite .
    ?CBU OntoMOPs:hasBindingDirection ?BindingDirection .
    ?BindingDirection rdf:type ?Direction.
    ?CBUBindingSite OntoMOPs:hasOuterCoordinationNumber ?OuterCoordination .
    ?CBUBindingSite rdfs:label ?CBUFunctionalGroup.  
    ?CBUBindingSite rdf:type ?CBUType.
    ?CBU OntoSpecies:hasUniqueSpecies ?speciesIRI .
    FILTER ((?mopIRI) = <#MOPIRI>) .  
    }"""
    queryStr = queryStr.replace('#MOPIRI', str(mopIRI))
    return queryStr

def mop_reference(string, mop_symmetry):
    """Using a MOP formula and Symmetry point group, checks if the MOP exists in the KG."""
    queryStr = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?MOPReference ?MOPFormula ?mopIRI ?Symmetry 
    WHERE
    {   
    ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
    ?mopIRI OntoMOPs:hasProvenance ?Provenance .
    ?Provenance OntoMOPs:hasReferenceDOI ?MOPReference .   
    FILTER ((?MOPFormula) = "#MOPReference").
    ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
    ?AssemblyModel OntoMOPs:hasSymmetryPointGroup ?Symmetry .
    FILTER ((?Symmetry) = "#SYMMETRY").
    }"""
    queryStr = queryStr.replace('#MOPReference', str(string))
    queryStr = queryStr.replace('#SYMMETRY', str(mop_symmetry))   
    return queryStr

def get_assemblyModel(modularity, planarity, gbu_number, symmetry):
    #queries the assembly model of a particular MOP. 
    queryStr = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?AssemblyModel ?NumberValue ?Planarity ?Modularity ?Symmetry
    WHERE
    {   
    ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
    ?mopIRI OntoMOPs:hasProvenance ?Provenance .
    ?Provenance OntoMOPs:hasReferenceDOI ?MOPReference . 
    ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
    ?AssemblyModel OntoMOPs:hasSymmetryPointGroup ?Symmetry .
    ?AssemblyModel OntoMOPs:hasGenericBuildingUnitNumber ?GBUNumber .
    ?GBUNumber OntoMOPs:isNumberOf ?GBU .
    ?GBU OntoMOPs:hasPlanarity ?Planarity .
    ?GBU OntoMOPs:hasModularity ?Modularity . 
	?GBUNumber OntoSpecies:value ?NumberValue .
    FILTER ((?Modularity) = "#MODULARITY") .
    FILTER ((?Planarity) = "#PLANARITY") .
    FILTER ((?NumberValue) = "#NUMBER") .
    FILTER ((?Symmetry) = "#SYMMETRY") .
    }"""
    queryStr = queryStr.replace('#MODULARITY', str(modularity))
    queryStr = queryStr.replace('#PLANARITY', str(planarity))
    queryStr = queryStr.replace('#NUMBER', str(gbu_number))
    queryStr = queryStr.replace('#SYMMETRY', str(symmetry))
    return queryStr

def get_assembly_iri(modularity, planarity, gbu_number, symmetry):
    #Query OntoSpecies to find Species IRI that corresponds to a given InChI.
    target = None
    results  = querykg(SPARQL_ENDPOINTS['ontomops'], get_assemblyModel(modularity, planarity, gbu_number, symmetry))
    if results:
        target = []
        for k in range(len(results)):
            if 'AssemblyModel' in results[k].keys():
                target.append(results[k]['AssemblyModel'])
    return target