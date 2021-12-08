'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

def getMOPIRIs():
    """This function collects all MOPs from the KG."""

    queryStr = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?mopIRI ?MOPFormula ?speciesIRI  ?CBUFormula ?NumberValue ?Planarity ?Modularity ?Symbol
    WHERE
    {   
    ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
    ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
    ?AssemblyModel OntoMOPs:hasPolyhedralShape ?PolhedralShape .
    ?PolhedralShape OntoMOPs:hasSymbol ?Symbol .
    ?AssemblyModel OntoMOPs:hasGenericBuildingUnitNumber ?GBUNumber .
    ?GBUNumber OntoMOPs:isNumberOf ?GBU .
    ?GBU OntoMOPs:hasPlanarity ?Planarity .
    ?GBU OntoMOPs:hasModularity ?Modularity . 
	?GBUNumber OntoSpecies:value ?NumberValue .
    ?mopIRI OntoMOPs:hasChemicalBuildingUnit ?CBU .
    ?CBU OntoMOPs:isFunctioningAs ?GBU .
    ?CBU OntoMOPs:hasCBUFormula ?CBUFormula .
    ?CBU OntoSpecies:hasUniqueSpecies ?speciesIRI .    
    }"""
    return queryStr

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



def species_properties(speciesIRI):
    """Queries the molecular weight and mass of every CBU. """
    queryStr = """
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?MolecularWeightValue ?MolecularChargeValue
    WHERE
    {
    ?species OntoSpecies:hasMolecularWeight ?MolecularWeight .
    ?MolecularWeight OntoSpecies:value ?MolecularWeightValue .
    ?species OntoSpecies:hasCharge ?MolecularCharge .
    ?MolecularCharge OntoSpecies:value ?MolecularChargeValue .

    FILTER ((?species) = <#SPECIESIRI>) . 
    }"""

    queryStr = queryStr.replace('#SPECIESIRI', str(speciesIRI))
    return queryStr
