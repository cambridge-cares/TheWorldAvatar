'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

def getMOPIRIs():
    """This function collects all MOPs from the KG."""

    queryStr = """
    PREFIX OntoMOPs: <https://www.theworldavatar.com/kg/ontomops/>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?mopIRI ?MOPFormula ?CBUFormula ?NumberValue ?Planarity ?Modularity ?Symbol ?cbuIRI
    WHERE {
        ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
        ?mopIRI OntoMOPs:hasProvenance ?Provenance .
        ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
        ?AssemblyModel OntoMOPs:hasPolyhedralShape ?PolhedralShape .
        ?PolhedralShape OntoMOPs:hasSymbol ?Symbol .
        ?AssemblyModel OntoMOPs:hasGenericBuildingUnitNumber ?GBUNumber .
        ?GBUNumber OntoMOPs:isNumberOf ?GBU .
        ?GBU OntoMOPs:hasGBUType ?GBUType .
        ?GBUType OntoMOPs:hasPlanarity ?Planarity .
        ?GBUType OntoMOPs:hasModularity ?Modularity .
        ?GBUNumber OntoMOPs:hasUnitNumberValue ?NumberValue .
        ?mopIRI OntoMOPs:hasChemicalBuildingUnit ?cbuIRI .
        ?cbuIRI OntoMOPs:isFunctioningAs ?GBU .
        ?cbuIRI OntoMOPs:hasCBUFormula ?CBUFormula .
    }"""
    return queryStr

def mop_GBUs(mopIRI):
    """Queries and collects MOP data relating to the GBUs/CBUs.
    As every MOP has two GBUs, returns back information on both. """
    queryStr = """
    PREFIX OntoMOPs: <https://www.theworldavatar.com/kg/ontomops/>
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?mopIRI ?MOPFormula ?CBUFormula ?NumberValue ?Planarity ?Modularity ?Symmetry ?MOPReference ?CBUType ?OuterCoordination ?CBUFunctionalGroup ?Direction ?cbuIRI
    WHERE {
        values ?mopIRI {<#MOPIRI>}
        ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
        ?mopIRI OntoMOPs:hasProvenance ?Provenance .
        ?Provenance OntoMOPs:hasReferenceDOI ?MOPReference .
        ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
        ?AssemblyModel OntoMOPs:hasSymmetryPointGroup ?Symmetry .
        ?AssemblyModel OntoMOPs:hasGenericBuildingUnitNumber ?GBUNumber .
        ?GBUNumber OntoMOPs:isNumberOf ?GBU .
        ?GBU OntoMOPs:hasGBUType ?GBUType .
        ?GBUType OntoMOPs:hasPlanarity ?Planarity .
        ?GBUType OntoMOPs:hasModularity ?Modularity .
        ?GBUNumber OntoMOPs:hasUnitNumberValue ?NumberValue .
        ?mopIRI OntoMOPs:hasChemicalBuildingUnit ?cbuIRI .
        ?cbuIRI OntoMOPs:isFunctioningAs ?GBU .
        ?cbuIRI OntoMOPs:hasCBUFormula ?CBUFormula .
        ?cbuIRI OntoMOPs:hasBindingSite ?CBUBindingSite .
        ?cbuIRI OntoMOPs:hasBindingDirection ?BindingDirection .
        ?BindingDirection rdf:type ?Direction.
        ?CBUBindingSite OntoMOPs:hasOuterCoordinationNumber ?OuterCoordination .
        ?CBUBindingSite OntoMOPs:hasBindingFragment ?CBUFunctionalGroup.
        ?CBUBindingSite rdf:type ?CBUType.
    }"""
    queryStr = queryStr.replace('#MOPIRI', str(mopIRI))
    return queryStr

def mop_reference(string, mop_symmetry):
    """Using a MOP formula and Symmetry point group, checks if the MOP exists in the KG."""
    queryStr = """
    PREFIX OntoMOPs: <https://www.theworldavatar.com/kg/ontomops/>
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



def species_properties(cbuIRI):
    """Queries the molecular weight and mass of every CBU. """
    queryStr = """
	PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?MolecularWeightValue ?MolecularChargeValue
    WHERE
    {
        values ?cbu {<#CBUIRI>}
        ?cbu OntoSpecies:hasMolecularWeight ?MolecularWeight .
        ?MolecularWeight Measure:hasValue/Measure:hasNumericalValue ?MolecularWeightValue .
        ?cbu OntoSpecies:hasCharge ?MolecularCharge .
        ?MolecularCharge Measure:hasValue/Measure:hasNumericalValue ?MolecularChargeValue .
    }"""

    queryStr = queryStr.replace('#CBUIRI', str(cbuIRI))
    return queryStr
