CBU_LABEL_QUERY = """
PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?target ?label  
WHERE
{   
?target rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#ChemicalBuildingUnit>.
?mop OntoMOPs:hasChemicalBuildingUnit ?target.
?mop OntoMOPs:hasAssemblyModel ?model.
?model OntoMOPs:hasPolyhedralShape ?shape.
?shape OntoMOPs:hasSymbol ?symbol.
?target OntoMOPs:hasCBUFormula ?label
}
"""

GBU_LABEL_QUERY = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT distinct ?target ?label
    WHERE
    {   
        ?cbu rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#ChemicalBuildingUnit>.
        ?cbu OntoMOPs:isFunctioningAs ?target.
        ?target OntoMOPs:hasModularity ?modularity . 
        ?target OntoMOPs:hasPlanarity ?planarity1 . 
        ?cbu OntoMOPs:hasCBUFormula ?cbuFormula .
        BIND(CONCAT(?modularity, "-", ?planarity1) AS ?label)
    }
        """

AMS_SHAPE_LABEL_QUERY = """
PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT distinct ?target ?label 
WHERE
{   
  ?target OntoMOPs:hasPolyhedralShape ?shape . 
  ?shape rdf:type ?label .
}
"""