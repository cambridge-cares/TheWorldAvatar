ONTOSPECIES_GET_SMILES = '''
PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT ?SMILES
WHERE {
<%s> OntoSpecies:SMILES ?SMILES . 
}  
'''

ONTOSPECIES_GET_FORMULA = '''
PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT ?SMILES
WHERE {
<%s> OntoSpecies:SMILES ?SMILES . 
}  
'''

ONTOSPECIES_GET_ALTLABEL = '''
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?alt_label 
WHERE {
<%s> skos:altLabel ?alt_label .
}
'''

ONTOSPECIES_GET_LABEL = '''
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?label  
WHERE {
#<http://www.theworldavatar.com/kb/ontospecies/s00002431.owl/Species_7257917534045400> rdfs:label ?label .
  <%s> rdfs:label ?label .
}
'''

ONTOSPECIES_GET_NAME = '''
PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT ?name 
WHERE {
<%s> OntoSpecies:SMILES ?name . 
}  
'''

ONTOSPECIES_GET_INCHIS = '''
PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?inchi 
WHERE {

<%s> OntoSpecies:inChI ?inchi .

}
'''


GET_VALID_ONTOSPECIES_IRI = '''
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX os:  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX gc:  <http://purl.org/gc/>
        SELECT DISTINCT ?species
        WHERE {
            ?ocIRI oc:hasUniqueSpecies ?species ;
                gc:isCalculationOn ?geomOpt ;
                gc:isCalculationOn ?geomType ;
                oc:hasInitialization ?init .
            ?init a oc:InitializationModule ;
                gc:hasParameter ?method ;
                gc:hasParameter ?basisSet .
            ?geomOpt a gc:GeometryOptimization ;
                       gc:hasMolecule ?mol .
            ?mol oc:hasSpinMultiplicity ?spin_mult .
            ?geomType a oc:GeometryType .
            ?geomType oc:hasGeometryType ?geomTypeValue .
            OPTIONAL {
                ?ocIRI gc:isCalculationOn ?vibAnal ;
                    gc:isCalculationOn ?rotConsts ;
                    gc:isCalculationOn ?rotSym .
                ?vibAnal a gc:VibrationalAnalysis ;
                        gc:hasResult ?freqResult .
                ?freqResult oc:hasFrequencies ?frequencies .
                ?rotConsts a oc:RotationalConstants ;
                        oc:hasRotationalConstants ?rot_constants .
                ?rotSym a oc:RotationalSymmetry ;
                        oc:hasRotationalSymmetryNumber ?sym_number .
            }
          
            filter( regex(str(?species), "Species" ))
        }
'''

ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY = '''

PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX os:  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX gc:  <http://purl.org/gc/>
        SELECT DISTINCT ?ocIRI 
        WHERE {
            ?ocIRI oc:hasUniqueSpecies <%s> ;
                gc:isCalculationOn ?geomOpt ;
                gc:isCalculationOn ?geomType ;
                oc:hasInitialization ?init .
            ?init a oc:InitializationModule ;
                gc:hasParameter ?method ;
                gc:hasParameter ?basisSet .
            ?geomOpt a gc:GeometryOptimization ;
                       gc:hasMolecule ?mol .
            ?mol oc:hasSpinMultiplicity ?spin_mult .
            ?geomType a oc:GeometryType .
            ?geomType oc:hasGeometryType ?geomTypeValue .
            OPTIONAL {
                ?ocIRI gc:isCalculationOn ?vibAnal ;
                    gc:isCalculationOn ?rotConsts ;
                    gc:isCalculationOn ?rotSym .
                ?vibAnal a gc:VibrationalAnalysis ;
                        gc:hasResult ?freqResult .
                ?freqResult oc:hasFrequencies ?frequencies .
                ?rotConsts a oc:RotationalConstants ;
                        oc:hasRotationalConstants ?rot_constants .
                ?rotSym a oc:RotationalSymmetry ;
                        oc:hasRotationalSymmetryNumber ?sym_number .
            }
        }
'''

GET_AGENT_INPUT_PARAMETERS = """
    PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
    SELECT DISTINCT ?type ?name ?isArray ?nerLabel
       WHERE {
          ?operation msm:hasInput ?MessageContent . 
          ?MessageContent msm:hasMandatoryPart ?MessagePart . 
          ?MessagePart msm:hasType ?type ;
                       msm:hasName ?name ; 
                       msm:isArray ?isArray ;
                       msm:hasNerLabel ?nerLabel .
       }  
    """

GET_AGENT_OUTPUTS = """
    PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
    SELECT DISTINCT ?type ?name ?isArray ?nerLabel (GROUP_CONCAT (DISTINCT ?qualifier; separator="; ") AS ?qualifiers)
       WHERE {
          ?operation msm:hasOutput ?MessageContent . 
          ?MessageContent msm:hasMandatoryPart ?MessagePart . 
          ?MessagePart msm:hasType ?type ;
                       msm:hasName ?name ; 
                       msm:isArray ?isArray ;
                       msm:hasNerLabel ?nerLabel .
                       
          OPTIONAL {
            ?MessagePart msm:hasQualifier ?qualifier_node .
          }
                       

          ?qualifier_node msm:hasName ?qualifier .  
       }  GROUP BY ?type ?name 
"""

GET_HTTP_URL = """
    PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>
    SELECT DISTINCT ?url 
       WHERE {
          ?operation msm:hasHttpUrl ?url . 
       }  
"""