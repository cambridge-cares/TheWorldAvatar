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
                       msm:hasNerLabel ?nerLabel ;
                       msm:hasQualifier ?qualifier_node .

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