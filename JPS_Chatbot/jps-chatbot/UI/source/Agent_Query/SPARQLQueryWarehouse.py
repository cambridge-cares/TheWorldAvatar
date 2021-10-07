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
                       msm:hasQualifier ?qualifier . 
       }  GROUP BY ?type ?name 
"""