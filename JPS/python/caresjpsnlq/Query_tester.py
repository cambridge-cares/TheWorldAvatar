from NLQ_SPARQL import SPARQLEngine
import pprint


query = '''PREFIX  rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT DISTINCT ?thegdpppp (SUM(?nvOfdesignedcapacity) AS ?sumofnvOfdesignedcapacity) 
WHERE
  { 
  
  SERVICE <http://dbpedia.org/sparql>
      { SELECT *
        WHERE
          { <http://dbpedia.org/resource/Argentina>
                      <http://dbpedia.org/property/gdpPpp>  ?thegdpppp
          }
      }
      
      
      
    { 
    
    SELECT  ?nvOfdesignedcapacity
      WHERE
        { ?powerplants  rdf:type          <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerGenerator> ;
                    ?p60                  <http://dbpedia.org/resource/Argentina> ;
                    <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#designCapacity>  ?designedcapacity .
          ?designedcapacity
                    <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue>  ?value5 .
          ?value5   <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue>  ?nvOfdesignedcapacity
        }
    }
    
    
    
    
  } GROUP BY ?thegdpppp
  
'''



query2 = '''    PREFIX  rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                SELECT DISTINCT  ?country  ?thegdpppp ?sumofnvOfdesignedcapacity
                
                WHERE{                         
                
                  SERVICE <http://dbpedia.org/sparql>
                  {
                    ?country <http://dbpedia.org/property/gdpPpp> ?thegdpppp   .
                    ?country rdf:type <http://dbpedia.org/ontology/Country> 
                  } 
                   
                    
                {
                    SELECT DISTINCT ?country (SUM(?nvOfdesignedcapacity) as ?sumofnvOfdesignedcapacity)
                    WHERE { 
                                                  
                    	 ?powerplants rdf:type <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerGenerator> . 
                         ?powerplants  <http://dbpedia.org/ontology/country> ?country .
                         
                         ?powerplants <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#designCapacity> ?designedcapacity . 
                         ?designedcapacity <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue> ?value24 .
                         ?value24 <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?nvOfdesignedcapacity  
                        }GROUP BY ?country 
                }
         
                
                }   '''

s = SPARQLEngine()
r = s.fire_mix_query(query2)
pprint.pprint(r)