from stdc.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from stdc.kgoperations.querykg import querykg
import json

def ontocompchem_data_query(ocIRI, osIRI):
    query = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX os:  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX gc:  <http://purl.org/gc/>

        SELECT ?spin_mult ?frequencies ?rot_constants ?sym_number
        WHERE {
            <#ocIRI#> oc:hasUniqueSpecies <#osIRI#> ;
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
                <#ocIRI#> gc:isCalculationOn ?vibAnal ;
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
        }""".replace('#ocIRI#',ocIRI).replace('#osIRI#',osIRI)
    return query

def ontopesscan_data_query(opesIRI):
    query = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

        SELECT ?y ?z
        WHERE {
            <#opesIRI#> ?y ?z
        }""".replace('#opesIRI#',opesIRI)
    return query