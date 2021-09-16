from stdc.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from stdc.kgoperations.querykg import querykg
import json

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
    return query

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
                gc:isCalculationOn ?vibAnal ;
                gc:isCalculationOn ?rotConsts ;
                gc:isCalculationOn ?rotSym ;
                gc:isCalculationOn ?geomOpt ;
                oc:hasInitialization ?init .

            ?init gc:hasParameter ?method ;
                gc:hasParameter ?basisSet .

            ?method oc:hasLevelOfTheory ?lvlOfTheory .
            ?basisSet gc:hasBasisSet ?basisSettype .

            FILTER REGEX(str(?lvlOfTheory), "B3LYP", "i")
            FILTER REGEX(str(?basisSettype), "6-311G+?\\\(d,p\\\)", "i")

            ?vibAnal a gc:VibrationalAnalysis .
            ?rotConsts a oc:RotationalConstants .
            ?rotSym a oc:RotationalSymmetry .
            ?geomOpt a gc:GeometryOptimization .

            ?geomOpt gc:hasMolecule ?mol .
            ?mol oc:hasSpinMultiplicity ?spin_mult .

            ?vibAnal gc:hasResult ?freqResult .
            ?freqResult oc:hasFrequencies ?frequencies .

            ?rotConsts oc:hasRotationalConstants ?rot_constants .
            ?rotSym oc:hasRotationalSymmetryNumber ?sym_number .
        }""".replace('#ocIRI#',ocIRI).replace('#osIRI#',osIRI)
    return query

def ontospecies_data_query(osIRI):
    query = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

        SELECT ?mol_weight ?chem_formula
        WHERE {
            <#osIRI#> os:hasMolecularWeight ?y .
            ?y os:value ?mol_weight .
            <#osIRI#> rdfs:label ?chem_formula .
            FILTER NOT EXISTS {FILTER REGEX(str(?chem_formula), "Species", "i") .}
        }""".replace('#osIRI#',osIRI)
    return query