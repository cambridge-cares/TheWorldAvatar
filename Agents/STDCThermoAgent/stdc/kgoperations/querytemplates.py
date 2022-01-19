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

def ontospecies_data_query(osIRI):
    query = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX otk: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

        SELECT ?mol_weight ?chem_formula ?enthalpy_ref
               ?enthalpy_ref_unit ?enthalpy_ref_temp
               ?enthalpy_ref_temp_unit ?enthalpy_ref_prov
        WHERE {
            <#osIRI#> os:hasMolecularWeight ?y .
            ?y os:value ?mol_weight .
            <#osIRI#> rdfs:label ?chem_formula .
            FILTER (!REGEX(str(?chem_formula), "Species", "i"))

            OPTIONAL {
                <#osIRI#> os:hasStandardEnthalpyOfFormation ?standardEnthalpyOfFormation .
                ?standardEnthalpyOfFormation os:hasPhase ?phase ;
                            os:hasReferenceTemperature ?tref ;
                            os:hasProvenance ?prov ;
                            os:value ?enthalpy_ref ;
                            os:units ?enthalpy_ref_unit .

                ?phase a otk:GasPhase .

                ?tref os:value ?enthalpy_ref_temp ;
                      os:units ?enthalpy_ref_temp_unit .

                ?prov rdfs:label ?enthalpy_ref_prov .
                FILTER (!REGEX(str(?enthalpy_ref_prov), "Reference", "i"))
            }
        }""".replace('#osIRI#',osIRI)
    return query