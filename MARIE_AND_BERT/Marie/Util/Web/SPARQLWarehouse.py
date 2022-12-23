ONTOCOMPCHEM_HANDSHAKE = """
PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT   ?ocIRI 
WHERE {
      ?ocIRI oc:hasUniqueSpecies ?species .
} LIMIT 10 
"""

FIND_SAMPLE_REACTIONS = """
SELECT DISTINCT ?reaction ?equation 
WHERE {?reaction <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasEquation> ?equation .}
LIMIT 100000
"""

FIND_ALL_REACTIONS = """
SELECT DISTINCT ?reaction ?equation 
WHERE {?reaction <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasEquation> ?equation .}
"""

PUBCHEM_HANDSHAKE = """
SELECT ?x 
WHERE {
    ?x ?y ?z . 
} LIMIT 10 
"""

ONTOSPECIES_ALL_PROPERTIES_TEMPLATE = """
SELECT DISTINCT %
WHERE {
    %s 
}
"""
ONTOKIN_ALL_SPECIES = """
SELECT DISTINCT ?species ?label ?dipolemoment ?dipolemoment_unit 

WHERE {
	?species rdf:type <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#Species> .
	?species rdfs:label ?label .  
	OPTIONAL{
	?species <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasDipoleMoment> ?dipolemoment . 
	}
}  LIMIT 100 
"""


ONTOKIN_ALL_PROPERTIES_ALL_SPECIES = """
SELECT DISTINCT ?species ?label ?transport ?DipoleMoment ?DipoleMoment_unit ?LennardJonesDiameter ?LennardJonesDiameter_unit  
?LennardJonesWellDepth ?LennardJonesWellDepth_unit ?Polarizability ?Polarizability_unit ?RotationalRelaxationCollisionNumber 
?RotationalRelaxationCollisionNumber_unit ?SpeciesGeometry

WHERE {
	?species rdf:type <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#Species> .
	?species rdfs:label ?label .  
    ?species <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasTransportModel> ?transport . 
	
  	OPTIONAL{
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasDipoleMoment> ?DipoleMoment . 
    }
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasDipoleMomentUnits> ?DipoleMoment_unit . 
    }
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesDiameter> ?LennardJonesDiameter . 
    }
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesDiameterUnits> ?LennardJonesDiameter_unit . 
    }
    OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesWellDepth> ?LennardJonesWellDepth . 
    }
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasLennardJonesWellDepthUnits> ?LennardJonesWellDepth_unit . 
    }
    OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasPolarizability> ?Polarizability . 
    }
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasPolarizabilityUnits> ?Polarizability_unit . 
    }
     OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasRotationalRelaxationCollisionNumber> ?RotationalRelaxationCollisionNumber . 
    }
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasPolarizabilityUnits> ?RotationalRelaxationCollisionNumber_unit . 
    }	
  	OPTIONAL {
      ?transport <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasSpeciesGeometry> ?SpeciesGeometry . 
    }

}  
 
"""




ONTOSPECIES_ALL_SPECIES = """
SELECT DISTINCT ?species

WHERE {
	?species rdf:type <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 
}
"""

ONTOCOMPCHEM_ALL_SPEICES = """
PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT ?species ?ocIRI 
WHERE {
      ?ocIRI oc:hasUniqueSpecies ?species .
}
"""

ONTOCOMPCHEM_ALL_CALCULATION_QUERY = """
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX gc:  <http://purl.org/gc/>
        
        SELECT DISTINCT ?species ?ocIRI 
        ?geomType ?geomTypeValue 
        ?vibAnal ?vibAnalValue 
        ?rotConsts ?rotConstsValue 
        ?rotSym ?rotSymValue
        
        WHERE {
            ?ocIRI oc:hasUniqueSpecies ?species ;
                gc:isCalculationOn ?geomType .
            ?geomType oc:hasGeometryType ?geomTypeValue .
        
            OPTIONAL {
                ?ocIRI  gc:isCalculationOn ?vibAnalTemp ;
                        gc:isCalculationOn ?rotConsts ;
                        gc:isCalculationOn ?rotSym .
                    
                ?vibAnalTemp  gc:hasResult ?vibAnal .
                ?vibAnal oc:hasFrequencies ?vibAnalValue .

                ?rotConsts  oc:hasRotationalConstants ?rotConstsValue .
                ?rotSym   oc:hasRotationalSymmetryNumber ?rotSymValue .
            }} 

"""

ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY = """
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX os:  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX gc:  <http://purl.org/gc/>
        SELECT DISTINCT ?species ?geomType
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
            }} LIMIT 100
"""

WIKIDATA_GET_RELATION_LABELS = """
# find all properties of chemical compounds
SELECT DISTINCT ?label (GROUP_CONCAT(?alt;SEPARATOR="$") AS ?alt_labels)
WHERE 
{
  # Must be of a chemical component
 wd:%s  <http://www.w3.org/2000/01/rdf-schema#label>   ?label FILTER( LANG(?label)="en" ) .
 wd:%s  <http://www.w3.org/2004/02/skos/core#altLabel> ?alt FILTER( LANG(?alt_labels)="en" ) .
  
}
GROUP BY ?label
ORDER BY ?label
"""