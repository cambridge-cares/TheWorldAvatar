VIBRATION_FREQUENCY = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT   ?name
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
 
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn  ?VibrationalAnalysis .
?VibrationalAnalysis rdf:type gc:VibrationalAnalysis .
?VibrationalAnalysis gc:hasResult ?result .
?result ontocompchem:hasFrequencies ?frequency .
OPTIONAL {
?result gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://purl.org/gc/","") AS ?unit_short) .
}
}
'''

GAUSSIAN_FILE = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name  
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
# ============ to match molecule =========================
?g_calculation  ontocompchem:hasEnvironment   ?Environment .
?Environment    gc:hasOutputFile  ?File .
}
'''

SYMMETRY_NUMBER = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name   
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
# FILTER regex(?name, "^C 8 H 14 $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn  ?RotationalSymmetry .
?RotationalSymmetry rdf:type ontocompchem:RotationalSymmetry .
?RotationalSymmetry ontocompchem:hasRotationalSymmetryNumber ?symmetry_number .
}

'''

SPIN_MULTIPLICITY = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name   
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
# FILTER regex(?name, "^C 8 H 14 $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
?GeometryOptimization    gc:hasMolecule    ?Molecule .
?Molecule  ontocompchem:hasSpinMultiplicity ?SpinMultiplicity .
}
'''

FORMAL_CHARGE = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name   
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
# FILTER regex(?name, "^%s $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
?GeometryOptimization    gc:hasMolecule    ?Molecule .
?Molecule gc:hasFormalCharge  ?FormalCharge .
?FormalCharge gc:hasValue ?FormalCharge_value . 

OPTIONAL {
?FormalCharge gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://purl.org/gc/","") AS ?unit_short) .
}
}
'''

ELECTRONIC_ENERGY = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name     
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
# FILTER regex(?name, "^%s $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?ScfEnergy .
?ScfEnergy    gc:hasElectronicEnergy  ?x .
?x            gc:hasValue             ?Electronic_energy .

OPTIONAL {
?x gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
} # http://data.nasa.gov/qudt/owl/unit#Hartree
}
'''

GEOMETRY_TYPE = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name    
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
# FILTER regex(?name, "^%s $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryType .
?GeometryType   ontocompchem:hasGeometryType ?GeometryTypeValue .  
}

'''

# query_list_ontocompchem = {'VIBRATION_FREQUENCY': VIBRATION_FREQUENCY, 'GAUSSIAN_FILE': GAUSSIAN_FILE,
#                            'SYMMETRY_NUMBER': SYMMETRY_NUMBER, 'ELECTRONIC_ENERGY': ELECTRONIC_ENERGY,
#                            'FORMAL_CHARGE': FORMAL_CHARGE, 'GEOMETRY_TYPE': GEOMETRY_TYPE,
#                            'SPIN_MULTIPLICITY': SPIN_MULTIPLICITY}

query_list_ontocompchem = {'GEOMETRY_TYPE': GEOMETRY_TYPE,
                           'SPIN_MULTIPLICITY': SPIN_MULTIPLICITY}

