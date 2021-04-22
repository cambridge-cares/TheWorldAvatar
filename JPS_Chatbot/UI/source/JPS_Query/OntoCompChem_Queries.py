# template has one slot: species
ROTATIONAL_CONSTANT_QUERY = ''' 
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>

SELECT DISTINCT  ?name ?rotational_constants_value ?unit_short
WHERE  { 
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
?molecule_property gc:hasName "%s" .

 # ============ to match molecule =========================
?g_calculation gc:isCalculationOn ?rotational_constants .
?rotational_constants ontocompchem:hasRotationalConstants ?rotational_constants_value . 
OPTIONAL {
?rotational_constants gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
}
}  LIMIT 1
'''

VIBRATION_FREQUENCY_QUERY = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT   ?frequency ?name ?unit_short
WHERE  { 
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
?molecule_property gc:hasName "%s" .

# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn  ?VibrationalAnalysis .
?VibrationalAnalysis rdf:type gc:VibrationalAnalysis .
?VibrationalAnalysis gc:hasResult ?result . 
?result ontocompchem:hasFrequencies ?frequency .
OPTIONAL {
?result gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://purl.org/gc/","") AS ?unit_short) .
}
}  LIMIT 1
'''

ROTATIONAL_SYMMETRY_NUMBER = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name   ?symmetry_number  
WHERE  { 
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
?molecule_property gc:hasName "%s" .

# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn  ?RotationalSymmetry .
?RotationalSymmetry rdf:type ontocompchem:RotationalSymmetry .
?RotationalSymmetry ontocompchem:hasRotationalSymmetryNumber ?symmetry_number .
}   LIMIT 1
'''

GAUSSIAN_FILE = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name   ?File
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
?molecule_property gc:hasName "%s" .

# ============ to match molecule =========================
?g_calculation  ontocompchem:hasEnvironment   ?Environment .
?Environment    gc:hasOutputFile  ?File . 
} LIMIT 1
'''

SPIN_MULTIPLICITY = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT ?name ?SpinMultiplicity    
WHERE  {
   ?Molecule  ontocompchem:hasSpinMultiplicity ?SpinMultiplicity .
   ?GeometryOptimization    gc:hasMolecule    ?Molecule .
   ?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
   ?g_calculation  ontocompchem:hasInitialization ?initialization .
   ?initialization gc:hasMoleculeProperty ?molecule_property .	
   ?molecule_property gc:hasName ?name .
   ?molecule_property gc:hasName "%s" .
} LIMIT 1
'''

FORMAL_CHARGE = '''

PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT ?name ?FormalCharge_value    
WHERE  {
   ?Molecule gc:hasFormalCharge  ?FormalCharge .
   ?FormalCharge gc:hasValue ?FormalCharge_value . 
   ?GeometryOptimization    gc:hasMolecule    ?Molecule .
   ?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
   ?g_calculation ontocompchem:hasInitialization ?initialization .
   ?initialization gc:hasMoleculeProperty ?molecule_property .	
   ?molecule_property gc:hasName ?name .
   ?molecule_property gc:hasName "%s" .
   
 OPTIONAL {
    ?FormalCharge gc:hasUnit ?unit .
    BIND(REPLACE(STR(?unit),"http://purl.org/gc/","") AS ?unit_short) .
}
 
} LIMIT 1
 
'''

ELECTRONIC_ENERGY = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name    ?Electronic_energy ?unit_short
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName  ?name .
?molecule_property gc:hasName "%s" .
 # ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?ScfEnergy .
?ScfEnergy    gc:hasElectronicEnergy  ?x .
?x            gc:hasValue             ?Electronic_energy .

OPTIONAL {
?x gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
} # http://data.nasa.gov/qudt/owl/unit#Hartree
} LIMIT 1
'''

GEOMETRY_TYPE = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name   ?GeometryTypeValue 
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
?molecule_property gc:hasName "%s" .

 # ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryType .
?GeometryType   ontocompchem:hasGeometryType ?GeometryTypeValue .  
OPTIONAL {
?x gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
} # http://data.nasa.gov/qudt/owl/unit#Hartree
} LIMIT 1

'''

ontocompchem_simple_intents = ['symmetry_number',
                               'rotational_constants',
                               'vibration_frequency',
                               'guassian_file',
                               'spin_multiplicity',
                               'formal_charge',
                               'electronic_energy',
                               'geometry_type']

intent_to_template_mapping = {'rotational_constants': ROTATIONAL_CONSTANT_QUERY,
                              'symmetry_number': ROTATIONAL_SYMMETRY_NUMBER,
                              'vibration_frequency': VIBRATION_FREQUENCY_QUERY, 'guassian_file': GAUSSIAN_FILE,
                              'formal_charge': FORMAL_CHARGE, 'electronic_energy': ELECTRONIC_ENERGY,
                              'geometry_type': GEOMETRY_TYPE, 'spin_multiplicity': SPIN_MULTIPLICITY}
