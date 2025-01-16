import json
import re
import urllib.parse
import urllib.request



# ontocompchem queries
def fire_query(query):
    with open('queries', 'a') as f:
        f.write('----------- ontocompchem -----------')
        f.write('\n' + str(query))
    print('----------- firing the query to JPS ontochemcomp -------------')
    print(query)
    # x = input()
    url = "http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem"
    values = {'query': query}
    data = urllib.parse.urlencode(values).encode('utf-8')
    print(type(data))
    req = urllib.request.Request(url, data)
    print('-------------')
    print(req)
    response = urllib.request.urlopen(req).read()
    return response

# To get the rotational constants of a molecular
query = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?p  ?type
WHERE  { 
?up_node ?p ?node .
?up_node rdf:type ?type .
?node rdf:type <http://purl.org/gc/MoleculeProperty> .
?node gc:hasName ?name .
} LIMIT 4
'''
# ?g_calculation ontocompchem:hasUniqueSpecies ?species .
# ?g_calculation gc:isCalculationOn ?

# TODO: run through the ontocompchem questions ... from the simple ones
# what is the rotational constants of H2O2
query_get_rotational_constants_by_molecule = ''' 
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
FILTER regex(?name, "^H 2 O 2 $")
# ============ to match molecule =========================
?g_calculation gc:isCalculationOn ?rotational_constants .
?rotational_constants ontocompchem:hasRotationalConstants ?rotational_constants_value . 
OPTIONAL {
?rotational_constants gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
}
} 
'''

# TO get the frequency of a molecule
query_get_vibriation_frequency = '''
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
FILTER regex(?name, "^H 2 O 2 $")

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
query_get_rotational_symmetry = '''
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
FILTER regex(?name, "^H 2 O 2 $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn  ?RotationalSymmetry .
?RotationalSymmetry rdf:type ontocompchem:RotationalSymmetry .
?RotationalSymmetry ontocompchem:hasRotationalSymmetryNumber ?symmetry_number .



}   
'''


def process_species_for_ontocompchem(species):
    # to convert H2O2 or h2o2 to H 2 O 2
    temp = ''
    number_regex = r'[0-9]+'
    alphabet_regex = r'[a-zA-Z]'
    print('-----------------------')
    print('species', species)
    if type(species) == str:

        numbers = re.findall(number_regex,species)
        for number in list(set(numbers)):
            new_number = ' ' + number + ' '
            species = species.replace(number, new_number)

        return species
        # return result
    else:
        return None

test_query_h2o2 ='''
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
FILTER regex(?name, "^C 8 H 14 $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn  ?RotationalSymmetry .
?RotationalSymmetry rdf:type ontocompchem:RotationalSymmetry .
?RotationalSymmetry ontocompchem:hasRotationalSymmetryNumber ?symmetry_number .
}
'''


log_file_query  ='''
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
FILTER regex(?name, "^C 3 H 18 $")
# ============ to match molecule =========================
?g_calculation  ontocompchem:hasEnvironment   ?Environment .
?Environment    gc:hasOutputFile  ?File .
}
'''


multiplicity_query = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name    ?SpinMultiplicity
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
FILTER regex(?name, "^C 8 H 14 $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
?GeometryOptimization    gc:hasMolecule    ?Molecule .
?Molecule  ontocompchem:hasSpinMultiplicity ?SpinMultiplicity .
}
'''

formal_charge  = '''
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT  ?name    ?FormalCharge_value ?unit_short
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName ?name .
FILTER regex(?name, "%s $")
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

electronic_energy = '''

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
?molecule_property gc:hasName ?name .
FILTER regex(?name, "C 2 H 2 O 2 $")
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

geometry_type_query = '''

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
FILTER regex(?name, "H 2 O 2 $")
# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryType .
?GeometryType   ontocompchem:hasGeometryType ?GeometryTypeValue .  

OPTIONAL {
?x gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
} # http://data.nasa.gov/qudt/owl/unit#Hartree
}

'''


# r = fire_query(electronic_energy)
# print(r.decode('utf-8'))


def fire_query_ontokin(query):
    print('----------- firing the query to JPS -------------')
    print(query)
    with open('queries', 'a') as f:
        f.write('----------- ontokin -----------')
        f.write('\n' + str(query))
    url = "http://www.theworldavatar.com/OntoKinGUI/OntoKinEndpointProxy"
    values = {'queryString': query}
    data = urllib.parse.urlencode(values).encode('utf-8')
    req = urllib.request.Request(url, data)
    response = urllib.request.urlopen(req).read()
    return response

def fire_query_free(url, query, key):
    print(query)
    print(url)
    values = {key: query}
    data = urllib.parse.urlencode(values).encode('utf-8')
    req = urllib.request.Request(url, data)
    response = urllib.request.urlopen(req).read()
    return response

hasLennardJonesDiameter = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?LennardJonesDiameter ?DiameterUnits ?LennardJonesWellDepth ?WellDepthUnits
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^H2O2$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasLennardJonesDiameter  ?LennardJonesDiameter . 
  ?TransportModel ontokin:hasLennardJonesDiameterUnits ?DiameterUnits .
  ?TransportModel ontokin:hasLennardJonesWellDepth ?LennardJonesWellDepth . 
  ?TransportModel ontokin:hasLennardJonesWellDepthUnits ?WellDepthUnits .
}  
'''

hasPolarizability = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?Polarizability ?Unit 
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^H2O2$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasPolarizability  ?Polarizability . 
  ?TransportModel ontokin:hasPolarizabilityUnits ?Unit .
}  
'''

hasDipoleMoment = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?DipoleMoment ?Unit 
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^H2O2$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasDipoleMoment  ?DipoleMoment . 
  ?TransportModel ontokin:hasDipoleMomentUnits ?Unit .
}  
'''

relaxation_collision = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?RotationalRelaxationCollisionNumber
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^H2O2$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasRotationalRelaxationCollisionNumber  ?RotationalRelaxationCollisionNumber . 
}  
'''

ontokin_url = 'http://www.theworldavatar.com/rdf4j-server/repositories/ontokin'
ontospecies_url = 'http://www.theworldavatar.com/rdf4j-server/repositories/ontospecieskb'
ontocompchem_url = 'http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem'


# r = fire_query_free(ontokin_url, hasLennardJonesDiameter, 'query')
# r = fire_query_ontokin(has_transport_query)
# r = fire_query_ontokin(relaxation_collision)
# print('result from', r)
q = '''
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
FILTER regex(?name, "^C 8 H 14 $")
# ============ to match molecule =========================
?g_calculation  ontocompchem:hasEnvironment   ?Environment .
?Environment    gc:hasOutputFile  ?File .
}
'''
r = fire_query(q)
print(r)

#
# process_species_for_ontocompchem('h2o2')
# process_species_for_ontocompchem('H2O2')
# process_species_for_ontocompchem('Ch4')
# process_species_for_ontocompchem('C8H14')