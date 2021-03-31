GENERAL_QUERY = '''

PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?%s ?Unit
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:%s ?%s . 
  OPTIONAL{
  ?TransportModel ontokin:%sUnits ?Unit .
  }
}  LIMIT 1


'''

HIGH_SPEED_GENERAL_QUERY = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?%s ?Unit
{
	 
	 ?TransportModel ontokin:%s ?%s .
     ?Species ontokin:hasTransportModel ?TransportModel .
     ?Species rdfs:label ?label .
	 ?Species rdfs:label "%s" .
	 
	  OPTIONAL{
		?TransportModel ontokin:%sUnits ?Unit .
     } 
	 
}  LIMIT 1
'''

# 1. att name, 1.5  2. att iri name 3. att name 4. att iri name

LENNARD_JONES_WELL_DEPTH = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?LennardJonesWellDepth ?Unit
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasLennardJonesWellDepth ?LennardJonesWellDepth . 
  OPTIONAL{
  ?TransportModel ontokin:hasLennardJonesWellDepthUnits ?Unit .
  } 
}  
'''


POLARIZABILITY = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?Polarizability ?Unit 
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasPolarizability  ?Polarizability . 
  ?TransportModel ontokin:hasPolarizabilityUnits ?Unit .
}  
'''


DIPOLE_MOMENT = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?DipoleMoment ?Unit 
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasDipoleMoment  ?DipoleMoment . 
  ?TransportModel ontokin:hasDipoleMomentUnits ?Unit .
}  
'''


RELAXATION_COLLISION = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?RotationalRelaxationCollisionNumber
{
     ?TransportModel ontokin:hasRotationalRelaxationCollisionNumber  ?RotationalRelaxationCollisionNumber . 
     ?Species ontokin:hasTransportModel ?TransportModel .
     ?Species rdfs:label ?label .
	 ?Species rdfs:label "%s" . 
}  LIMIT 1

 

'''




ontokin_simple_intents = ['polarizability',
                          'dipole_moment',
                          'rotational_relaxation_collision',
                          'lennard_jones_well']
