LENNARD_JONES_WELL_DEPTH = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?name
{
  ?Species rdfs:label ?name .
  # FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasLennardJonesDiameter  ?LennardJonesDiameter . 
  ?TransportModel ontokin:hasLennardJonesDiameterUnits ?DiameterUnits .
  ?TransportModel ontokin:hasLennardJonesWellDepth ?LennardJonesWellDepth . 
  ?TransportModel ontokin:hasLennardJonesWellDepthUnits ?WellDepthUnits .
}  
'''


POLARIZABILITY = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?name
{
  ?Species rdfs:label ?name .
  # FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasPolarizability  ?Polarizability . 
  ?TransportModel ontokin:hasPolarizabilityUnits ?Unit .
}  
'''


DIPOLE_MOMENT = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?name
{
  ?Species rdfs:label ?name .
  # FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasDipoleMoment  ?DipoleMoment . 
  ?TransportModel ontokin:hasDipoleMomentUnits ?Unit .
}  
'''


RELAXATION_COLLISION = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?name
{
  ?Species rdfs:label ?name .
  # FILTER regex(?label, "^%s$")
  ?Species ontokin:hasTransportModel ?TransportModel . 
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasRotationalRelaxationCollisionNumber  ?RotationalRelaxationCollisionNumber . 
}  
'''

query_list_ontokin =  {'LENNARD_JONES_WELL_DEPTH': LENNARD_JONES_WELL_DEPTH,
                        'POLARIZABILITY': POLARIZABILITY, 'DIPOLE_MOMENT':DIPOLE_MOMENT , 'RELAXATION_COLLISION':RELAXATION_COLLISION }