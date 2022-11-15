import rdflib
from rdflib import Graph

from ontomatch.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from ontomatch.kgoperations.querykg import querykg, updatekg, res2triples
from ontomatch.utils.blackboard import Agent, LOCAL_BLACKBOARD_DIR
import ontomatch.utils.util



def downloadDataKg(addr, endpoint_label, namespace):
    #Query str to get all related data
    # hint:Query hint:constructDistinctSPO false .
    qstr_base = '''
PREFIX ns2: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
PREFIX ns1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX ns7: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
prefix ns5: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
PREFIX ns6: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>
PREFIX j.4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>

   CONSTRUCT {{ 
   ?s a ns2:PowerPlant.
      ?s ns2:hasYearOfBuilt [ns1:hasValue [ns1:numericalValue ?v]].
   ?s ns5:designCapacity [ns1:hasValue [ns1:numericalValue ?d]].
   ?s ns7:realizes ?f.
   ?s ns6:isOwnedBy [ ns6:hasName ?cn ].
   ?s j.4:hasProjectedCoordinate_y [ns1:hasValue [ns1:numericalValue ?y]].
    ?s j.4:hasProjectedCoordinate_x [ns1:hasValue [ns1:numericalValue ?x]].
   }} WHERE {{ graph ?g    {{ 
   ?s ns2:hasYearOfBuilt ?ov.
   ?ov ns1:hasValue ?nv.
   ?nv ns1:numericalValue ?v.
   ?s ns5:designCapacity ?od.
      ?od ns1:hasValue ?nd.
   ?nd ns1:numericalValue ?d.
   ?s ns7:realizes ?f.
   ?s ns6:isOwnedBy ?c.
   ?c ns6:hasName ?cn.
   ?s j.4:hasGISCoordinateSystem  ?ps.
   ?ps j.4:hasProjectedCoordinate_x ?xo.
         ?xo ns1:hasValue ?xvo.
   ?xvo ns1:numericalValue ?x.
      ?ps j.4:hasProjectedCoordinate_y ?yo.
         ?yo ns1:hasValue ?yvo.
   ?yvo ns1:numericalValue ?y.
    }}  
   FILTER(STR(?g)="{}")}} 
    '''
    qstr_kwl = '''
PREFIX ns2: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
PREFIX ns1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX ns7: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
prefix ns5: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
PREFIX ns6: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>
PREFIX j.4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>

   select ?Subject ?Predicate ?Object WHERE {{ graph ?g    {{ 
   ?Subject ?Predicate ?Object
    }}  
   FILTER(STR(?g)="{}")}} 
    '''
    qstr_nons =   '''
    prefix ns1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#> 
   prefix ns2: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> 
   prefix ns3: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> 
  prefix ns4: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> 
   prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
   CONSTRUCT {{ 
   ?s a ns4:PowerPlant.
   ?s rdfs:label ?label.
   ?s ns2:hasRequirementsAspect [ns3:hasValue [ns3:numericalValue ?d ]].
   ?s ns2:hasFunctionalAspect [ns4:consumesPrimaryFuel [rdfs:label ?f ]].
   ?s ns1:isOwnedBy [ ns1:hasName ?cn ].
   }} WHERE {{ 
    ?s a ns4:PowerPlant.
       ?s rdfs:label ?label.
   ?s ns2:hasRequirementsAspect ?od.
      ?od ns3:hasValue ?nd.
   ?nd ns3:numericalValue ?d.
   ?s ns2:hasFunctionalAspect ?of.
      ?of ns4:consumesPrimaryFuel ?nf.
   ?nf rdfs:label ?f.
    ?s ns1:isOwnedBy ?c.
   ?c ns1:hasName ?cn.
   }} 
    '''
    if namespace is None:
        qstr = qstr_nons
    elif 'kwl' in namespace or 'dukes' in namespace:
        qstr = qstr_kwl.format(namespace)
    else:
        qstr = qstr_base.format(namespace)

    res = querykg(SPARQL_ENDPOINTS[endpoint_label], qstr)
    triples = res2triples(res)
    graph = rdflib.Graph()
    #rdflib
    for triple in triples:
        graph.add(triple)
    graph.serialize(addr, format='turtle')


def readConfProperties(confjson):
    propertiesRequired = [o['prop2'] for o in confjson['mapping']['triples']]
    return propertiesRequired


def upload2Kg(linkfilepath):
    #read from file
    g = Graph()
    g.parse(linkfilepath)

    #construct update query
    pairs = g.query("""
    PREFIX owl:    <http://www.w3.org/2002/07/owl#>
    SELECT ?a ?b WHERE { ?a owl:sameAs ?b}""")
    updatestr = '''
    PREFIX owl:    <http://www.w3.org/2002/07/owl#>
    INSERT DATA {'''
    #call update
    for row in pairs:
        updatestr = updatestr + (f"<{row.a}> owl:sameAs <{row.b}>. ")
    updatestr = updatestr + "}"
    print(updatestr)
    updatekg(SPARQL_ENDPOINTS['powerplants'], updatestr)




if __name__ == '__main__':
    downloadDataKg('gppd_gbr_all.ttl','ukpowerplants',None)
    #downloadDataKg('gppd_gbr.ttl','powerplants','http://dukes')
    #upload2Kg('../tmp/blackboard/linked_power_plants.ttl')