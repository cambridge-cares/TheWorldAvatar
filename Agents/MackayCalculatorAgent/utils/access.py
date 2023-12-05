import requests
from requests.auth import HTTPBasicAuth
#chemistry query
chemep = "https://theworldavatar.io/chemistry/blazegraph-dev/ui/namespace/ontospecies/sparql"
pwd = "admin"
user = "bg_user"
chemquery = '''
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?speciesIRI ?type ?hhv ?lhv ?unitlabel ?refstatevalue ?refstateunitlabel
    WHERE 
    {
    ?speciesIRI rdf:type os:Species ;
                os:hasInChI ?Inchi .
    ?Inchi os:value ?Inchistr .
    FILTER (str(?Inchistr) = "InChI=1S/H2/h1H") 
    ?speciesIRI  os:hasHigherHeatingValue ?property .
    ?property rdf:type ?type ;
              os:value ?hhv ;
              os:unit ?unit ;
              os:hasReferenceState ?rfstate .
    ?rfstate os:value ?refstatevalue ;
             os:unit ?refstateunit .    
    ?unit rdfs:label ?unitlabel .
    ?refstateunit rdfs:label ?refstateunitlabel .
      

    ?speciesIRI os:hasLowerHeatingValue* ?property2 .
    ?property2 rdf:type ?type2 ;
              os:value ?lhv ;

    }
'''


#country query
coep = "https://www.theworldavatar.com/blazegraph/namespace/country/sparql"
coquery = '''
PREFIX ontoc: <https://www.theworldavatar.com/kg/ontocountry/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
prefix xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?countryName ?population ?dwellingunit
WHERE {
  ?country a ontoc:Country;
             rdfs:label ?countryName;
             ontoc:hasPopulation/om:hasValue/om:hasNumericalValue ?population;
             ontoc:hasTotalDwellingUnits/om:hasValue/om:hasNumericalValue ?dwellingunit.
}
'''


def unitConvertHeatValue(hv):
    return hv/1e6
def unitConvertPpl(ppl):
    return ppl/1000# convert population to thousands of population

def accessKG():
    headers= {"Accept":"application/sparql-results+json"}
    r = requests.post(chemep, data={'query': chemquery},headers=headers,auth=HTTPBasicAuth(user, pwd))
    chemresult  =r.json()
    hhv = unitConvertHeatValue(float(chemresult['results']['bindings'][0]['hhv']['value']))
    lhv = unitConvertHeatValue(float(chemresult['results']['bindings'][0]['lhv']['value']))
    r = requests.post(coep, data={'query': coquery},headers=headers)
    coresult = r.json()
    population = unitConvertPpl(float(coresult['results']['bindings'][0]['population']['value']))
    dwellingunit = unitConvertPpl(float(coresult['results']['bindings'][0]['dwellingunit']['value']))
    #Write to model
    return {"hhv":hhv, "lhv":lhv,"dwellingunit":dwellingunit,"population":population}
