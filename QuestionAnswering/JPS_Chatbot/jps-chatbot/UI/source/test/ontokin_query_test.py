import json
import sys
from pprint import pprint

from SPARQLWrapper import SPARQLWrapper, JSON
import urllib.parse
import urllib.request

def fire_query(query):
    url = "http://www.theworldavatar.com/OntoKinGUI/OntoKinEndpointProxy"
    values = {'queryString': query}
    data = urllib.parse.urlencode(values).encode('utf-8')
    print(type(data))
    req = urllib.request.Request(url, data)
    response = urllib.request.urlopen(req).read()
    return response


query_1 = '''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
SELECT ?MechanismName ?MechanismIRI
WHERE {
?ReactionIRI ontokin:hasEquation ?Equation .
?ReactionIRI ontokin:belongsToPhase ?Phase .
?Phase ontokin:containedIn ?MechanismIRI .
?MechanismIRI rdfs:label ?MechanismName .
}
LIMIT 1
'''
query_2 = '''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
SELECT ?MechanismName ?MechanismIRI ?Equation
WHERE {
?ReactionIRI ontokin:hasEquation ?Equation .
FILTER regex(?Equation, " O")
FILTER regex(?Equation, " NO")
FILTER regex(?Equation, "O2 ")
FILTER regex(?Equation, "N ")
FILTER (STRLEN(?Equation)=17 || STRLEN(?Equation)=16)
?ReactionIRI ontokin:belongsToPhase ?Phase .
?Phase ontokin:containedIn ?MechanismIRI .
?MechanismIRI rdfs:label ?MechanismName .
} LIMIT 1'''

query_3 = '''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  ?reaction ?equation ?xLabel ?yLabel  ?zLabel ?aLabel
WHERE  {
?reaction reaction:hasReactant ?x ;
ontokin:hasEquation ?equation ;
reaction:hasReactant ?y ;
reaction:hasProduct ?z .
 
?x rdfs:label ?xLabel .
FILTER regex(?xLabel, "^H") 
?y rdfs:label ?yLabel .
FILTER regex(?yLabel, "^O2") 
?z rdfs:label ?zLabel .
FILTER regex(?zLabel, "^O") 
?a rdfs:label ?aLabel .
FILTER regex(?aLabel, "^OH")  
}  LIMIT 1
'''

query_4 = '''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  ?reaction ?ReactantLabel  ?ReactantLabel2 ?Product_1Label
WHERE {
?reaction reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 .
 
?Product_1 ?p3 ?y3 .
?y3 rdfs:label ?Product_1Label .
FILTER regex(?Product_1Label, "^H2O$") 
 
?Reactant_1 ?p ?y .
?y rdfs:label ?ReactantLabel .
FILTER regex(?ReactantLabel, "^H2$") 

?Reactant_2 ?p2 ?y2 .
?y2 rdfs:label ?ReactantLabel2 .
FILTER regex(?ReactantLabel2, "^O2$")  
 
}  LIMIT 1
'''

query_5 ='''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  ?reaction  ?Reactant_1_label ?Reactant_2_label ?Product_1_label ?Product_2_label
WHERE  {
?reaction 
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 ;
reaction:hasProduct ?Product_2 . 
?Reactant_1 rdfs:label ?Reactant_1_label . 
?Reactant_2 rdfs:label ?Reactant_2_label . 
?Product_1 rdfs:label ?Product_1_label . 
?Product_2 rdfs:label ?Product_2_label . 
FILTER regex(?Reactant_1_label, "^O2$")  
FILTER regex(?Reactant_2_label, "^H$")  
FILTER regex(?Product_1_label, "^O$")  
FILTER regex(?Product_2_label, "^O$")  
}  LIMIT 1 

 
'''

query_6 = '''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT   ?reaction ?Equation 
WHERE  {
?reaction ontokin:hasEquation ?Equation ; 
reaction:hasReactant ?Reactant_1 ;
reaction:hasProduct ?Product_1 ;
reaction:hasProduct ?Product_2 .  
?Reactant_1 rdfs:label ?Reactant_1_label . 
?Product_1 rdfs:label ?Product_1_label . 
?Product_2 rdfs:label ?Product_2_label .   
FILTER regex(?Reactant_1_label, "^H$")
FILTER regex(?Product_1_label, "^O$")
FILTER regex(?Product_2_label, "^OH$")

} LIMIT 1  

 
'''


query_7 = '''PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT   ?reaction ?Equation 
WHERE  {
?reaction ontokin:hasEquation ?Equation ; 
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 .  
?Reactant_1 rdfs:label ?Reactant_1_label . 
?Reactant_2 rdfs:label ?Reactant_2_label .   
FILTER regex(?Reactant_1_label, "^H2$")
FILTER regex(?Reactant_2_label, "^OH$")

} LIMIT 1 '''

# mechanism ==> phase ==> reaction

query_8 = '''PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT   ?reaction ?Equation ?v1 ?v2 ?v3
        WHERE   
{        ?reaction ontokin:hasEquation ?Equation ; 
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 ;
reaction:hasProduct ?Product_2 .  
?Reactant_1 rdfs:label ?Reactant_1_label . 
?Reactant_2 rdfs:label ?Reactant_2_label . 
?Product_1 rdfs:label ?Product_1_label . 
?Product_2 rdfs:label ?Product_2_label .   
FILTER regex(?Reactant_1_label, "^HO2$")
FILTER regex(?Reactant_2_label, "^HCO$")
FILTER regex(?Product_1_label, "^CO2$")
FILTER regex(?Product_2_label, "^H$")   

 
 ?reaction ontokin:hasArrheniusCoefficient ?t0 .
 ?t0 ontokin:hasActivationEnergy ?v1 ;
 ontokin:hasActivationEnergyUnits  ?v2 ;
 ontokin:hasPreExponentialFactor ?v3 ;
 
        
}LIMIT 1  
        
         '''
query_9 = '''
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT   ?reaction ?Equation ?t
        WHERE  {
        ?reaction ontokin:hasEquation ?Equation ;
        ontokin:hasArrheniusCoefficient  ?t ;
   
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 ;
reaction:hasProduct ?Product_2 ;
reaction:hasProduct ?Product_3 .
?Reactant_1 rdfs:label ?Reactant_1_label .
?Reactant_2 rdfs:label ?Reactant_2_label .
?Product_1 rdfs:label ?Product_1_label .
?Product_2 rdfs:label ?Product_2_label .
?Product_3 rdfs:label ?Product_3_label .
FILTER regex(?Reactant_1_label, "^HO2$")
FILTER regex(?Reactant_2_label, "^HCO$")
FILTER regex(?Product_1_label, "^H$")
FILTER regex(?Product_2_label, "^OH$")
FILTER regex(?Product_3_label, "^CO2$")
 
 

        } LIMIT 10

'''

query_a = '''        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT   ?reaction ?Equation ?ActivationEnergy ?ActivationEnergyUnits  ?PreExponentialFactor ?PreExponentialFactorUnits ?TemperatureExponent ?TemperatureExponentUnits
 
        WHERE  {
        ?reaction ontokin:hasEquation ?Equation ;
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 .
?Reactant_1 rdfs:label ?Reactant_1_label .
?Reactant_2 rdfs:label ?Reactant_2_label .
?Product_1 rdfs:label ?Product_1_label .
FILTER regex(?Reactant_1_label, "^H2$")
FILTER regex(?Reactant_2_label, "^O2$")
FILTER regex(?Product_1_label, "^H2O$")
 OPTIONAL {
 ?reaction  <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasArrheniusCoefficient>  ?t .
 ?t ontokin:hasActivationEnergy ?ActivationEnergy .
 ?t ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits  .
 ?t ontokin:hasPreExponentialFactor ?PreExponentialFactor .
 ?t ontokin:hasPreExponentialFactorUnits ?PreExponentialFactorUnits .
 ?t ontokin:hasTemperatureExponent ?TemperatureExponent .
 ?t ontokin:hasTemperatureExponentUnits ?TemperatureExponentUnits .
   }

        } LIMIT 1
'''

query_b = '''
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT   ?reaction ?Equation ?ActivationEnergy ?ActivationEnergyUnits  ?PreExponentialFactor ?PreExponentialFactorUnits ?TemperatureExponent ?TemperatureExponentUnits
 
        WHERE  {
        ?reaction ontokin:hasEquation ?Equation ;
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 .
?Reactant_1 rdfs:label ?Reactant_1_label .
?Reactant_2 rdfs:label ?Reactant_2_label .
?Product_1 rdfs:label ?Product_1_label .
FILTER regex(?Reactant_1_label, "^H2$")
FILTER regex(?Reactant_2_label, "^O2$")
FILTER regex(?Product_1_label, "^H2O$")
 OPTIONAL {
 ?reaction  <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasArrheniusCoefficient>  ?t .
 ?t ontokin:hasActivationEnergy ?ActivationEnergy .
 ?t ontokin:hasPreExponentialFactor ?PreExponentialFactor .
 ?t ontokin:hasTemperatureExponent ?TemperatureExponent .
   }

        } LIMIT 1
'''

query_c = '''
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT   ?reaction ?Equation ?t
        WHERE  {
        ?reaction ontokin:hasEquation ?Equation ;
        ontokin:hasArrheniusCoefficient  ?t ;
   
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
reaction:hasProduct ?Product_1 ;
reaction:hasProduct ?Product_2 ;
reaction:hasProduct ?Product_3 .
?Reactant_1 rdfs:label ?Reactant_1_label .
?Reactant_2 rdfs:label ?Reactant_2_label .
?Product_1 rdfs:label ?Product_1_label .
?Product_2 rdfs:label ?Product_2_label .
?Product_3 rdfs:label ?Product_3_label .
FILTER regex(?Reactant_1_label, "^HO2$")
FILTER regex(?Reactant_2_label, "^HCO$")
FILTER regex(?Product_1_label, "^H$")
FILTER regex(?Product_2_label, "^OH$")
FILTER regex(?Product_3_label, "^CO2$")
 
 

        } LIMIT 10

'''

query_e = '''
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT   ?reaction ?Equation  ?ActivationEnergy ?ActivationEnergyUnits  ?PreExponentialFactor ?PreExponentialFactorUnits ?TemperatureExponent ?TemperatureExponentUnits
        WHERE  {
        ?reaction ontokin:hasEquation ?Equation ;
reaction:hasReactant ?Reactant_1 ;
reaction:hasReactant ?Reactant_2 ;
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasArrheniusCoefficient>  ?t .
 ?t ontokin:hasActivationEnergy ?ActivationEnergy .
 ?t ontokin:hasActivationEnergyUnits  ?ActivationEnergyUnits  .
 ?t ontokin:hasPreExponentialFactor ?PreExponentialFactor .
 ?t ontokin:hasPreExponentialFactorUnits ?PreExponentialFactorUnits .
 ?t ontokin:hasTemperatureExponent ?TemperatureExponent .
 ?t ontokin:hasTemperatureExponentUnits ?TemperatureExponentUnits .
?Reactant_1 rdfs:label ?Reactant_1_label .
?Reactant_2 rdfs:label ?Reactant_2_label .
FILTER regex(?Reactant_1_label, "^H$")
FILTER regex(?Reactant_2_label, "^OH$")
 
        } LIMIT 1

'''

lwd = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?LennardJonesDiameter ?DiameterUnits ?LennardJonesWellDepth ?WellDepthUnits
{
  ?Species rdfs:label ?label .
  FILTER regex(?label, "^C2H2O2$")
  ?Species ontokin:hasTransportModel ?TransportModel .
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasLennardJonesDiameter  ?LennardJonesDiameter .
  ?TransportModel ontokin:hasLennardJonesDiameterUnits ?DiameterUnits .
  ?TransportModel ontokin:hasLennardJonesWellDepth ?LennardJonesWellDepth .
  ?TransportModel ontokin:hasLennardJonesWellDepthUnits ?WellDepthUnits .
}

'''

get_reaction = '''
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
        SELECT  DISTINCT   ?Equation   
        WHERE  {
        ?reaction ontokin:hasEquation ?Equation ;
 
        }  LIMIT 50
'''

t = '''
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
FILTER regex(?name, "^C2H2O2$")
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

result = fire_query(t).decode('utf-8')
print(result)

print('Here we go')