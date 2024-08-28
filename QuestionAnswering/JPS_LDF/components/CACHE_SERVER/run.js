const newEngine = require('@comunica/actor-init-sparql').newEngine;

const myEngine = newEngine();

basic_query = `
		PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
		
		SELECT  DISTINCT   ?Equation ?MechanismName
        WHERE  {
			
			
         ?Phase ontokin:containedIn ?MechanismIRI .
		 ?MechanismIRI rdfs:label ?MechanismName .

		 {
			 SELECT DISTINCT ?Phase ?Equation
			 WHERE {
				 
				 ?reaction ontokin:hasEquation ?Equation .
				 ?reaction ontokin:belongsToPhase ?Phase .
			 }
			 
		 } 
 
        }  
 
`;


test_query =`


PREFIX rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:     <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:  <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation  
WHERE  {	
  	?reaction ontokin:hasEquation ?Equation .
    # FILTER regex(str(?Equation), ".*=] %s .*| .*=].* %s$")
    # FILTER regex(str(?Equation), ".*=] %s .*| .*=].* %s$")
}  

`


test_query_reaction_rate = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation ?ActivationEnergy  
WHERE  {	
 
 		 ?ArrheniusCoefficient 	ontokin:hasActivationEnergy ?ActivationEnergy ;
								ontokin:hasActivationEnergyUnits  ?ActivationEnergyUnits  ;
								ontokin:hasPreExponentialFactor ?PreExponentialFactor ;
								ontokin:hasPreExponentialFactorUnits ?PreExponentialFactorUnits ;
								ontokin:hasTemperatureExponent ?TemperatureExponent ;
								ontokin:hasTemperatureExponentUnits ?TemperatureExponentUnits .
 
	{
	SELECT ?reaction ?Equation ?ArrheniusCoefficient
	WHERE {
			?reaction <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasArrheniusCoefficient> ?ArrheniusCoefficient .
			?reaction ontokin:hasEquation ?Equation . 
		}
	}
	

	}   LIMIT 1


`;


test_is_reversible = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?Equation ?isReversible
WHERE  {		 
 
			?reaction ontokin:isReversible ?isReversible .
			?reaction ontokin:hasEquation ?Equation .
	 
	}  LIMIT 1

`;


test_ontocompchem = `


PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT     ?SpinMultiplicity
WHERE  {
?g_calculation rdf:type ontocompchem:G09 .
?g_calculation ontocompchem:hasInitialization ?initialization .
?initialization gc:hasMoleculeProperty ?molecule_property .
?molecule_property gc:hasName "C8H14" .
 

# ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
?GeometryOptimization    gc:hasMolecule    ?Molecule .
?Molecule  ontocompchem:hasSpinMultiplicity ?SpinMultiplicity .
} LIMIT 1



`;

test_ontocompchem_simple = `
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

 { 
	SELECT DISTINCT ?g_calculation ?name
	WHERE {
       
	   ?g_calculation ontocompchem:hasInitialization ?initialization .
	   ?initialization gc:hasMoleculeProperty ?molecule_property .	
   	   ?molecule_property gc:hasName ?name .

	   ?molecule_property gc:hasName "C8H14" .
	  
	} LIMIT 1 
 } 
} LIMIT 1
 
`;
test_ontokin = `

PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?POLARIZABILITY ?Unit
{

         ?TransportModel ontokin:hasPolarizability ?POLARIZABILITY .
     ?Species ontokin:hasTransportModel ?TransportModel .
     ?Species rdfs:label ?label .
         ?Species rdfs:label "C2H2O2" .

          OPTIONAL{
                ?TransportModel ontokin:hasPolarizabilityUnits ?Unit .
     }

}  LIMIT 1



`

test_formal_charge = `
PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT ?name ?FormalCharge_value  ?unit_short
WHERE  {
   ?Molecule gc:hasFormalCharge  ?FormalCharge .
   ?FormalCharge gc:hasValue ?FormalCharge_value . 
   ?GeometryOptimization    gc:hasMolecule    ?Molecule .
   ?g_calculation  gc:isCalculationOn    ?GeometryOptimization .
 { 
	SELECT DISTINCT ?g_calculation ?name
	WHERE {
       
	   ?g_calculation ontocompchem:hasInitialization ?initialization .
	   ?initialization gc:hasMoleculeProperty ?molecule_property .	
   	   ?molecule_property gc:hasName ?name .
	   ?molecule_property gc:hasName "C3H6" .
	  
	} LIMIT 1 
 } 
 
 OPTIONAL {
    ?FormalCharge gc:hasUnit ?unit .
    BIND(REPLACE(STR(?unit),"http://purl.org/gc/","") AS ?unit_short) .
}
 
} LIMIT 1

`


test_electronic_energy = `

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
?molecule_property gc:hasName "C2H2O2" .
 # ============ to match molecule =========================
?g_calculation  gc:isCalculationOn    ?ScfEnergy .
?ScfEnergy    gc:hasElectronicEnergy  ?x .
?x            gc:hasValue             ?Electronic_energy .

OPTIONAL {
?x gc:hasUnit ?unit .
BIND(REPLACE(STR(?unit),"http://data.nasa.gov/qudt/owl/unit#","") AS ?unit_short) .
} # http://data.nasa.gov/qudt/owl/unit#Hartree
} LIMIT 1
`


 


myEngine.invalidateHttpCache();


console.time('Execution time');
var query = test_query;

	(async () => {
		const result = await myEngine.query(query, {
		 sources: ['http://localhost:8080/ldfserver/ontokin'], products:["OH"], reactants:["H"]  
		 // sources: ['http://localhost:8080/ldfserver/ontocompchem'], products:['placeholder'].sort(), reactants:["placeholder"].sort()
		});


		const bindings = await result.bindings();
		 
		let full_result = [];
		for (let binding of bindings){
			let row = parse_bindings(binding);
			full_result.push(row);
		}
		console.log(JSON.stringify(full_result));
		console.timeEnd('Execution time');
		
		
	})();


function parse_bindings(binding) {
	let _rst = [];
    let _obj = binding.toObject();
	let _keys = Object.keys(_obj);
	let	_row = {};

	for (let key of _keys){
		value = _obj[key].value;
		key =  key.replace('?', '') ;
		_row[key] = value;
	}
	
	return _row
 }


