package uk.ac.cam.cares.jps.base.query.fed;

import java.util.List;
import java.util.function.Supplier;

public class DatasetProvider {

	public static String[] generateOntoSpeciesOntoCompChem(int sizeOntoSpecies, int sizeOntoCompChem, int sizeJoin, int numbercrid) {
		
		DatasetGenerator genSpecies = createGeneratorOntoSpecies(numbercrid);		
		genSpecies.generateVariableValues(sizeOntoSpecies);
		String datasetSpecies = genSpecies.build();	
		
		List<Object> joinValues = genSpecies.getGeneratedValues("?species", sizeJoin);
		DatasetGenerator genCompChem = createGeneratorOntoCompChem(joinValues);
		genCompChem.generateVariableValues(sizeOntoCompChem);
		String datasetCompChem = genCompChem.build();	
		
		return new String[] { datasetSpecies, datasetCompChem };
	}

	public static DatasetGenerator createGeneratorOntoSpecies(int numbercrid) {
		String prefixes = "@prefix ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> .\r\n"
				+ "@prefix ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> .\r\n"
				+ "@prefix gc: <http://purl.org/gc/> .\r\n"
				+ "@prefix kbontospecies: <http://www.theworldavatar.com/kb/ontospecies/> .\r\n";
		
		DatasetGenerator generator = new DatasetGenerator(prefixes).
				pattern("?species ontospecies:casRegistryID ?crid").
				pattern("?species ontospecies:hasAtomicBond ?atomicBond").
				pattern("?species ontospecies:hasGeometry ?geometry").
				pattern("?species ontospecies:hasStandardEnthalpyOfFormation ?enthalpy").
				pattern("?enthalpy ontospecies:value ?enthalpyOfFormationValue");
		
		generator.generator("?species", DatasetGenerator.supplierUUID("kbontospecies:Species_", "")). 
				generator("?crid", DatasetGenerator.supplierUUID("\"107-18-6", "\""), numbercrid).
				generator("?atomicBond", DatasetGenerator.supplierUUID("\"5 1 1 6 1 1 1 2 2 8 3 1 2 3 1 2 7 1 10 4 1 3 4 1 3 9 1", "\"")).
				generator("?geometry", DatasetGenerator.supplierUUID("\"C -0.923508 -0.555332 -1.207091 C -0.508887 -0.407765 0.049627 C 0.368786 0.714318 0.522948 O 1.573496 0.239793 1.13319 H -0.648316 0.15453 -1.982095 H -1.557937 -1.381879 -1.506737 H -0.792092 -1.134554 0.808132 H 0.589914 1.398858 -0.30644 H -0.133639 1.289675 1.305278 H 2.032282 -0.317645 0.495588", "\"")).
				generator("?enthalpy", DatasetGenerator.supplierUUID("kbontospecies:StandardEnthalpyOfFormation_", "")).
				generator("?enthalpyOfFormationValue", DatasetGenerator.supplierConstant("-123.6"));
		
		return generator;
	}
	
	public static DatasetGenerator createGeneratorOntoCompChem(List<Object> joinValuesSpecies) {
		String prefixes = "@prefix ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> .\r\n"
				+ "@prefix ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> .\r\n"
				+ "@prefix gc: <http://purl.org/gc/> .\r\n"
				+ "@prefix kbontospecies: <http://www.theworldavatar.com/kb/ontospecies/> .\r\n"
				+ "@prefix kbontocompchem: <http://www.theworldavatar.com/kb/ontocompchem/> .\r\n";
		
		DatasetGenerator generator = new DatasetGenerator(prefixes).
				pattern("?compchemspecies ontocompchem:hasUniqueSpecies ?species").
				pattern("?compchemspecies gc:isCalculationOn ?scfEnergy").
				pattern("?scfEnergy a ontocompchem:ScfEnergy").
				pattern("?scfEnergy gc:hasElectronicEnergy ?scfElectronicEnergy").
				pattern("?scfElectronicEnergy gc:hasValue ?scfEnergyValue").
				pattern("?compchemspecies gc:isCalculationOn ?zeroEnergy").
				pattern("?zeroEnergy a ontocompchem:ZeroPointEnergy").
				pattern("?zeroEnergy gc:hasElectronicEnergy ?zeroElectronicEnergy").
				pattern("?zeroElectronicEnergy gc:hasValue ?zeroEnergyValue");
				
				
		Supplier<Object> joinsupplier = DatasetGenerator.supplierUUID("kbontospecies:Species_", ""); ;
		if (joinValuesSpecies != null) {
			joinsupplier = DatasetGenerator.supplierJoin(joinValuesSpecies, joinsupplier);
		}
		
		generator.generator("?species", joinsupplier).
				generator("?compchemspecies", DatasetGenerator.supplierUUID("kbontocompchem:spec_", "")).
				generator("?scfEnergy", DatasetGenerator.supplierUUID("kbontocompchem:scfenergy_", "")).
				generator("?scfElectronicEnergy", DatasetGenerator.supplierUUID("kbontocompchem:electronicenergy_", "")).
				generator("?scfEnergyValue", DatasetGenerator.supplierConstant(-314.455919684)).
				generator("?zeroEnergy", DatasetGenerator.supplierUUID("kbontocompchem:zeropointenergy_", "")).
				generator("?zeroElectronicEnergy", DatasetGenerator.supplierUUID("kbontocompchem:zeroelectronicenergy_", "")).
				generator("?zeroEnergyValue", DatasetGenerator.supplierConstant(0.219432));
		
		return generator;
	}
}
