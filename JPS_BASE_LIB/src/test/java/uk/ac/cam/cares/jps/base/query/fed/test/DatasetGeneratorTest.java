package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

public class DatasetGeneratorTest extends TestCase {
	
	public void testSupplierList() {
		List<Object> list = Arrays.asList(10, 20, 30);
		Supplier<Object> supplier = DatasetGenerator.supplierList(list);
		assertEquals(10, supplier.get());
		assertEquals(20, supplier.get());
		assertEquals(30, supplier.get());

		try {
			supplier.get();
			throw new AssertionFailedError("should throw an ArrayIndexOutOfBoundsException");
		} catch (ArrayIndexOutOfBoundsException e) {
		}
	}

	public void testSupplierJoin() {
		List<Object> list1 = Arrays.asList(10, 20);
		List<Object> list2 = Arrays.asList("A", "B");
		Supplier<Object> supplier = DatasetGenerator.supplierList(list2);
		Supplier<Object> joinSupplier = DatasetGenerator.supplierJoin(list1, supplier);
		assertEquals(10, joinSupplier.get());
		assertEquals(20, joinSupplier.get());
		assertEquals("A", joinSupplier.get());
		assertEquals("B", joinSupplier.get());
		try {
			joinSupplier.get();
			throw new AssertionFailedError("should throw an ArrayIndexOutOfBoundsException");
		} catch (ArrayIndexOutOfBoundsException e) {
		}
	}
	
	public void testSupplierInt() {
		Supplier<Object> supplier = DatasetGenerator.supplierInt(-1, 2);
		Set<Integer> set = new HashSet<Integer>();
		for (int i=0; i<1000; i++) {
			set.add((Integer) supplier.get());
		}
		assertEquals(4, set.size());
		for (int i=-1; i<=2; i++) {
			assertTrue(set.contains(i));
		}
	}
	
	public String[] generateOntoSpeciesOntoCompChem(int sizeOntoSpecies, int sizeOntoCompChem, int sizeJoin) {
		
		DatasetGenerator genSpecies = createGeneratorOntoSpecies();		
		genSpecies.generateVariableValues(sizeOntoSpecies);
		
		StringBuffer b = new StringBuffer();
		genSpecies.build(b);	
		return new String[] { b.toString() };
	}

	public DatasetGenerator createGeneratorOntoSpecies() {
		String prefixes = "@prefix ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> .\r\n"
				+ "@prefix ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> .\r\n"
				+ "@prefix gc: <http://purl.org/gc/> .\r\n"
				+ "@prefix kbontospecies: <http://www.theworldavatar.com/kb/ontospecies/> .\r\n";
		
		String template = "?species ontospecies:casRegistryID ?crid .\r\n"
				+ "?species ontospecies:hasAtomicBond ?atomicBond .\r\n"
				+ "?species ontospecies:hasGeometry ?geometry . \r\n"
				+ "?species ontospecies:hasStandardEnthalpyOfFormation ?enthalpy .\r\n"
				+ "?enthalpy ontospecies:value ?enthalpyOfFormationValue .\r\n";
		
		DatasetGenerator generator = new DatasetGenerator(prefixes, template);
		
		Supplier<Object> supplier = DatasetGenerator.supplierUUID("kbontospecies:Species_", ""); 
		generator.bindVariable("?species", supplier);
		supplier = DatasetGenerator.supplierUUID("\"107-18-6", "\""); 
		generator.bindVariable("?crid", supplier);
		supplier = DatasetGenerator.supplierUUID("\"5 1 1 6 1 1 1 2 2 8 3 1 2 3 1 2 7 1 10 4 1 3 4 1 3 9 1", "\""); 
		generator.bindVariable("?atomicBond", supplier);
		supplier = DatasetGenerator.supplierUUID("\"C -0.923508 -0.555332 -1.207091 C -0.508887 -0.407765 0.049627 C 0.368786 0.714318 0.522948 O 1.573496 0.239793 1.13319 H -0.648316 0.15453 -1.982095 H -1.557937 -1.381879 -1.506737 H -0.792092 -1.134554 0.808132 H 0.589914 1.398858 -0.30644 H -0.133639 1.289675 1.305278 H 2.032282 -0.317645 0.495588", "\""); 
		generator.bindVariable("?geometry", supplier);
		supplier = DatasetGenerator.supplierUUID("kbontospecies:StandardEnthalpyOfFormation_", ""); 
		generator.bindVariable("?enthalpy", supplier);
		supplier = DatasetGenerator.supplierConstant("-123.6"); 
		generator.bindVariable("?enthalpyOfFormationValue", supplier);
		
		return generator;
	}
	
	public void testGenerateOntoSpeciesOntoCompChem() {
		String[] datasets = generateOntoSpeciesOntoCompChem(3, 4, 2);
		
		System.out.println(datasets[0]);
	}
}
