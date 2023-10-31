package uk.ac.cam.cares.jps.base.query.fed;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.riot.Lang;
import org.json.JSONArray;
import org.junit.Ignore;

import junit.framework.AssertionFailedError;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

@Ignore("The code this tests is not used and takes a long time to run.")
public class DatasetGeneratorTest extends QueryProvider {
	
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
	
	public void testSupplierOptional() {
		Supplier<Object> supplier = DatasetGenerator.supplierConstant("A");
		supplier = DatasetGenerator.supplierOptional(supplier, 2);
		assertEquals("A", supplier.get());
		assertEquals("A", supplier.get());
		assertNull(supplier.get());
		assertNull(supplier.get());
	}
	
	public void testGetOrderedVariableNames() {
		
		DatasetGenerator generator = new DatasetGenerator("").
				generator("?scfEnergy", DatasetGenerator.supplierConstant("constant1")).
				generator("?scfEnergyValue", DatasetGenerator.supplierConstant("constant2")); 
		
		List<String> varNames = generator.getOrderedVariableNames();
		assertEquals("?scfEnergyValue", varNames.get(0));
		assertEquals("?scfEnergy", varNames.get(1));
	}

	public void testBuild() {
		DatasetGenerator generator = new DatasetGenerator("").
				pattern("?species ontospecies:casRegistryID ?crid").
				pattern("?species ontospecies:hasStandardEnthalpyOfFormation ?enthalpy").
				pattern("?enthalpy ontospecies:value ?enthalpyOfFormationValue").
				generator("?species", DatasetGenerator.supplierConstant("SPECIES")).
				generator("?crid", DatasetGenerator.supplierConstant("CRID")).
				generator("?enthalpy", DatasetGenerator.supplierConstant("ENTHALPY")).
				generator("?enthalpyOfFormationValue", DatasetGenerator.supplierConstant("ENTHALPYVALUE"));
		
		String dataset = generator.generateVariableValues(1).build();
		
		String expected = "\r\n"
				+ "\r\n"
				+ "SPECIES ontospecies:casRegistryID CRID .\r\n"
				+ "SPECIES ontospecies:hasStandardEnthalpyOfFormation ENTHALPY .\r\n"
				+ "ENTHALPY ontospecies:value ENTHALPYVALUE .\r\n"
				+ "\r\n";
		
		assertEquals(expected, dataset);
	}
	
	public void testBuildWithSupplierOptional() {
		
		DatasetGenerator generator = new DatasetGenerator("").
				pattern("?species ontospecies:casRegistryID ?crid").
				pattern("?species ontospecies:hasStandardEnthalpyOfFormation ?enthalpy").
				generator("?species", DatasetGenerator.supplierConstant("SPECIES")).
				generator("?crid", DatasetGenerator.supplierConstant("CRID"), 2). // 2 time CRID, then null
				generator("?enthalpy", DatasetGenerator.supplierConstant("ENTHALPY"));
		
		String dataset = generator.generateVariableValues(4).build();
		
		//System.out.println("XXX");
		//System.out.println(dataset);
		//System.out.println("YYY");
		
		String expected = "\r\n"
				+ "\r\n"
				+ "SPECIES ontospecies:casRegistryID CRID .\r\n"
				+ "SPECIES ontospecies:hasStandardEnthalpyOfFormation ENTHALPY .\r\n"
				+ "\r\n"
				+ "SPECIES ontospecies:casRegistryID CRID .\r\n"
				+ "SPECIES ontospecies:hasStandardEnthalpyOfFormation ENTHALPY .\r\n"
				+ "\r\n"
				+ "SPECIES ontospecies:hasStandardEnthalpyOfFormation ENTHALPY .\r\n"
				+ "\r\n"
				+ "SPECIES ontospecies:hasStandardEnthalpyOfFormation ENTHALPY .\r\n"
				+ "\r\n";
		
		assertEquals(expected, dataset);
	}
	
	public void testGenerateOntoSpeciesOntoCompChem() {
		
		int sizeJoin = 25;
		String[] datasets = DatasetProvider.generateOntoSpeciesOntoCompChem(100, 200, sizeJoin, -1);
		
		// merge the triple of both datasets into one RDF graph
		OntModel model = JenaHelper.createModel();	
		JenaHelper.readFromString(datasets[0], model, Lang.TURTLE);
		JenaHelper.readFromString(datasets[1], model, Lang.TURTLE);
		
		// assert that the non-federated query has result set of size sizeJoin
		setQueryFormatParams(false, false, false);
		Query query = getSparqlOntoSpeciesOntoCompChemLarge();	
		ResultSet result = JenaHelper.query(model, query.sparql);
		String resultW3C = JenaResultSetFormatter.convertToJSONW3CStandard(result);
		JSONArray ja = JenaResultSetFormatter.convertToSimplifiedJsonArray(resultW3C);
		assertEquals(sizeJoin, ja.length());
	}
}
