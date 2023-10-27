package uk.ac.cam.cares.jps.base.query.fed;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.jena.ontology.OntModel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer.PostingsListElement;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer.ServiceDescriptionSummary;

public class ServiceDescriptionIndexerTest extends TestCase {
	
	static final Logger LOGGER = LogManager.getLogger(ServiceDescriptionIndexerTest.class);

	private static ServiceDescriptionIndexer smallTestIndexer = null;
	
	private static String[] filenames = new String[] {
			"service_descr_local_ontospecies.rdf", //"service_descr_claudius_ontospecies.rdf",
			"service_descr_claudius_ontokin.rdf",
			"service_descr_citieskg_singaporeEPSG24500.rdf",
			"service_descr_claudius_sgbiodieselplants.rdf",
			"service_descr_local_doe_chemrxn.rdf",
			"service_descr_local_lab_1.rdf",
			"service_descr_local_lab_2.rdf",
			"service_descr_local_ontocompchemcloned.rdf", //"service_descr_claudius_ontocompchem.rdf"
			"service_descr_wikidata_small.rdf"
		};
	
	static String getServiceDescrPath(int index) {
		return "./src/test/resources/FedQuery/ServiceDescriptions/" + filenames[index];
	}
	
	public static ServiceDescriptionIndexer getSmallTestIndexer() {
		if (smallTestIndexer == null) {
			LOGGER.debug("initializing full indexer");
			smallTestIndexer = new ServiceDescriptionIndexer();
			for (int i=0; i<filenames.length; i++) {
				smallTestIndexer.addServiceDescription(getServiceDescrPath(i));
			}
		}
		return smallTestIndexer;
	}

	public void testGetNumberOfClassTriples() {
		ServiceDescriptionIndexer indexer = new ServiceDescriptionIndexer();
		String path = getServiceDescrPath(0);
		OntModel model = JenaHelper.createModel(path);
		Map<String, Long> map = indexer.getNumberOfClassTriples(model);
		assertEquals(14, map.keySet().size());
		long number = map.get("http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#MolecularWeight");
		assertEquals(5641, number);
	}
	
	public void testGetNumberOfPropertyTriples() {
		ServiceDescriptionIndexer indexer = new ServiceDescriptionIndexer();
		String path = getServiceDescrPath(0);
		OntModel model = JenaHelper.createModel(path);
		Map<String, Long> map = indexer.getNumberOfPropertyTriples(model);
		assertEquals(34, map.keySet().size());
		long number = map.get("http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#casRegistryID");
		assertEquals(9690, number);
	}
	
	public void testAddOneServiceDescription() {
		ServiceDescriptionIndexer indexer = new ServiceDescriptionIndexer();
		String path = getServiceDescrPath(0);
		indexer.addServiceDescription(path);
		
		List<ServiceDescriptionSummary> summaries = indexer.getSummaries();
		assertEquals(1, summaries.size());
		assertEquals(path, summaries.get(0).path);
		
		Set<String> keys = indexer.getKeys();
		assertEquals(48, keys.size());
		
		for (String key : keys) {
			Set<PostingsListElement> postingsList = indexer.getPostingsList(key);
			assertEquals(1, postingsList.size());
		}
	}
	
	public void testAddTwoServiceDescriptions() {
		ServiceDescriptionIndexer indexer = new ServiceDescriptionIndexer();
		// add first description
		String path1 = getServiceDescrPath(1);
		indexer.addServiceDescription(path1);
		
		Set<String> keys = indexer.getKeys();
		assertEquals(183, keys.size());
		
		// add second description
		String path2 = getServiceDescrPath(0);
		indexer.addServiceDescription(path2);
		
		keys = indexer.getKeys();
		// 15 common keys in OntoKin and OntoSpecies datasets
		// 216 = 183 + 48 - 15
		assertEquals(216 , keys.size());
		
		List<ServiceDescriptionSummary> summaries = indexer.getSummaries();
		assertEquals(2, summaries.size());
		assertEquals(path1, summaries.get(0).path);
		assertEquals(path2, summaries.get(1).path);
		
		// assert 15 common keys
		List<String> commonKeys = new ArrayList<String>();
		for (String key : keys) {
			Set<PostingsListElement> postingsList = indexer.getPostingsList(key);
			if (postingsList.size() == 2) {
				commonKeys.add(key);
			}
		}
		assertEquals(15, commonKeys.size());
		LOGGER.debug("common keys (relations, types) between ontospecies and ontokin= " + commonKeys);
		assertTrue(commonKeys.contains("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#GasPhase"));
	}
	
	public void testFillSummary() {
		ServiceDescriptionIndexer indexer = new ServiceDescriptionIndexer();
		ServiceDescriptionIndexer.ServiceDescriptionSummary summary = indexer.createSummary();
		String path = getServiceDescrPath(0);
		OntModel model = JenaHelper.createModel(path);
		indexer.fillSummary(summary, path, model);
		assertEquals(491495l, (long) summary.ntriples);
		assertEquals(134368l, (long) summary.nentities);
		assertEquals(34l, (long) summary.nproperties);
		assertEquals(14l, (long) summary.nclasses);
		assertEquals("http://localhost:8080/blazegraph/namespace/ontospecies/sparql", summary.endpointURL);
	}
	
	public void testConjunctiveQuery_1_FourKeysOntoSpeciesOntoKin() {
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		
		// both service descriptions contain "GasPhase"
		String[] keys = new String[] {
			"http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#GasPhase"
		};
		List<ServiceDescriptionSummary> list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(2, list.size());
		
		// only the first service description contains "hasPhase"
		keys = new String[] {
			"http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasPhase"
		};
		list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(1, list.size());
		
		// only the first service description contains the following four keys
		keys = new String[] {
			"http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#GasPhase", 
			"http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasPhase",
			"http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#Element",
			"http://www.w3.org/2000/01/rdf-schema#label"
		};
		list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(1, list.size());
		assertEquals(getServiceDescrPath(0), list.get(0).path);
	}
	
	public void testConjunctiveQuery_2_BiodieselCitiesKG() {
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		
		// keys for the first subquery concerning Biodiesel plant in Singapore
		String[] keys = new String[] {
			"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#isCostOfPlantItem",
			"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue",
			"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue"
		};
		List<ServiceDescriptionSummary> list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(1, list.size());
		assertEquals(getServiceDescrPath(3), list.get(0).path);
	}
		
	public void testConjunctiveQuery_3_BiodieselCitiesKG() {		
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		
		// keys for the second subquery concerning Biodiesel plant in Singapore
		String[] keys = new String[] {
			"http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#attrName",
			"http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#uriVal",
			"http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#cityObjectId",
			"http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#EnvelopeType",
			// the following predicates are Bigdata specific and used for its geo:search service
			// they don't appear explicitely as part of triples of any dataset 
			// and their triple counts are thus zero
			//"http://www.bigdata.com/rdf/geospatial#predicate",
			//"http://www.bigdata.com/rdf/geospatial#searchDatatype",
			//"http://www.bigdata.com/rdf/geospatial#customFields",
			//"http://www.bigdata.com/rdf/geospatial#customFieldsLowerBounds",
			//"http://www.bigdata.com/rdf/geospatial#customFieldsUpperBounds"
		};
		List<ServiceDescriptionSummary> list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(1, list.size());
		assertEquals(getServiceDescrPath(2), list.get(0).path);
	}
	
	public void testConjunctiveQuery_4_Automated_Lab() {		
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		
		// keys for the first subquery concerning labs
		String[] keys = new String[] {
			"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#hasYield",
			"http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue",
			"http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue",
			"http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit"				
		};
		List<ServiceDescriptionSummary> list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(2, list.size());
		String firstPath = list.get(0).path;
		String secondPath = list.get(1).path;
		if (firstPath.contains("lab_1")) {
			assertEquals(getServiceDescrPath(5), firstPath);
			assertEquals(getServiceDescrPath(6), secondPath);
		} else {
			assertEquals(getServiceDescrPath(6), firstPath);
			assertEquals(getServiceDescrPath(5), secondPath);
		}
	}
	
	public void testConjunctiveQuery_5_Automated_Lab() {		
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		
		// keys for the second subquery concerning DoE
		String[] keys = new String[] {
			"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#utilisesHistoricalData",
			"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#refersTo"
		};
		List<ServiceDescriptionSummary> list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(1, list.size());	
		assertEquals(getServiceDescrPath(4), list.get(0).path);
	}
	
	public void testOntoCompChemHasUniqueSpecies () {
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		
		// keys for the second subquery concerning DoE
		String[] keys = new String[] {
			"http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#hasUniqueSpecies"
		};
		List<ServiceDescriptionSummary> list = indexer.conjunctiveQuery(Arrays.asList(keys));
		assertEquals(1, list.size());	
		assertEquals(getServiceDescrPath(7), list.get(0).path);
		
	}
}
