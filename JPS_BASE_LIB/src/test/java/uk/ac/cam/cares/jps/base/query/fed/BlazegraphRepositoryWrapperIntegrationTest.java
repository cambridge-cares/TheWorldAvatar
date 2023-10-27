package uk.ac.cam.cares.jps.base.query.fed;

import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.jena.ontology.OntModel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.fed.BlazegraphRepositoryWrapper;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer;

public class BlazegraphRepositoryWrapperIntegrationTest extends TestCase {
	
	static final Logger LOGGER = LogManager.getLogger(BlazegraphRepositoryWrapperIntegrationTest.class);
	
	@Test
	public void testGetParts1() {
		String serviceUrl = "http://localhost:8080/blazegraph";
		String[] parts = BlazegraphRepositoryWrapper.getParts(serviceUrl);
		assertEquals(serviceUrl, parts[0]);
		assertNull(parts[1]);
	}
	
	@Test
	public void testGetParts2() {
		String serviceUrl = "http://localhost:8080/blazegraph";
		String namespace = "anynamespace";
		String endpointUrl = serviceUrl + BlazegraphRepositoryWrapper.getPathForBlazegraph(namespace);
		String[] parts = BlazegraphRepositoryWrapper.getParts(endpointUrl);
		assertEquals(serviceUrl, parts[0]);
		assertEquals(namespace, parts[1]);
	}
	
	@Test
	public void testCreateNamespace() {
		
		String serviceUrl = TripleStoreProvider.getServiceUrl(TripleStoreProvider.ID_BLAZEGRAPH_1);
		BlazegraphRepositoryWrapper wrapper = new BlazegraphRepositoryWrapper(serviceUrl);

		String newnamespace = "testEmpty";
		Properties props = TripleStoreProvider.readStandardNamespacePropertiesForBlazegraph();
		wrapper.createNamespace(newnamespace, props);
		
		// assert namespace 
		List<String> namespaces = wrapper.queryNamespaces(false);
		LOGGER.debug("found namespaces after creating new name space=" + namespaces.toString());
		String message = "new namespace was not found, namespace=" + newnamespace; 
		assertTrue(message, namespaces.contains(newnamespace));

		// assert queries are possible and empty new dataset returns empty result
		String sparql = "SELECT * WHERE { ?s ?p ?o }";
		String result = wrapper.query(sparql, newnamespace);
		JSONArray array = new JSONArray(result);
		assertEquals(0, array.length());
	}
	
	@Test
	public void testQueryServiceDescription() {
		
		String serviceUrl = TripleStoreProvider.getServiceUrl(TripleStoreProvider.ID_BLAZEGRAPH_1);
		//String serviceUrl = "http://localhost:8091/blazegraph";
		BlazegraphRepositoryWrapper wrapper = new BlazegraphRepositoryWrapper(serviceUrl);

		String namespace = TripleStoreProvider.NAMESPACE_WIKIDATA_SMALL;	
		String endpointUrl = TripleStoreProvider.getEndpointUrl(namespace);
		
		// assert that data can be queried and the count is correct
		String sparql = "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\r\n"
				+ "SELECT (COUNT(DISTINCT ?s) as ?scount) WHERE { ?s wdt:P662 ?o }";
		String result = wrapper.query(sparql, namespace);
		JSONArray array = new JSONArray(result);
		int actualCount = array.getJSONObject(0).getInt("scount");
		int expectedCount = 581;
		assertEquals(expectedCount, actualCount);
		
		// assert that the service description has the same triple number for predicate P662 as the count
		String serviceDescr = ServiceDescriptionIndexer.queryServiceDescription(endpointUrl);
		OntModel model = JenaHelper.createModel();
		JenaHelper.readFromString(serviceDescr, model);
		Map<String, Long> map = ServiceDescriptionIndexer.getNumberOfPropertyTriples(model);
		long actualTripleNumber = map.get("http://www.wikidata.org/prop/direct/P662");
		assertEquals(expectedCount, actualTripleNumber);
	}
}
