package uk.ac.cam.cares.jps.scenario.kb.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.UUID;

import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAbstract;

public class TestKnowledgeBaseClient extends TestKnowledgeBaseAllImplementations {
		
	public void setUp() {
		JPSHttpServlet.disableScenario();
		setUpFileBasedRemote();
		printTime(null);
	}
	
	public void testPrintSupportedRdfFormats() {
		for (RDFFormat current : KnowledgeBaseAbstract.SUPPORTED_RDF_FORMATS) {
			System.out.println(current.getDefaultFileExtension() + ", " + current.getFileExtensions() + ", "
					+ current.getDefaultMIMEType() + ", " + current.getMIMETypes() + ", " + current.supportsContexts());
		}
	}
	
	public void testgetRDFFormatFromFileType() {
		String fileName = "1dabc1f0a8024ad8ab3ce90e18ae802a.ttl";
		RDFFormat format = KnowledgeBaseAbstract.getRDFFormatFromFileType(fileName);
		assertEquals(RDFFormat.TURTLE.getName(), format.getName());
	}
	
	/**
	 * If both datasetUrl and targetUrl are given, datasetUrl must be a know datasetUrl such as
	 * .../jps/data/test instead of .../jps/data/test/hey
	 */
	public void testWrongDatasetUrl() {
		String dataset = "/jps/data/test/hey";
		String target = "http://localhost:8081/jps/data/test/hey/testE-303load.owl";
		try {
			putE303Load(dataset, target);
		} catch (JPSRuntimeException e) {
			return;
		}
		assertTrue("no exception was thrown", true);
	}
	
	public void testPutAndGetNonRdfFile() {
		String path = "/jps/dataset/testfilebased/testputandget";
		String body = UUID.randomUUID().toString();
		KnowledgeBaseClient.put(null, path, body, null);

		String accept = null;
		String result = KnowledgeBaseClient.get(null, path, accept);
		assertEquals(body, result);
	}
	
	public void testPutAndGetRdfFileWithAcceptAndWithoutConversionForPath() {
		String path = complete("/jps/dataset/testfilebased/testE-303load.owl");
		String body = putE303LoadRemoteKBCOnly(null, path, null);
		String accept = MediaType.APPLICATION_RDF_XML.type;
		String result = KnowledgeBaseClient.get(null, path, accept);
		assertEquals(body,  result);
	}
	
    private String complete(String path) {
        return KeyValueManager.getServerAddress() + path;
    }
	
	public void testPutAndGetRdfFileWithAcceptAndWithConversionToTurtle() {
		String path = complete("/jps/dataset/testfilebased/testE-303load.owl");
		putE303LoadRemoteKBCOnly(null, path, null);
		String accept = MediaType.TEXT_TURTLE.type;
		String result = KnowledgeBaseClient.get(null, path, accept);
		assertEquals("@prefix",  result.substring(0,7));
	}
	
	public void testPutAndGetRdfFileWithAcceptAndConversionToJSONLD() {
		String path = complete("/jps/dataset/testfilebased/testE-303load.owl");
		putE303LoadRemoteKBCOnly(null, path, null);
		String accept = MediaType.APPLICATION_LD_JSON.type;
		String result = KnowledgeBaseClient.get(null, path, accept);
		assertTrue( result.contains("@id"));
		assertTrue( result.contains("@type"));
	}
	
	public void internClientSparqlQueryDirect(String dataset, String target) {
		putE303LoadRemoteKBCOnly(dataset, target, null);
		String sparql = "SELECT ?s ?p ?o WHERE { ?s ?p <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerLoad> } ";
		String result = KnowledgeBaseClient.query(dataset, target, sparql);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/E-303load.owl#E-303load", subject);
	}
	
	public void testClientSparqlQueryDirect() {
		String dataset = null;
		String target = complete("/jps/dataset/testfilebased/testE-303load.owl");
		internClientSparqlQueryDirect(dataset, target);
	}	
	
	public void internClientSparqlUpdateDirect(String dataset, String target) {
		putE303LoadRemoteKBCOnly(dataset, target, null);

		String sparqlupdate = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " + 
				"INSERT DATA { " + 
				"<http://example.com/zzz> dcterms:created \"2019-10-20T13:25:13.857\"^^xsd:dateTime . " + 
				"}";
		
		KnowledgeBaseClient.update(dataset, target, sparqlupdate);
		
		// assert
		String sparqlquery = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"SELECT ?s ?p ?o WHERE { ?s dcterms:created ?o } ";
		String result = KnowledgeBaseClient.query(dataset, target, sparqlquery);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://example.com/zzz", subject);
	}
	
	public void testClientSparqlUpdateDirect() {
		String dataset = null;
		String target = complete("/jps/dataset/testfilebased/testupdate/testE-303load.owl");
		internClientSparqlUpdateDirect(dataset, target);
	}
	
	public void testClientSparqlUpdateDirectWithFancyParameterUrl() {
		String target = "http://localhost:9090/fancyupdatepath/testE-303load.owl";
		internClientSparqlUpdateDirect(datasetUrl, target);
	}
	
	public void testPutAndGetNonRdfFileWithFancyParameterUrl() {

		String target = "http://localhost:9090/fancy/path/some.owl";
		String body = UUID.randomUUID().toString();
		KnowledgeBaseClient.put(datasetUrl, target, body, null);

		String accept = null;
		String result = KnowledgeBaseClient.get(datasetUrl, target, accept);
		assertEquals(body,result);
	}
	
	private void assertKnowledgeBaseAbstracQuery(InputStream inputStream) {
		String sparql = "SELECT ?s ?p ?o WHERE { ?s ?p <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerLoad> } ";

		String result = KnowledgeBaseAbstract.query(inputStream, RDFFormat.RDFXML, sparql);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/E-303load.owl#E-303load", subject);
	}
	
	public void testKnowledgeBaseAbstractQueryWithInputStreamFromString() throws FileNotFoundException {
		String filePath = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/E-303load.owl";
		String content = FileUtil.readFileLocally(filePath);
		InputStream inputStream = FileUtil.stringToInputStream(content);
		assertKnowledgeBaseAbstracQuery(inputStream);
	}
	
	public void testKnowledgeBaseAbstractQueryWithInputStreamFromFile() throws FileNotFoundException {	
		String filePath = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/E-303load.owl";
		File file = new File(filePath);
		InputStream inputStream = new FileInputStream(file);
		assertKnowledgeBaseAbstracQuery(inputStream);
	}
}