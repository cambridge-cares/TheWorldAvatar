package uk.ac.cam.cares.jps.scenarios.test;

import java.io.FileNotFoundException;
import java.util.UUID;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

public class TestKnowledgeBaseAllImplementations extends TestKnowledgeBaseHelper {
	
	protected static final String SPARQL_COUNT_TRIPLES = "SELECT (COUNT(?s) as ?count) WHERE { ?s ?p ?o }";
	protected static final String SPARQL_COUNT_BUILDINGS = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
			+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n"
			+ "SELECT (COUNT(?bdn) as ?count) \n"
			+ "WHERE { ?bdn a citygml:BuildingType . }";
	
	private static final String SPARQL_RANDOM_NAMED = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" + 
			"PREFIX foaf: <http://xmlns.com/foaf/0.1/>\r\n" + 
			"PREFIX ex: <http://www.example.com/fancyprops/>\r\n" + 
			"SELECT ?person WHERE { ?person rdf:type foaf:Person . }";
	
	private void setUpRdf4jInMemoryDirect() {
		if (client() == null) {
			//String datasetUrl = "http://localhost:8081/jps/data/testnative";
			String datasetUrl = "http://localhost:8081/jps/data/perfinmemory";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, true);
		}
	}
	
	private void setUpRdf4jInMemoryRemote() {
		if (client() == null) {
			//String datasetUrl = "http://localhost:8081/jps/data/testnative";
			// "http://localhost:8081/jps/data/perfinmemory"
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/perfinmemory";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, false);
		}
	}
	
	private void setUpFileBasedRemote() {
		if (client() == null) {
			//String datasetUrl = "http://localhost:8081/jps/data/testnative";
			// "http://localhost:8081/jps/data/perfinmemory"
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/dataset/testfilebased";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, false);
		}	
	}
	
	public void setUp() {
		//setUpRdf4jInMemoryDirect();
		//setUpRdf4jInMemoryRemote();
		setUpFileBasedRemote();
		printTime(null);
	}
	
	public void testPutAndGetRdfFileWithAcceptAndWithoutConversion() {
		String resourceUrl = getE303LoadUrl();		
		putE303Load(datasetUrl, resourceUrl);
		String accept = MediaType.APPLICATION_RDF_XML.type;
		String result = client().get(resourceUrl, accept);
		System.out.println(result);
		assertTrue(result.contains("<rdf:RDF"));
	}
	
	public void testPutAndGetRdfFileWithAcceptAndWithConversionToTurtle() {
		String resourceUrl = getE303LoadUrl();		
		putE303Load(datasetUrl, resourceUrl);
		String accept = MediaType.TEXT_TURTLE.type;
		String result = client().get(resourceUrl, accept);
		assertEquals("@prefix", result.substring(0,7));
	}
	
	public void testPutAndQueryRdfFileWithFancyParameterUrl() throws FileNotFoundException {
		String marker = "4.42";
		String resourceUrl = "http://www.myhost.com:7777/fancyquerypath/testE-303load.owl";
		putE303Load(datasetUrl, resourceUrl, marker);
		String value = queryE303MarkerValue(resourceUrl);
		assertEquals(marker, value);
	}

	private String queryE303MarkerValue(String resourceUrl) throws FileNotFoundException {
		
		String sparql = "PREFIX powsys:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX EN_behavior:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?value WHERE { "
				+ "?s ?p EN_behavior:AbsorbedReactivePower . "
				+ "?s system:hasValue/system:numericalValue ?value"
				+ "} ";
		String result = client().query(resourceUrl, sparql);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String value = simplified.getJSONArray("results").getJSONObject(0).getString("value");
		return value;
	}
	
	private String[] createRandomIriAndTurtleContent() {
		
		String iri = datasetUrl + "/" + UUID.randomUUID().toString().replace("-", "") + ".ttl";
		String iriInBrackets = "<" + iri + ">";
		
		StringBuffer turtle = new StringBuffer("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . ")
				.append("@prefix foaf: <http://xmlns.com/foaf/0.1/> . ")
				.append("@prefix ex: <http://www.example.com/fancyprops/> . ")
				.append(iriInBrackets).append(" rdf:type foaf:Person . ");
		
		for (int i=1; i<=100; i++) {
			turtle.append(iriInBrackets).append(" ex:prop").append(i).append(" \"" + i + "\" . ");
		}
		
		return new String[] {iri, turtle.toString()};
	}
	
	public void testQueryOneRandomNamedGraphInTurtle() {
		
		String[] a = createRandomIriAndTurtleContent();
		System.out.println(a[0]);
		System.out.println(a[1]);
		
		client().put(a[0], a[1], MediaType.TEXT_TURTLE.type);
		
		System.out.println(SPARQL_COUNT_TRIPLES);
		
		String result = client().query(a[0], SPARQL_RANDOM_NAMED);
		System.out.println("RESULT = " + result);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String person = simplified.getJSONArray("results").getJSONObject(0).getString("person");
		assertEquals(a[0], person);
	}
	
	public void testQueryxRandomNamedGraphsInTurtle() {
		
		printTime(null);
		
		int number = 100;
		for (int i=1; i<=number; i++) {
			System.out.println("random graph" + i);
			String[] a = createRandomIriAndTurtleContent();		
			client().put(a[0], a[1], MediaType.TEXT_TURTLE.type);		
			String result = client().query(a[0], SPARQL_RANDOM_NAMED);
			JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
			String person = simplified.getJSONArray("results").getJSONObject(0).getString("person");
			assertEquals(a[0], person);
		}
		
		printTime("testQueryxRandomNamedGraphs with x=" + number);
	}
	
	private void putAndUpdateE303Provenance(String dataset, String target, String provenanceName) throws FileNotFoundException {
		String marker = "2.98";
		putE303Load(dataset, target, marker);
		String value = queryE303MarkerValue(target);
		assertEquals(marker, value);

		String sparqlupdate = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " + 
				"INSERT DATA { " + 
				"<http://example.com/" + provenanceName + "> dcterms:created \"2019-10-20T13:25:13.857\"^^xsd:dateTime . " + 
				"}";
		
		client().update(target, sparqlupdate);
		
		String sparqlquery = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"SELECT ?s ?p ?o WHERE { ?s dcterms:created ?o } ";
		String result = client().query(target, sparqlquery);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://example.com/" + provenanceName, subject);
	}
	
	public void testPutAndUpdateRdfFile() throws FileNotFoundException {
		String provenanceName = UUID.randomUUID().toString();
		String resourceUrl = "http://www.myhost.com:7778/fancyquerypath/testE-303load.owl";
		putAndUpdateE303Provenance(datasetUrl, resourceUrl, provenanceName);
	}
}
