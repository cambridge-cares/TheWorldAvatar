package uk.ac.cam.cares.jps.scenario.kb.test;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.UUID;

import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.scenario.kb.KnowledgeBaseAbstract;

public abstract class TestKnowledgeBaseAllImplementations extends TestKnowledgeBaseHelper {
	
	protected static final String SPARQL_COUNT_BUILDINGS = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
			+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n"
			+ "SELECT (COUNT(?bdn) as ?count) \n"
			+ "WHERE { ?bdn a citygml:BuildingType . }";
	
	private static final String SPARQL_RANDOM_NAMED = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" + 
			"PREFIX foaf: <http://xmlns.com/foaf/0.1/>\r\n" + 
			"PREFIX ex: <http://www.example.com/fancyprops/>\r\n" + 
			"SELECT ?person WHERE { ?person rdf:type foaf:Person . }";
	
	protected void setUpRdf4jNativeDirect() {
		if (client() == null) {
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/testrdf4jnative";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, true);
		}
	}
	
	protected void setUpRdf4jInMemoryDirect() {
		if (client() == null) {
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/testrdf4jinmemory";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, true);
		}
	}
	
	protected void setUpRdf4jInMemoryRemote() {
		if (client() == null) {
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/testrdf4jinmemory";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, false);
		}
	}
	
	protected void setUpBlazegraphDirect() {
		if (client() == null) {
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/testblazegraph";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, true);
		}
	}
	
	protected void setUpFusekiDirect() {
		if (client() == null) {
			//String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/testfuseki";
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/data/meta";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, true);
		}
	}
	
	protected void setUpFileBasedRemote() {
		if (client() == null) {
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/dataset/testfilebased";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, false);
		}	
	}
	
	public void setUp() {
		//setUpRdf4jNativeDirect();
		//setUpRdf4jInMemoryDirect();
		setUpRdf4jInMemoryRemote();
		//setUpBlazegraphDirect();
		//setUpFusekiDirect();
		//setUpFileBasedRemote();
		printTime(null);
	}
	
	public void testPutAndGetRdfFileWithAcceptAndWithoutConversion() {
		String resourceUrl = getE303LoadUrl();		
		putE303Load(resourceUrl);
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
		System.out.println(result);
		assertEquals("@prefix", result.substring(0,7));
	}
	
	public void testPutAndQueryRdfFileWithFancyParameterUrl() throws FileNotFoundException {
		String marker = "4.42";
		String resourceUrl = "http://www.myhost.com:7777/fancyquerypath/testE-303load.owl";
		putE303Load(resourceUrl, marker);
		String value = queryE303MarkerValue(resourceUrl);
		assertEquals(marker, value);
	}

	protected String queryE303MarkerValue(String resourceUrl) throws FileNotFoundException {
		
		String sparql = "PREFIX powsys:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX EN_behavior:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?value WHERE { "
				+ "?s ?p EN_behavior:AbsorbedReactivePower . "
				+ "?s system:hasValue/system:numericalValue ?value"
				+ "} ";
		String result = client().query(resourceUrl, sparql);
		//Unfortunately, client().query doesn't tell if it's return from agent or direct Query
		JSONObject simplified;
		try {
			simplified = JenaResultSetFormatter.convertToSimplifiedList(new JSONObject(result).getString("result"));
			
		}catch (JSONException e) {
			simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		}
		
		System.out.println(simplified);
		String value = simplified.getJSONArray("results").getJSONObject(0).getString("value");
		return value;
	}
	
	private String[] createRandomIriAndTurtleContent(String iri, int from, int to) {
		
		if (iri == null) {
			iri = datasetUrl + "/" + UUID.randomUUID().toString().replace("-", "") + ".ttl";
		}
		String iriInBrackets = "<" + iri + ">";
		
		StringBuffer turtle = new StringBuffer("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . ")
				.append("@prefix foaf: <http://xmlns.com/foaf/0.1/> . ")
				.append("@prefix ex: <http://www.example.com/fancyprops/> . ")
				.append(iriInBrackets).append(" rdf:type foaf:Person . ");
		
		for (int i=from; i<=to; i++) {
			turtle.append(iriInBrackets).append(" ex:prop").append(i).append(" \"" + i + "\" . ");
		}
		
		return new String[] {iri, turtle.toString()};
	}
	
	public void testQueryOneRandomNamedGraphInTurtle() {
		int numberExampleProperties = 100;
		String[] a = createRandomIriAndTurtleContent(null, 1, numberExampleProperties);
		System.out.println(a[0]);
		System.out.println(a[1]);
		client().put(a[0], a[1], MediaType.TEXT_TURTLE.type);
		
		int resultCount = queryCount(a[0], SPARQL_COUNT_TRIPLES);
		// one additional statement "a[0] is a Person"
		assertEquals(numberExampleProperties + 1, resultCount);
		 
		String result = client().query(a[0], SPARQL_RANDOM_NAMED);
		System.out.println("RESULT = " + result);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String person = simplified.getJSONArray("results").getJSONObject(0).getString("person");
		assertEquals(a[0], person);
		
		// check that put really replaces all existing triples for the same named graph
		// and not just adds new triples
		a = createRandomIriAndTurtleContent(a[0], 201, 210);
		System.out.println(a[0]);
		System.out.println(a[1]);
		client().put(a[0], a[1], MediaType.TEXT_TURTLE.type);
		
		resultCount = queryCount(a[0], SPARQL_COUNT_TRIPLES);
		// one additional statement "a[0] is a Person"
		assertEquals(11, resultCount);
	}
	
	public void testQueryxRandomNamedGraphsInTurtle() {
		
		printTime(null);
		
		int numberExampleProperties = 1;
		int number = 1;
		for (int i=1; i<=number; i++) {
			System.out.println("random graph" + i);
			String[] a = createRandomIriAndTurtleContent(null, 1, numberExampleProperties);		
			client().put(a[0], a[1], MediaType.TEXT_TURTLE.type);		
			String result = client().query(a[0], SPARQL_RANDOM_NAMED);
			JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
			String person = simplified.getJSONArray("results").getJSONObject(0).getString("person");
			assertEquals(a[0], person);
		}
		
		printTime("testQueryxRandomNamedGraphs with x=" + number);
	}
	
	protected void putAndUpdateE303Provenance(String target, String provenanceName) throws FileNotFoundException {
		String marker = "2.98";
		putE303Load(target, marker);
		String value = queryE303MarkerValue(target);
		assertEquals(marker, value);

		String sparqlupdate = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " + 
				"INSERT DATA { " +
				//"GRAPH <" + target + "> { " +
				"<http://example.com/" + provenanceName + "> dcterms:created \"2019-10-20T13:25:13.857\"^^xsd:dateTime . " + 
				//"}" +
				" }";
		
		client().update(target, sparqlupdate);
		//client().update(null, sparqlupdate);
		
		String sparqlquery = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"SELECT ?s ?p ?o WHERE { ?s dcterms:created ?o } ";
		String result = client().query(target, sparqlquery);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(new JSONObject(result).getString("result"));
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://example.com/" + provenanceName, subject);
	}
	
	public void testPutAndUpdateRdfFile() throws FileNotFoundException {
		String provenanceName = UUID.randomUUID().toString();
		String resourceUrl = "http://www.myhost.com:7778/fancyquerypath/testE-303load.owl";
		putAndUpdateE303Provenance(resourceUrl, provenanceName);
	}
	
	public void testQueryOwlFileWithImports() throws IOException {
		
		String sparql = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?entity ?Pmaxvalue ?emissionfactor " // add the emission value as optional
				+ "WHERE {?entity  a  j1:PowerGenerator  ." 
				//+ "?entity   j2:isSubsystemOf ?plant ." // plant
				+ "?entity   j2:isModeledBy ?model ." 
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ."
				+ "?pmax  j2:hasValue ?vpmax ." 
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

				+ "?entity j4:realizes ?genprocess ." 
				+ "?genprocess j9:usesGenerationTechnology ?tech ."
				+ "?tech j9:hasEmissionFactor ?emm ."
				+ "?emm j2:hasValue ?valueemm ."
				+ "?valueemm j2:numericalValue ?emissionfactor ." 
				+ "}";


//		String filePath = "C:/JPS_DATA/workingdir/JPS_SCENARIO/scenario/testTMPCSVReactorParameter/localhost_8080/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-008.owl";
//		String content = FileUtil.readFileLocally(filePath);
//		InputStream inputStream = FileUtil.stringToInputStream(content);
		
		URL url = new URL("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-008.owl");
		InputStream inputStream = url.openStream();
		
		String result = KnowledgeBaseAbstract.query(inputStream, RDFFormat.RDFXML, sparql);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String actual = simplified.getJSONArray("results").getJSONObject(0).getString("entity");
		assertEquals("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-008.owl#EGen-008", actual);
	}
}
