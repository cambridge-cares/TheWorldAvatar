package uk.ac.cam.cares.jps.scenario.kb.test;

import java.io.FileNotFoundException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

public class TestKnowledgeBaseFuseki extends TestKnowledgeBaseAllImplementations {

	public void setUp() {
		setUpFusekiDirect();
		printTime(null);
	}
	
	protected void putAndUpdateE303Provenance(String target, String provenanceName) throws FileNotFoundException {
		String marker = "2.98";
		putE303Load(target, marker);
		String value = queryE303MarkerValue(target);
		assertEquals(marker, value);

		String sparqlupdate = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " + 
				"INSERT DATA { " +
				"GRAPH <" + target + "> { " +
				"<http://example.com/" + provenanceName + "> dcterms:created \"2019-10-20T13:25:13.857\"^^xsd:dateTime . " + 
				"}" +
				" }";
		
		//client().update(target, sparqlupdate);
		client().update(null, sparqlupdate);
		
		String sparqlquery = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"SELECT ?s ?p ?o WHERE { ?s dcterms:created ?o } ";
		String result = client().query(target, sparqlquery);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://example.com/" + provenanceName, subject);
	}
	
//	public void testTMP() {
//		
//
//		String namedGraph = datasetUrl + "/" + UUID.randomUUID().toString().replace("-", "");
//		
//		
//		String sparql = "PREFIX dcterms:<http://purl.org/dc/terms/> \r\n" + 
//				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> \r\n" + 
//				"PREFIX JPSAGEN:<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#> \r\n" + 
//				"PREFIX example:<http://example.com/jps/> \r\n" + 
//				"INSERT DATA { GRAPH <" + namedGraph + "> { \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:format \"text/csv\" . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:created \"2019-11-01T04:24:10.633\"^^xsd:dateTime . \r\n" + 
//				"<http://example.com/jps/agents/myfancyagent> a dcterms:Agent . \r\n" + 
//				"<http://example.com/jps/agents/myfancyagent> a JPSAGEN:Service . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:creator <http://example.com/jps/agents/myfancyagent> . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:date \"2019-11-01T04:24:10.633\"^^xsd:dateTime . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:isPartOf <http://localhost:8080/jps/scenario/testAnnotate> . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:subject <http://dbpedia.org/resource/Air_pollution> . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> dcterms:subject <http://dbpedia.org/resource/Singapore> . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> example:admsmodel example:model1.apl . \r\n" + 
//				"<http://example.com/jps/some/path/output.csv> example:inputparam <http://example.com/ontokin/reactmeachxyz.owl> . \r\n" + 
//				"} } \r\n" + 
//				"";
//		
//		System.out.println(sparql);
//		
//		// create an empty named graph
//		StringBuffer turtle = new StringBuffer("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . ")
//				.append("@prefix foaf: <http://xmlns.com/foaf/0.1/> . ")
//				.append("@prefix ex: <http://www.example.com/fancyprops/> . ");
//				//.append("<http://example.com/xxx>").append(" rdf:type foaf:Person . ");
//		
//		client().put(namedGraph, turtle.toString(), MediaType.TEXT_TURTLE.type);
//		
//		client().update(null, sparql);
//	}
//	
//	public void testTMP2() {
//		testQueryxRandomNamedGraphsInTurtle();
//	}
//	
//	public void testTMP3() {
//		
//		String sparql = "SELECT ?g ?s ?p ?o\r\n" + 
//				"WHERE {\r\n" + 
//				"  GRAPH ?g { ?s ?p ?o }\r\n" + 
//				"}";
//		
//		String result = client().query(null, sparql);
//		
//		System.out.println(result);
//		
//		
//		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
//		System.out.println(simplified);
//		int number = simplified.getJSONArray("results").length();
//		System.out.println("NUMBER=" + number);
//	}
}
