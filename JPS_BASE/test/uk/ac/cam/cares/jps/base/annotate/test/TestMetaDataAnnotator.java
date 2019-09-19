package uk.ac.cam.cares.jps.base.annotate.test;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class TestMetaDataAnnotator extends TestCase implements Prefixes {
	
	@Override
	protected void setUp() throws Exception {
		deleteAllTestAnnotations();
	}

	@Override
	protected void tearDown() throws Exception {
		deleteAllTestAnnotations();
	}
	
	public void deleteAllTestAnnotations() {
		// before you want to perform the next delete all sparql query, know what you do and make sure you are deleting the right data set
		// in particular, you have to understand that you are deleting data on claudius if your config properties are improper !!!
		String sparql = "DELETE { ?s ?p ?o . } \r\n" +
				"WHERE { ?s ?p ?o . FILTER ( CONTAINS(STR(?s), \"example.com\") )}";
		
		MetaDataAnnotator.getSparqlService().executePost(sparql);
		
		sparql = "DELETE { ?s ?p ?o . } \r\n" +
				"WHERE { ?s ?p ?o . FILTER ( CONTAINS(STR(?o), \"example.com\") )}";
		
		MetaDataAnnotator.getSparqlService().executePost(sparql);
	}
	
	public void testXsdTimeStampFormat() {	
		long millis = 1559633714596l;
		String timestamp = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis);
		assertEquals("2019-06-04T07:35:14.596", timestamp);
		
		long actualMillis = MetaDataAnnotator.getMillisFromXsdTimeStampFormat(timestamp);
		assertEquals(millis, actualMillis);
	}
	
	private int getNumberOfResultsWithoutHeader(String sparqlcsvresult) {
		// -1 because of the header line
		//return (sparqlcsvresult.split("\n").length - 1);
		List<String[]> list = MatrixConverter.fromCsvToArray(sparqlcsvresult);
		return (list.size() - 1);
	}
	
	private String getSparqlInsertExample(String target, String scenario, String creationTime) {
		JPSContext.putScenarioUrl(scenario);
		
		MediaType mediaType = MediaType.TEXT_CSV;
		String agent = "http://example.com/jps/agents/myfancyagent";
		List<String> topics = new ArrayList<String>();
		topics.add("http://dbpedia.org/resource/Air_pollution");
		topics.add("http://dbpedia.org/resource/Singapore");
		List<String> prefixes = new ArrayList<String>();
		prefixes.add("PREFIX example:<http://example.com/jps/>");
		List<String> triples = new ArrayList<String>();
		triples.add("<" + target + "> example:admsmodel example:model1.apl .");
		triples.add("<" + target + "> example:inputparam <http://example.com/ontokin/reactmeachxyz.owl> .");
		
		return MetaDataAnnotator.getSparqlInsert(target, mediaType, creationTime, agent, true, topics, prefixes, triples);
	}
	
	public void testSparqlInsert() {		
		String sparql = getSparqlInsertExample("http://example.com/jps/some/path/output.csv", "http://example.com/scenario/xyz", null);
		System.out.println(sparql);
		assertEquals(17, sparql.split("\n").length);
	}
	
	public void testAnnotate() {
		String sparql = getSparqlInsertExample("http://example.com/jps/some/path/output.csv", "http://example.com/scenario/xyz", null);
		System.out.println(sparql);
		MetaDataAnnotator.annotate(sparql);
		sparql = "SELECT ?s ?p ?o WHERE {?s ?p ?o } LIMIT 100";
		String result = MetaDataQuery.query(sparql);
		System.out.println(result);
		assertEquals(11, getNumberOfResultsWithoutHeader(result));
	}
	
	public void testSparqlQueryResources() {
		String sparql = getSparqlInsertExample("http://example.com/jps/some/path/output1.csv", "http://example.com/scenario/xyz1", "2019-09-18T11:26:26.419");
		MetaDataAnnotator.annotate(sparql);
		sparql = getSparqlInsertExample("http://example.com/jps/some/path/output2.csv", "http://example.com/scenario/xyz1", "2019-09-19T11:26:26.419");
		MetaDataAnnotator.annotate(sparql);
		sparql = getSparqlInsertExample("http://example.com/jps/some/path/output3.csv", "http://example.com/scenario/xyz2", "2019-09-19T11:26:29");
		MetaDataAnnotator.annotate(sparql);
		
		sparql = "SELECT ?s ?p ?o WHERE {?s ?p ?o } LIMIT 100";
		String result = MetaDataQuery.query(sparql);
		System.out.println(result);
		// 2 triples for agent + 9 triples * 3 resources
		assertEquals(29, getNumberOfResultsWithoutHeader(result));
		
		sparql = MetaDataQuery.getSparqlQueryResources(null, "2019-09-18T11:26:26", "2019-09-19T11:26:28", null, null, null, null, null);
		System.out.println(sparql);
		result = MetaDataQuery.query(sparql);
		System.out.println(result);
		assertEquals(2, getNumberOfResultsWithoutHeader(result));
		
		sparql = MetaDataQuery.getSparqlQueryResources(MediaType.TEXT_CSV, null, null, null, "2019-09-18T11:26:26", "2019-09-19T11:26:29", null, null);
		System.out.println(sparql);
		result = MetaDataQuery.query(sparql);
		System.out.println(result);
		assertEquals(3, getNumberOfResultsWithoutHeader(result));
		
		List<String> topics = new ArrayList<String>();
		topics.add("http://dbpedia.org/resource/Singapore");
		sparql = MetaDataQuery.getSparqlQueryResources(null, null, null, "http://example.com/jps/agents/myfancyagent", 
				"2019-09-18T11:26:27", "2019-09-19T11:27:00", "http://example.com/scenario/xyz2", topics);
		System.out.println(sparql);
		result = MetaDataQuery.query(sparql);
		System.out.println(result);
		assertEquals(1, getNumberOfResultsWithoutHeader(result));
	}
	
	public void testQueryResourcesWithTime() {
		
		String target = "http://example.com/resource";
		String agent = "http://example.com/kb/agents/Service__ADMS.owl#Service";
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "1", "2015-02-05T09:00:00", null);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "2", "2015-02-05T10:11:05", null);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "3", "2015-02-05T11:00:00", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "4", "2015-02-06T18:12:31", null);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "5", "2015-02-07T22:00:00", null);
		
		String result = MetaDataQuery.queryResources(null, "2015-02-05T10:11:05", "2015-02-06T18:12:31");
		System.out.println(result);
		
		assertEquals(3, getNumberOfResultsWithoutHeader(result));
	}
	
	public void testQueryResourcesWithTimeAndAgent() {
		
		String target = "http://example.com/resource";
		String agent = "http://example.com/kb/agents/Service__ADMS.owl#Service";
		String agentB = "http:/example.com/kb/agents/AgentB.owl#Service";
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "1", "2015-02-05T09:00:00", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "2", "2015-02-05T10:11:05", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "2B", "2015-02-05T10:11:05", agentB);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "3", "2015-02-05T11:00:00", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "3B", "2015-02-05T11:00:00", agentB);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "4", "2015-02-06T18:12:31", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "4B", "2015-02-06T18:12:31", agentB);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "5", "2015-02-07T22:00:00", agent);
		
		String result = MetaDataQuery.queryResources(agent, "2015-02-05T10:11:05", "2015-02-06T18:12:31");
		System.out.println(result);
		
		assertEquals(3, getNumberOfResultsWithoutHeader(result));
	}
	
	public void testAnnotateWithJpsContextAndSimulationTime() {

		MediaType mediaType = MediaType.TEXT_CSV;
		List<String> topicsBerlin = new ArrayList<String>();
		topicsBerlin.add("http://dbpedia.org/resource/Air_pollution");
		topicsBerlin.add("http://dbpedia.org/resource/Berlin");
		
		List<String> topicsSingapore = new ArrayList<String>();
		topicsSingapore.add("http://dbpedia.org/resource/Air_pollution");
		topicsSingapore.add("http://dbpedia.org/resource/Singapore");
		
		String admsAgent = "http://example.com/jps/agents/admsagent";
		String srmAgent = "http://example.com/jps/agents/srmagent";
		
		// first scenario for Berlin
		String scenarioNameBerlin = "http://example.com/scenario/admstestberlin";
		JPSContext.putScenarioUrl(scenarioNameBerlin);
		
		// first scenario, first simulation
		JPSContext.putSimulationTime("2012-02-02T02:00:00");
		MetaDataAnnotator.annotate("http://example.com/jps/222/adms/outputadms.gst", mediaType, admsAgent, true, topicsBerlin);
		MetaDataAnnotator.annotate("http://example.com/jps/222/data/srmdata.csv", mediaType, srmAgent, true, topicsBerlin);
		
		// first scenario, second simulation
		JPSContext.putSimulationTime("2012-02-02T03:00:00");
		MetaDataAnnotator.annotate("http://example.com/jps/333/adms/outputadms.gst", mediaType, admsAgent, true, topicsBerlin);
		MetaDataAnnotator.annotate("http://example.com/jps/333/data/srmdata.csv", mediaType, srmAgent, true, topicsBerlin);

		// second scenario for Singapore
		String scenarioNameSingapore = "http://example.com/scenario/admstestsingapore";
		JPSContext.putScenarioUrl(scenarioNameSingapore);
		
		// second scenario, first simulation
		JPSContext.putSimulationTime("2012-02-02T02:00:00");
		MetaDataAnnotator.annotate("http://example.com/jps/222xxx/adms/outputadms.gst", mediaType, admsAgent, true, topicsSingapore);
		MetaDataAnnotator.annotate("http://example.com/jps/222xxx/data/srmdata.csv", mediaType, srmAgent, true, topicsSingapore);
		
		// check: 4 resources for scenario Berlin and 2 resources for scenario Singapore
		String result = MetaDataQuery.queryResources(null, null, null, null, null, null, scenarioNameBerlin, null);
		assertEquals(4, getNumberOfResultsWithoutHeader(result));
		result = MetaDataQuery.queryResources(null, null, null, null, null, null, scenarioNameSingapore, null);
		assertEquals(2, getNumberOfResultsWithoutHeader(result));
		
		// check: 2 resources for scenario Berlin and adms agent
		result = MetaDataQuery.queryResources(null, null, null, admsAgent, null, null, scenarioNameBerlin, null);
		assertEquals(2, getNumberOfResultsWithoutHeader(result));
		
		// check: 2 resources for scenario Berlin between simulation 2:00am and 2:59am
		result = MetaDataQuery.queryResources(null, null, null, null, "2012-02-02T02:00:00", "2012-02-02T02:59:00", scenarioNameBerlin, null);
		assertEquals(2, getNumberOfResultsWithoutHeader(result));
		
		// check: 2 resources for adms agent between simulation time 1:00am and 2:00am
		result = MetaDataQuery.queryResources(null, null, null, admsAgent, "2012-02-02T01:00:00", "2012-02-02T02:00:00", null, null);
		assertEquals(2, getNumberOfResultsWithoutHeader(result));
	}
}
