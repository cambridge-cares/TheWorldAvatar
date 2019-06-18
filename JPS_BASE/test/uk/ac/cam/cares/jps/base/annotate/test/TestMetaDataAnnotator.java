package uk.ac.cam.cares.jps.base.annotate.test;

import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
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
	
	private void deleteAllTestAnnotations() {
		String sparqlDeleteTime = "PREFIX dcterms:<" + PrefixToUrlMap.getPrefixUrl(DCTERMS) + "> \r\n" +
				"PREFIX time:<" + PrefixToUrlMap.getPrefixUrl(TIME) + "> \r\n" +
				"PREFIX xsd:<" + PrefixToUrlMap.getPrefixUrl(XSD) + "> \r\n" +
				"DELETE { ?resource time:hasTime ?t . ?t time:inXSDDateTime ?time . ?t a time:Instant . } \r\n" + 
				"WHERE { ?resource time:hasTime ?t . ?t time:inXSDDateTime ?time . FILTER ( CONTAINS(STR(?resource), \"www.justfortesting.com\") ) . }"; 
		
		// before you want to perform the next delete all sparql query, know what you do and make sure you are deleting the right data set
		// in particular, you have to understand that you are deleting data on claudius !!!
//		sparql = "PREFIX time:<https://www.w3.org/2006/time#> \r\n" + 
//				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> \r\n" + 
//				"DELETE { ?s ?p ?o . } \r\n" +
//				"WHERE { ?s ?p ?o . }";
		
		
		MetaDataAnnotator.getSparqlService().executePost(sparqlDeleteTime);
		
		String sparqlDeleteAgent = "PREFIX dcterms:<" + PrefixToUrlMap.getPrefixUrl(DCTERMS) + "> \r\n" +
				"PREFIX time:<" + PrefixToUrlMap.getPrefixUrl(TIME) + "> \r\n" +
				"PREFIX xsd:<" + PrefixToUrlMap.getPrefixUrl(XSD) + "> \r\n" +
				"DELETE { ?resource dcterms:creator ?agent . ?agent a dcterms:Agent . } \r\n" + 
				"WHERE { ?resource dcterms:creator ?agent .  FILTER ( CONTAINS(STR(?resource), \"www.justfortesting.com\") ) . }"; 
		
		MetaDataAnnotator.getSparqlService().executePost(sparqlDeleteAgent);
	}

	public void testXsdTimeStampFormat() {	
		long millis = 1559633714596l;
		String timestamp = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis);
		assertEquals("2019-06-04T07:35:14.596", timestamp);
	}
	
	public void xxxxtestGetSparql() {
		String iri = "http://www.justfortesting.com/some/path/output.csv";
		//String sparql = MetaDataAnnotator.getSparqlInsertAnnotateWithTime(iri, "2019-06-06T09:11:00");
		String sparql = MetaDataAnnotator.getSparqlInsertAnnotateWithTimeAndAgent(iri, null, "blabla");
		System.out.println(sparql);
	}
	
	public void xxxxtestAnnotateWithCurrentTime() {
		long millis = System.currentTimeMillis();
		String target = "http://www.justfortesting.com/some/path/output" + millis + ".csv";
		MetaDataAnnotator.annotateWithCurrentTime(target);
	}
	
	public void xxxxtestAnnotateWithTimeAndAgent() {
		long millis = System.currentTimeMillis();
		String time = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis);
		String target = "http://www.justfortesting.com/some/path/output" + millis + ".csv";
		String agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
		MetaDataAnnotator.annotateWithTimeAndAgent(target, time, agent);
	}
	
	public void testQueryResourcesWithTime() {
		
		String target = "http://www.justfortesting.com/resource";
		String agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
		MetaDataAnnotator.annotateWithTime(target + "1", "2015-02-05T09:00:00");
		MetaDataAnnotator.annotateWithTime(target + "2", "2015-02-05T10:11:05");
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "3", "2015-02-05T11:00:00", agent);
		MetaDataAnnotator.annotateWithTime(target + "4", "2015-02-06T18:12:31");
		MetaDataAnnotator.annotateWithTime(target + "5", "2015-02-07T22:00:00");
		
		String result = MetaDataQuery.queryResources("2015-02-05T10:11:05", "2015-02-06T18:12:31", null);
		System.out.println(result);
		
		List<String[]> list = MatrixConverter.fromCsvToArray(result);
		// 1 header line plus 3 result lines
		assertEquals(4, list.size());
	}
	
	public void testQueryResourcesWithTimeAndAgent() {
		
		String target = "http://www.justfortesting.com/resource";
		String agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
		String agentB = "http://www.theworldavatar.com/kb/agents/AgentB.owl#Service";
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "1", "2015-02-05T09:00:00", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "2", "2015-02-05T10:11:05", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "2B", "2015-02-05T10:11:05", agentB);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "3", "2015-02-05T11:00:00", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "3B", "2015-02-05T11:00:00", agentB);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "4", "2015-02-06T18:12:31", agent);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "4B", "2015-02-06T18:12:31", agentB);
		MetaDataAnnotator.annotateWithTimeAndAgent(target + "5", "2015-02-07T22:00:00", agent);
		
		String result = MetaDataQuery.queryResources("2015-02-05T10:11:05", "2015-02-06T18:12:31", agent);
		System.out.println(result);
		
		List<String[]> list = MatrixConverter.fromCsvToArray(result);
		// 1 header line plus 3 result lines
		assertEquals(4, list.size());
	}
}
