package uk.ac.cam.cares.jps.base.annotate.test;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.discovery.MediaType;

public class MetaDataAnnotatorTest {

	@Test 
    public void testSparqlOverHttpService() {
		Assert.assertNull(MetaDataAnnotator.getSparqlService());
	}
    
	@Test
	public void testGetSparqlInsertFull() {
		String ExpectedResult = "PREFIX dcterms:<http://purl.org/dc/terms/>\r\nPREFIX xsd:<http://www.w3.org/2001/XMLSchema#>"
				+ "\r\nprefix1 \r\nprefix2 \r\nINSERT DATA { GRAPH <testScenario> { \r\n<testTarget> dcterms:format \"text/csv\" . \r\n<testTarget> "
				+ "dcterms:created \"testCreationTime\"^^xsd:dateTime . \r\n<testAgent> a dcterms:Agent . \r\n<testAgent> a "
				+ "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service> . \r\n<testTarget> "
				+ "dcterms:creator <testAgent> . \r\n<testTarget> dcterms:date \"testSimulationTime\"^^xsd:dateTime . \r\n<testTarget> "
				+ "dcterms:isPartOf <testScenario> . \r\n<testTarget> dcterms:subject <topic1> . \r\n<testTarget> "
				+ "dcterms:subject <topic2> . \r\ntriple1 \r\ntriple2 \r\n} } \r\n";

        String Result = MetaDataAnnotator.getSparqlInsertFull("testTarget", MediaType.TEXT_CSV, "testCreationTime", "testAgent",
    				true, "testSimulationTime", "testScenario", Arrays.asList("topic1","topic2"), Arrays.asList("prefix1","prefix2"), Arrays.asList("triple1","triple2"));

        Assert.assertEquals(Result, ExpectedResult);
	}
	
	long timeInMillis = 1628150245696L;
	String timeInXsdStampFormat = "2021-08-05T07:57:25.696";
	
	@Test
	public void testGetTimeInXsdTimeStampFormat() {
		Assert.assertEquals(MetaDataAnnotator.getTimeInXsdTimeStampFormat(timeInMillis), timeInXsdStampFormat);
	}

	@Test
	public void testGetMillisFromXsdTimeStampFormat() {
		Assert.assertEquals(MetaDataAnnotator.getMillisFromXsdTimeStampFormat(timeInXsdStampFormat), timeInMillis);
	}
	
}
