package uk.ac.cam.cares.jps.scenario.kb.test;

import java.io.File;
import java.util.Arrays;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public abstract class TestKnowledgeBaseHelper extends TestCase {

	protected static final String SPARQL_COUNT_TRIPLES = "SELECT (COUNT(?s) as ?count) WHERE { ?s ?p ?o }";
	
	private KnowledgeBaseSwitchClient client = null;
	protected String datasetUrl = null;
	private long startTime = -1;
	
	protected void printTime(String s) {
		
		long endTime = System.currentTimeMillis();
		
		if (s == null) {
			startTime = endTime;
			return;
		}

		if (startTime > 0) {
			long diff = endTime - startTime;
			System.out.println(s + " diff = " + diff);
		}
		startTime = endTime;
	}
	
	public void createClient(String datasetUrl, boolean direct) {
		this.datasetUrl = datasetUrl;
		client = new KnowledgeBaseSwitchClient(datasetUrl, direct);
	}
	
	protected KnowledgeBaseSwitchClient client() {
		return client;
	}
	
	public File[] getBuildingFiles(int from, int to) {
		//File dir = new File("D:/myTemp/cityGML/cityGML/buildingsthehaguenamedgraphs");
		File dir = new File("C:/Users/Andreas/TMP/buildingsthehaguenamedgraphs");
		
		File[] files = dir.listFiles();
		return Arrays.copyOfRange(files, from, to);
	}
	
	public File getFile(int pos) {
		return getBuildingFiles(pos,pos)[0];
	}
	
	protected String getE303LoadUrl() {
		return datasetUrl + "/some/path/testE-303load.owl";
	}
	
	protected String putE303Load(String targetUrl) {
		return putE303Load(targetUrl, null);
	}
	
	protected String putE303Load(String targetUrl, String numbermarker) {
		String filePath = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/E-303load.owl";
		String body = FileUtil.readFileLocally(filePath);
		if (numbermarker != null) {
			body = body.replace("0.27", numbermarker);
		}
		client().put(targetUrl, body, MediaType.APPLICATION_RDF_XML.type);
		return body;
	}
	
	protected String putE303LoadRemoteKBCOnly(String datasetUrl, String targetUrl, String numbermarker) {
		String filePath = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/E-303load.owl";
		String body = FileUtil.readFileLocally(filePath);
		if (numbermarker != null) {
			body = body.replace("0.27", numbermarker);
		}
		AccessAgentCaller.put(datasetUrl, targetUrl, body, MediaType.APPLICATION_RDF_XML.type);
		return body;
	}
	
	protected void assertMarkerInE303Load(String content, String numbermarker) {
		String match = numbermarker + "</system:numericalValue>";
		assertTrue(content.contains(match));
	}
	
	protected int queryCount(String resourceUrl, String sparql) {

		String result = client().query(resourceUrl, sparql);
		//System.out.println(result);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		return simplified.getJSONArray("results").getJSONObject(0).getInt("count");
	}
	
	protected int countAllTriples() {
		int count = queryCount(null, SPARQL_COUNT_TRIPLES);
		System.out.println("count triples=" + count);
		return count;
	}
	
	protected int countTriples(String resourceUrl) {
		int count = queryCount(resourceUrl, SPARQL_COUNT_TRIPLES);
		System.out.println("count triples=" + count);
		return count;
	}
}
