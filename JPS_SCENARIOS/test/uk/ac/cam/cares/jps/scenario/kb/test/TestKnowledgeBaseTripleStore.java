package uk.ac.cam.cares.jps.scenario.kb.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.rio.RDFParseException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

public class TestKnowledgeBaseTripleStore extends TestKnowledgeBaseAllImplementations {
	
	public void setUp() {
		setUpRdf4jInMemoryRemote();
		//setUpBlazegraphDirect();
		printTime(null);
	}
	
	protected int countBuildings() {
		int count = queryCount(null, SPARQL_COUNT_BUILDINGS);
		System.out.println("count buildings=" + count);
		return count;
	}
	
	public void testPutOneFileAsNamedGraphAndOneFileAsUnnamedGraphAndQuery() throws RDFParseException, RepositoryException, IOException {
		
		File fileNamedGraph = getBuildingFiles(5000, 5001)[0];
		File defaultGraph = getBuildingFiles(5001, 5002)[0];
		
		String namedGraph = datasetUrl + "/" + fileNamedGraph.getName().replace(".owl", "");
		String content = FileUtil.readFileLocally(fileNamedGraph.getAbsolutePath());
		client().put(namedGraph, content, MediaType.APPLICATION_RDF_XML.type);
		
		content = FileUtil.readFileLocally(defaultGraph.getAbsolutePath());
		client().put(null, content, MediaType.APPLICATION_RDF_XML.type);
		
		int numberOfBuildings = countBuildings();
		System.out.println("number of buildings = " + numberOfBuildings);
		
		String sparql = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
				+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n"
				+ "SELECT (COUNT(?g) as ?count) \n"
				+ "WHERE { GRAPH ?g { ?bdn a citygml:BuildingType . } }";
		
		int numberOfBuildingsInNamedGraphs = queryCount(null, sparql);
		System.out.println("number of buildings in named graphs = " + numberOfBuildingsInNamedGraphs);
	
		assertEquals(numberOfBuildingsInNamedGraphs, numberOfBuildings - 1);
	}
	
	public void testPut10FilesAsNamedGraphsAndQuery() throws RDFParseException, RepositoryException, IOException {
		
		int expectedNumberOfBuildings = 10;
		int from = 0;
		int to = from + expectedNumberOfBuildings;
		File[] files = getBuildingFiles(from, to);
				
		int countTriples = 0;
		int namedGraphCounter = 0;
		for (File current : files) {
			namedGraphCounter++;
			
			String namedGraph = datasetUrl + "/" + current.getName().replace(".owl", "");
			System.out.println(namedGraphCounter + " " + namedGraph);
			
			String body = FileUtil.readFileLocally(current.getAbsolutePath());
			client().put(namedGraph, body, MediaType.APPLICATION_RDF_XML.type);
			countTriples = countAllTriples();
		}
		
		int numberOfBuildings = countBuildings();
		System.out.println("number of buildings = " + numberOfBuildings);
		assertEquals(expectedNumberOfBuildings, numberOfBuildings);
	}
	
	/**
	 * 20191016 laptop Andreas: <br>
	 * (1) setUpFileBasedRemote(), all building files (11444)<br>
	 * around 12 min<br>
	 * <br>
	 * (2) setUpRdf4jNativeDirect, all building files (11444)<br>
	 * around 54 min<br>
	 * 00001 - 00100: ~ 19 sec<br>
	 * 00001 - 05000: 18 min<br>
	 * 05000 - 05100: ~ 25 sec<br>
	 * 05000 - 10000: 29 min<br>
	 * 09000 - 09100: ~ 85 sec<br>
	 * 10000 - 10100: ~ 32 sec<br>
	 * 
	 * @throws RDFParseException
	 * @throws RepositoryException
	 * @throws IOException<br>
	 * <br>
	 * (3) setUpBlazegraphDirect(), all building files (11444)<br>
	 * 2019-10-27 06:59:01
	 */
	public void xxxtestPerformancePutxFilesAsNamedGraphs() throws RDFParseException, RepositoryException, IOException {
		
		printTime(null);
		
		int numberOfBuildings = 100000;
		int from = 0;
		int to = from + numberOfBuildings;
		File[] files = getBuildingFiles(from, to);
				
		int namedGraphCounter = 0;
		for (File current : files) {
			namedGraphCounter++;
			
			String namedGraph = datasetUrl + "/" + current.getName().replace(".owl", "");
			System.out.println(namedGraphCounter + " " + namedGraph);
			
			String body = FileUtil.readFileLocally(current.getAbsolutePath());
			client().put(namedGraph, body, MediaType.APPLICATION_RDF_XML.type);
		}
		
		printTime("testPerformancePutxFilesAsNamedGraphs");
	}
	
	public void xxxtestPerformanceGetxFilesAsNamedGraphs() throws RDFParseException, RepositoryException, IOException {
		
		printTime(null);
		
//		int numberOfBuildings = 100;
//		int from = 0;
//		int to = from + numberOfBuildings;
//		File[] files = getBuildingFiles(from, to);
		
		
		File[] files = getBuildingFiles(0, 11000);
		System.out.println("number of all buildings = " + files.length);
		
		List<File> subsetFiles = new ArrayList<File>();
		for (int i=0; i<10; i++) {
			 File[] sub = Arrays.copyOfRange(files, i * 100, i * 100 + 5);
			 for (File current : sub) {
				 subsetFiles.add(current);
			 }
		}
		
		System.out.println("number of buildings in subset = " + subsetFiles.size());
				
		int namedGraphCounter = 0;
		for (File current : subsetFiles) {
			namedGraphCounter++;
			
			String namedGraph = datasetUrl + "/" + current.getName().replace(".owl", "");
			System.out.println(namedGraphCounter + " " + namedGraph);
			
			client().get(namedGraph, MediaType.APPLICATION_RDF_XML.type);
		}
		
		printTime("testPerformanceGetxFilesAsNamedGraphs");
	}
	
	/**
	 * 20191016 laptop Andreas: <br>
	 * (1) setUpFileBasedRemote(), 1000 buildings<br>
	 * around 35 sec (query with target resource but without graph in SPARQL query)<br>
	 * <br>
	 * (2) setUpRdf4jNativeDirect, 1000 buildings<br>
	 * around 17 sec (query without target resource but with graph in SPARQL query)<br>
	 * 
	 * @throws RDFParseException
	 * @throws RepositoryException
	 * @throws IOException
	 */
	public void xxxtestPerformanceQueryxFilesAsNamedGraphs() throws RDFParseException, RepositoryException, IOException {
		
		printTime(null);
		
//		int numberOfBuildings = 100;
//		int from = 0;
//		int to = from + numberOfBuildings;
//		File[] files = getBuildingFiles(from, to);
		
		
		File[] files = getBuildingFiles(0, 11000);
		System.out.println("number of all buildings = " + files.length);
		
		List<File> subsetFiles = new ArrayList<File>();
		for (int i=0; i<10; i++) {
			 File[] sub = Arrays.copyOfRange(files, i * 100, i * 100 + 100);
			 for (File current : sub) {
				 subsetFiles.add(current);
			 }
		}
		
		System.out.println("number of buildings in subset = " + subsetFiles.size());
				
		String sparqlTemplate = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
				+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n"
				+ "SELECT ?g ?bdn \n"
				+ "FROM NAMED <%s> \n"
				+ "WHERE { Graph ?g { ?bdn a citygml:BuildingType . } } \n";
		
//		String sparqlTemplate = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
//				+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n"
//				+ "SELECT ?g ?bdn \n"
//				+ "WHERE { ?bdn a citygml:BuildingType . }  \n"
//				+ "LIMIT 100";
		
		System.out.println(sparqlTemplate);
		
		int namedGraphCounter = 0;
		for (File current : subsetFiles) {
			namedGraphCounter++;
			
			String namedGraph = datasetUrl + "/" + current.getName().replace(".owl", "");
			System.out.println(namedGraphCounter + " " + namedGraph);
			
			String sparql = MiscUtil.format(sparqlTemplate, namedGraph);
			System.out.println(sparql);
			
			String result = client().query(null, sparql);
			//String result = client().query(namedGraph, sparql);
			JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
			String graph = simplified.getJSONArray("results").getJSONObject(0).getString("g");
			//String graph = simplified.getJSONArray("results").getJSONObject(0).getString("bdn");
			System.out.println("result = " + graph);
		}
		
		printTime("testPerformanceQueryCountxFilesAsNamedGraphs");
	}
	
	public void xxxtestPerformanceQueryCountxFilesAsNamedGraphs() throws RDFParseException, RepositoryException, IOException {
		
		printTime(null);
		
//		int numberOfBuildings = 100;
//		int from = 0;
//		int to = from + numberOfBuildings;
//		File[] files = getBuildingFiles(from, to);
		
		
		File[] files = getBuildingFiles(0, 11000);
		System.out.println("number of all buildings = " + files.length);
		
		List<File> subsetFiles = new ArrayList<File>();
		for (int i=0; i<10; i++) {
			 File[] sub = Arrays.copyOfRange(files, i * 100, i * 100 + 10);
			 for (File current : sub) {
				 subsetFiles.add(current);
			 }
		}
		
		System.out.println("number of buildings in subset = " + subsetFiles.size());
				
		String sparqlTemplate = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
				+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n"
				+ "SELECT (count(?s) as ?count) \n"
				+ "FROM NAMED <%s> \n"
				+ "WHERE { Graph ?g { ?s ?p ?o . } } \n";
		
		System.out.println(sparqlTemplate);
		
		int namedGraphCounter = 0;
		int totalCount = 0;
		for (File current : subsetFiles) {
			namedGraphCounter++;
			
			String namedGraph = datasetUrl + "/" + current.getName().replace(".owl", "");
			System.out.println(namedGraphCounter + " " + namedGraph);
			
			String sparql = MiscUtil.format(sparqlTemplate, namedGraph);
			System.out.println(sparql);
			
			String result = client().query(null, sparql);
			//String result = client().query(namedGraph, sparql);
			JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
			String countAsString = simplified.getJSONArray("results").getJSONObject(0).getString("count");
			int count = Integer.valueOf(countAsString);
			totalCount += count;
			System.out.println("count = " + count + ", total = " + totalCount);
		}
		
		printTime("testPerformanceQueryCountxFilesAsNamedGraphs");
	}
}
