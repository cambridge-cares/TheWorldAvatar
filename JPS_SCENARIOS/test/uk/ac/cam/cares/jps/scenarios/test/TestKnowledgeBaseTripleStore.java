package uk.ac.cam.cares.jps.scenarios.test;

import java.io.File;
import java.io.IOException;

import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.rio.RDFParseException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class TestKnowledgeBaseTripleStore extends TestKnowledgeBaseAllImplementations {
	
	private int queryCount(String sparql) {

		String result = client().query(null, sparql);
		//System.out.println(result);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		return simplified.getJSONArray("results").getJSONObject(0).getInt("count");
	}
	
	protected int countTriples() {
		int count = queryCount(SPARQL_COUNT_TRIPLES);
		System.out.println("count triples=" + count);
		return count;
	}
	
	protected int countBuildings() {
		int count = queryCount(SPARQL_COUNT_BUILDINGS);
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
		
		int numberOfBuildingsInNamedGraphs = queryCount(sparql);
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
			countTriples = countTriples();
		}
		
		int numberOfBuildings = countBuildings();
		System.out.println("number of buildings = " + numberOfBuildings);
		assertEquals(expectedNumberOfBuildings, numberOfBuildings);
	}
}
