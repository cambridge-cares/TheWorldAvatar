package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.sql.SQLException;

import org.apache.jena.update.UpdateRequest;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.tools.RenameTool;

public class RenameToolTest {

	private RenameTool renameTool;

	private String sparqlQuery = "SELECT ?s  ?p  ?o" +
			"WHERE\n" +  
			"  { ?s  ?p  ?o \n" + 
			"      BIND(<http://www.w3.org/2008/05/skos#replacement> AS ?targetURI)\n" +  
			"      FILTER ( ( ?s = ?targetURI ) || ( ?p = ?targetURI ) || ( ?o = ?targetURI ) )\n" + 
			"  }\n";
	
	private String expectedQueryResult = "[{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_4\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_9\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_10\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_2\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_7\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_5\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_3\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_8\"},"+
			"{\"p\":\"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\",\"s\":\"http://www.w3.org/2008/05/skos#replacement\"},"+
			"{\"p\":\"http://www.w3.org/2000/01/rdf-schema#subPropertyOf\",\"s\":\"http://www.w3.org/2008/05/skos#replacement\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_1\"},"+
			"{\"p\":\"http://www.w3.org/2008/05/skos#replacement\",\"s\":\"http://www.theworldavatar.com/kb/species/species.owl#species_6\"}]";
	
	// temporary folder for testing
	@Rule
	private TemporaryFolder tempFolder = new TemporaryFolder();
	
	//Test renameURI on owl file
	@Test
	public void testRenameURI() throws URISyntaxException, SQLException, ParseException, IOException {
		
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "http://www.w3.org/2008/05/skos#altLabel";
		String replacement = "http://www.w3.org/2008/05/skos#replacement";
		
		//copy species to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		KnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(tempFilePath.toString());
		
		//perform update
		RenameTool.renameURI(kbClient, target, replacement);
		
		//select query to check result
		String result = kbClient.execute(sparqlQuery);
		
		assertEquals(expectedQueryResult, result);
	}
	
	//Test renameURIFragment on owl file
	@Test
	public void testRenameURIFragment() throws URISyntaxException, SQLException, ParseException, IOException {
				
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "altLabel";
		String replacement = "replacement";
		
		//copy species to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		KnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(tempFilePath.toString());
		
		//perform update
		RenameTool.renameURIFragment(kbClient, target, replacement);
		
		//select query to check result
		String result = kbClient.execute(sparqlQuery);
		
		assertEquals(expectedQueryResult, result);
	}
	
	// Test sparql builder. Update to replace URI.
	@Test
	public void testBuildSparqlUpdateURI() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		String expected = "DELETE {\n" +
		"  GRAPH <http://example.com/test> {\n" +
		"    ?s ?p ?o .\n" +
		"  }\n" +
		"}\n" +
		"INSERT {\n" +
		"  GRAPH <http://example.com/test> {\n" +
		"    ?newS ?newP ?newO .\n" +
		"  }\n" +
		"}\n"+ 
		"WHERE\n" +
		"  { GRAPH <http://example.com/test>\n" +
		"      { ?s  ?p  ?o\n" +
		"        BIND(<http://example.com/test/target> AS ?targetURI)\n" +
		"        BIND(<http://example.com/test/replacement> AS ?replacementURI)\n" +
		"        FILTER ( ( ?s = ?targetURI ) || ( ( ?p = ?targetURI ) || ( ?o = ?targetURI ) ) )\n" +
		"        BIND(if(( ?s = ?targetURI ), ?replacementURI, ?s) AS ?newS)\n" +
		"        BIND(if(( ?p = ?targetURI ), ?replacementURI, ?p) AS ?newP)\n" +
		"        BIND(if(( ?o = ?targetURI ), ?replacementURI, ?o) AS ?newO)\n" +
		"      }\n" +
		"  }\n";
		
		// access private member
		renameTool = new RenameTool();
		assertNotNull(renameTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class, String.class, String.class));
		Method buildSparqlUpdateURI = renameTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class, String.class, String.class);
		buildSparqlUpdateURI.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateURI.invoke(renameTool, "http://example.com/test/target", "http://example.com/test/replacement", "http://example.com/test");
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
	
	// Test sparql builder. Update to replace URI in default graph.
	@Test
	public void testBuildSparqlUpdateURIDefaultGraph() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		String expected = "DELETE {\n" +
		"  ?s ?p ?o .\n" +
		"}\n" +
		"INSERT {\n" +
		"  ?newS ?newP ?newO .\n" +
		"}\n"+ 
		"WHERE\n" +
		"  { ?s  ?p  ?o\n" +
		"    BIND(<http://example.com/test/target> AS ?targetURI)\n" +
		"    BIND(<http://example.com/test/replacement> AS ?replacementURI)\n" +
		"    FILTER ( ( ?s = ?targetURI ) || ( ( ?p = ?targetURI ) || ( ?o = ?targetURI ) ) )\n" +
		"    BIND(if(( ?s = ?targetURI ), ?replacementURI, ?s) AS ?newS)\n" +
		"    BIND(if(( ?p = ?targetURI ), ?replacementURI, ?p) AS ?newP)\n" +
		"    BIND(if(( ?o = ?targetURI ), ?replacementURI, ?o) AS ?newO)\n" +
		"  }\n";
		
		// access private member
		renameTool = new RenameTool();
		assertNotNull(renameTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class, String.class, String.class));
		Method buildSparqlUpdateURI = renameTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class, String.class, String.class);
		buildSparqlUpdateURI.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateURI.invoke(renameTool, "http://example.com/test/target", "http://example.com/test/replacement", null);
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
	
	// Test sparql builder. Update to replace URI fragment.
	@Test
	public void testBuildSparqlUpdateString() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		String expected = "DELETE {\n" +
		"  GRAPH <http://example.com/test> {\n" +
		"    ?s ?p ?o .\n" +
		"  }\n" +
		"}\n" +
		"INSERT {\n" +
		"  GRAPH <http://example.com/test> {\n" +
		"    ?newS ?newP ?newO .\n" +
		"  }\n" +
		"}\n"+ 
		"WHERE\n" +
		"  { GRAPH <http://example.com/test>\n" +
		"      { ?s  ?p  ?o\n" +
		"        BIND(regex(str(?s), \"/test/target\") AS ?matchS)\n" +
		"        BIND(regex(str(?p), \"/test/target\") AS ?matchP)\n" +
		"        BIND(regex(str(?o), \"/test/target\") AS ?matchO)\n" +
		"        FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n" +
		"        BIND(if(?matchS, uri(replace(str(?s), \"/test/target\", \"/test/replacement\")), ?s) AS ?newS)\n" +
		"        BIND(if(?matchP, uri(replace(str(?p), \"/test/target\", \"/test/replacement\")), ?p) AS ?newP)\n" +
		"        BIND(if(?matchO, uri(replace(str(?o), \"/test/target\", \"/test/replacement\")), ?o) AS ?newO)\n" +
		"      }\n" +
		"  }\n";
		
		// access private member
		renameTool = new RenameTool();
		assertNotNull(renameTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class, String.class, String.class));
		Method buildSparqlUpdateString = renameTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class, String.class, String.class);
		buildSparqlUpdateString.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateString.invoke(renameTool, "/test/target", "/test/replacement", "http://example.com/test");
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
	
	// Test sparql builder. Update to replace URI fragment.
	@Test
	public void testBuildSparqlUpdateStringDefaultGraph() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		String expected = "DELETE {\n" +
		"  ?s ?p ?o .\n" +
		"}\n" +
		"INSERT {\n" +
		"  ?newS ?newP ?newO .\n" +
		"}\n"+ 
		"WHERE\n" +
		"  { ?s  ?p  ?o\n" +
		"    BIND(regex(str(?s), \"/test/target\") AS ?matchS)\n" +
		"    BIND(regex(str(?p), \"/test/target\") AS ?matchP)\n" +
		"    BIND(regex(str(?o), \"/test/target\") AS ?matchO)\n" +
		"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n" +
		"    BIND(if(?matchS, uri(replace(str(?s), \"/test/target\", \"/test/replacement\")), ?s) AS ?newS)\n" +
		"    BIND(if(?matchP, uri(replace(str(?p), \"/test/target\", \"/test/replacement\")), ?p) AS ?newP)\n" +
		"    BIND(if(?matchO, uri(replace(str(?o), \"/test/target\", \"/test/replacement\")), ?o) AS ?newO)\n" +
		"  }\n";
		
		// access private member
		renameTool = new RenameTool();
		assertNotNull(renameTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class, String.class, String.class));
		Method buildSparqlUpdateString = renameTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class, String.class, String.class);
		buildSparqlUpdateString.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateString.invoke(renameTool, "/test/target", "/test/replacement", null);
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
}