package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Paths;
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

	// temporary folder for testing
	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	@Test
	public void testRenameURI() {
		
		fail("Test not created");
	}
	
	@Test
	public void testRenameURIFragment() throws URISyntaxException, SQLException, ParseException {
		
		fail("Test not created");
		
		String testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI()).toFile().getPath();
		String tempFolderPath = tempFolder.getRoot().toString();
		
		String target = "/test/target";
		String replacement = "/test/replacement";
			
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient();
		//TODO setup filebasedkbclient
		
		RenameTool.renameURIFragment(kbClient, target, replacement);
		
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