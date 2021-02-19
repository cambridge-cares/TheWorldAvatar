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
import java.util.Iterator;

import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.tools.RenamingTool;

/**
 * JUnit tests for RenameTool
 * 
 * @author Casper Lindberg
 *
 */
public class RenamingToolTest {

	private RenamingTool renamingTool;
	
	private String sparqlQueryP = "SELECT ?s  ?p  ?o" +
			"WHERE\n" +  
			"  { ?s  ?p  ?o \n" + 
			"      BIND(<http://www.w3.org/2008/05/skos#replacement> AS ?targetURI)\n" +  
			"      FILTER ( ?p = ?targetURI )\n" + 
			"  }\n";
	
	private String sparqlQueryS = "SELECT ?s  ?p  ?o" +
			"WHERE\n" +  
			"  { ?s  ?p  ?o \n" + 
			"      BIND(<http://www.w3.org/2008/05/skos#replacement> AS ?targetURI)\n" +  
			"      FILTER ( ?s = ?targetURI )\n" + 
			"  }\n";
	
	// temporary folder for testing
	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	
	
	
	/**
	 * Test renameURI on an owl file. 
	 * 
	 * @throws URISyntaxException
	 * @throws ParseException
	 * @throws IOException
	 */
	@Test
	public void testRenameURI() throws URISyntaxException, ParseException, IOException {
		
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "http://www.w3.org/2008/05/skos#altLabel";
		String replacement = "http://www.w3.org/2008/05/skos#replacement";
		
		//copy species.owl to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		KnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(tempFilePath.toString());
		
		//perform update
		renamingTool = new RenamingTool(target, replacement);
		renamingTool.renameURI(kbClient);
		
		//select query to check result: predicate renamed
		JSONArray resultP = kbClient.executeQuery(sparqlQueryP);
		
		//check result
		Iterator<Object> iteratorP = resultP.iterator();
		while(iteratorP.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorP.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("p").toString());
		}
		
		//select query to check result: subject renamed
		JSONArray resultS = kbClient.executeQuery(sparqlQueryS);
		
		//check result
		Iterator<Object> iteratorS = resultS.iterator();
		while(iteratorS.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorS.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("s").toString());
		}
	}
	
	/**
	 * Test renameURIFragment on an owl file. 
	 * 
	 * @throws URISyntaxException
	 * @throws ParseException
	 * @throws IOException
	 */
	@Test
	public void testRenameURIFragment() throws URISyntaxException, ParseException, IOException {
				
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "altLabel";
		String replacement = "replacement";
		
		//copy species to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		KnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(tempFilePath.toString());
		
		//perform update
		renamingTool = new RenamingTool(target, replacement);
		renamingTool.renameString(kbClient);
		
		//select query to check result: predicate renamed
		JSONArray resultP = kbClient.executeQuery(sparqlQueryP);
		
		//check result
		Iterator<Object> iteratorP = resultP.iterator();
		while(iteratorP.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorP.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("p").toString());
		}
		
		//select query to check result: subject renamed
		JSONArray resultS = kbClient.executeQuery(sparqlQueryS);
		
		//check result
		Iterator<Object> iteratorS = resultS.iterator();
		while(iteratorS.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorS.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("s").toString());
		}
	}
	
	/**
	 * Test sparql update builder. Build sparql update to replace URI in named graph.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
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
		"        BIND(if(isBlank(?s), ?s, if(( ?s = ?targetURI ), ?replacementURI, ?s)) AS ?newS)\n" +
        "        BIND(if(isBlank(?p), ?p, if(( ?p = ?targetURI ), ?replacementURI, ?p)) AS ?newP)\n" +
        "        BIND(if(isBlank(?o), ?o, if(( ?o = ?targetURI ), ?replacementURI, ?o)) AS ?newO)\n" +
		"      }\n" +
		"  }\n";
		
		// access private method
		renamingTool = new RenamingTool("http://example.com/test/target", "http://example.com/test/replacement");
		assertNotNull(renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class));
		Method buildSparqlUpdateURI = renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class);
		buildSparqlUpdateURI.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateURI.invoke(renamingTool, "http://example.com/test");
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
	
	/**
	 * Test sparql update builder. Build sparql update to replace URI in default graph.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
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
	    "    BIND(if(isBlank(?s), ?s, if(( ?s = ?targetURI ), ?replacementURI, ?s)) AS ?newS)\n" +
	    "    BIND(if(isBlank(?p), ?p, if(( ?p = ?targetURI ), ?replacementURI, ?p)) AS ?newP)\n" +
	    "    BIND(if(isBlank(?o), ?o, if(( ?o = ?targetURI ), ?replacementURI, ?o)) AS ?newO)\n" +
		"  }\n";
		
		// access private method
		renamingTool = new RenamingTool("http://example.com/test/target", "http://example.com/test/replacement");
		assertNotNull(renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class));
		Method buildSparqlUpdateURI = renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateURI", String.class);
		buildSparqlUpdateURI.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateURI.invoke(renamingTool, new Object[]{ null });
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
	
	/**
	 * Test sparql update builder. Builds sparql update to replace URI fragment in named graph.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
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
		"        FILTER strstarts(str(?s), \"http://www.theworldavatar.com/\")\n" +
		"        BIND(regex(str(?s), \"/test/match\") AS ?matchS)\n" +
		"        BIND(regex(str(?p), \"/test/match\") AS ?matchP)\n" +
		"        BIND(regex(str(?o), \"/test/match\") AS ?matchO)\n" +
		"        FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n" +
		"        BIND(if(isBlank(?s), ?s, if(?matchS, uri(replace(str(?s), \"/test/target\", \"/test/replacement\")), ?s)) AS ?newS)\n" +
		"        BIND(if(isBlank(?p), ?p, if(?matchP, uri(replace(str(?p), \"/test/target\", \"/test/replacement\")), ?p)) AS ?newP)\n" +
		"        BIND(if(isBlank(?o), ?o, if(?matchO, uri(replace(str(?o), \"/test/target\", \"/test/replacement\")), ?o)) AS ?newO)\n" +
		"      }\n" +
		"  }\n";
		
		// access private method
		renamingTool = new RenamingTool("/test/match", "/test/target", "/test/replacement");
		renamingTool.setFilter(getFilterExpression());
		assertNotNull(renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class));
		Method buildSparqlUpdateString = renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class);
		buildSparqlUpdateString.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateString.invoke(renamingTool,  "http://example.com/test");
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
	
	/**
	 * Return filter expression for unit test
	 */
	private Expr getFilterExpression() {
		
		ExprFactory exprFactory = new ExprFactory();
		ExprVar exprOldS = RenamingTool.exprS;
		
		return exprFactory.strstarts(exprFactory.str(exprOldS), exprFactory.asExpr("http://www.theworldavatar.com/"));
	}
	
	/**
	 * Test sparql update builder. Builds sparql update to replace URI fragment in default graph.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
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
		"    BIND(regex(str(?s), \"/test/match\") AS ?matchS)\n" +
		"    BIND(regex(str(?p), \"/test/match\") AS ?matchP)\n" +
		"    BIND(regex(str(?o), \"/test/match\") AS ?matchO)\n" +
		"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n" +
		"    BIND(if(isBlank(?s), ?s, if(?matchS, uri(replace(str(?s), \"/test/target\", \"/test/replacement\")), ?s)) AS ?newS)\n" +
		"    BIND(if(isBlank(?p), ?p, if(?matchP, uri(replace(str(?p), \"/test/target\", \"/test/replacement\")), ?p)) AS ?newP)\n" +
		"    BIND(if(isBlank(?o), ?o, if(?matchO, uri(replace(str(?o), \"/test/target\", \"/test/replacement\")), ?o)) AS ?newO)\n" +
		"  }\n";
		
		// access private method
		renamingTool = new RenamingTool("/test/match", "/test/target", "/test/replacement");
		assertNotNull(renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class));
		Method buildSparqlUpdateString = renamingTool.getClass().getDeclaredMethod("buildSparqlUpdateString", String.class);
		buildSparqlUpdateString.setAccessible(true);
		
		UpdateRequest sparql = (UpdateRequest) buildSparqlUpdateString.invoke(renamingTool, new Object[]{ null });
		
		String strSparql = sparql.toString();
		
		assertEquals(expected, strSparql);
	}
}