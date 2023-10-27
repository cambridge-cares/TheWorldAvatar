package uk.ac.cam.cares.jps.base.tools;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Iterator;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;

/**
 * JUnit tests for RenameTool
 * 
 * @author Casper Lindberg
 *
 */
public class RenamingToolTest {

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
	
	static String removeWhiteSpace(String string) {
		return string.replaceAll("\\s+","");
	}
	
	/**
	 * Test constructor RenamingTool(String strMatch, String strTarget, String strReplacement) and setMatch method  
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	@Test
	public void testConstructAndSetMatch() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		String replacement = "test/constructor/replacement";
		String target = "test/constructor/target";
		String match = "test/constructor/match";
		
		//constructor
		RenamingTool renamingTool = new RenamingTool(match, target, replacement);
		
		//get field
		assertNotNull(renamingTool.getClass().getDeclaredField("strMatch"));
		Field field1 = renamingTool.getClass().getDeclaredField("strMatch");
		field1.setAccessible(true);
		
		//assert that the constructor set variable correctly
		String fieldValue1 = (String) field1.get(renamingTool);
		assertEquals(match, fieldValue1);
		
		//test setter method
		match = "test/match";
		renamingTool.setMatch(match);
		fieldValue1 = (String) field1.get(renamingTool);
		assertEquals(match, fieldValue1);
	}
	
	
	/**
	 * Test constructor RenamingTool(String strTarget, String strReplacement) and setTarget and setReplacement methods  
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	@Test
	public void testConstructSetTargetAndReplacment() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		String replacement = "test/constructor/replacement";
		String target = "test/constructor/target";
		
		//constructor
		RenamingTool renamingTool = new RenamingTool(target, replacement);
		
		//get private variables
		assertNotNull(renamingTool.getClass().getDeclaredField("strReplacement"));
		Field field1 = renamingTool.getClass().getDeclaredField("strReplacement");
		field1.setAccessible(true);
		
		assertNotNull(renamingTool.getClass().getDeclaredField("strTarget"));
		Field field2 = renamingTool.getClass().getDeclaredField("strTarget");
		field2.setAccessible(true);
		
		//assert that constructor set the variables correctly
		String fieldValue1 = (String) field1.get(renamingTool);
		assertEquals(replacement, fieldValue1);
		
		String fieldValue2 = (String) field2.get(renamingTool);
		assertEquals(target, fieldValue2);
		
		//test setter methods
		target = "test/target";
		renamingTool.setTarget(target);
		fieldValue2 = (String) field2.get(renamingTool);
		assertEquals(target, fieldValue2);
		
		replacement = "test/replacement";
		renamingTool.setReplacement(replacement);
		fieldValue1 = (String) field1.get(renamingTool);
		assertEquals(replacement, fieldValue1);
	}
	
	/**
	 * Test setter methods: setUpdateSize (number of triples renamed per update) 
	 * and setSingleUpdate (perform renaming using a single update)
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	public void testSetUpdateSize() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		RenamingTool renamingTool = new RenamingTool("", "");
		
		//Access private variables
		assertNotNull(renamingTool.getClass().getDeclaredField("splitUpdate"));
		Field field1 = renamingTool.getClass().getDeclaredField("splitUpdate");
		field1.setAccessible(true);
		
		assertNotNull(renamingTool.getClass().getDeclaredField("stepSize"));
		Field field2 = renamingTool.getClass().getDeclaredField("stepSize");
		field2.setAccessible(true);
		
		//Assert initial state
		Boolean fieldValue1 = (Boolean) field1.get(renamingTool);
		assertTrue(fieldValue1);
		
		int fieldValue2 = (int) field2.get(renamingTool);
		assertEquals(1000000, fieldValue2);
		
		//Test setter method for update step size
		renamingTool.setUpdateSize(9);
		fieldValue2 = (int) field2.get(renamingTool);
		assertEquals(9, fieldValue2);
		
		//Test setting single update variable
		renamingTool.setSingleUpdate();
		fieldValue1 = (Boolean) field1.get(renamingTool);
		assertFalse(fieldValue1);
	}
	
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
		TripleStoreClientInterface kbClient = new FileBasedStoreClient(tempFilePath.toString());
		
		//perform renaming as single update
		RenamingTool renamingTool = new RenamingTool(target, replacement);
		renamingTool.setSingleUpdate();
		renamingTool.renameIRI(kbClient);
		
		//select query to check result: predicate renamed
		JSONArray resultP = kbClient.executeQuery(sparqlQueryP);
		
		//check result (subject)
		Iterator<Object> iteratorP = resultP.iterator();
		while(iteratorP.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorP.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("p").toString());
		}
		
		//select query to check result: subject renamed
		JSONArray resultS = kbClient.executeQuery(sparqlQueryS);
		
		//check result (predicate)
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
	public void testRenameString() throws URISyntaxException, ParseException, IOException {
				
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "altLabel";
		String replacement = "replacement";
		
		//copy species to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		TripleStoreClientInterface kbClient = new FileBasedStoreClient(tempFilePath.toString());
		
		//perform renaming as single update
		RenamingTool renamingTool = new RenamingTool(target, replacement);
		renamingTool.setSingleUpdate();
		renamingTool.renameString(kbClient);
		
		//select query to check result: predicate renamed
		JSONArray resultP = kbClient.executeQuery(sparqlQueryP);
		
		//check result (predicate)
		Iterator<Object> iteratorP = resultP.iterator();
		while(iteratorP.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorP.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("p").toString());
		}
		
		//select query to check result: subject renamed
		JSONArray resultS = kbClient.executeQuery(sparqlQueryS);
		
		//check result (subject)
		Iterator<Object> iteratorS = resultS.iterator();
		while(iteratorS.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorS.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("s").toString());
		}
	}
	
	/**
	 * Test the method renameString, performing the renaming with multiple updates 
	 * @throws URISyntaxException
	 * @throws ParseException
	 * @throws IOException
	 */
	@Test
	public void testRenameStringMultipleUpdates() throws URISyntaxException, ParseException, IOException {
		
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "altLabel";
		String replacement = "replacement";
		
		//copy species to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		TripleStoreClientInterface kbClient = new FileBasedStoreClient(tempFilePath.toString());
		
		//perform update
		RenamingTool renamingTool = new RenamingTool(target, replacement);
		renamingTool.setUpdateSize(2); //update 2 triples at a time
		renamingTool.renameString(kbClient);
		
		//select query to check result: predicate renamed
		JSONArray resultP = kbClient.executeQuery(sparqlQueryP);
		
		//check result (predicate)
		Iterator<Object> iteratorP = resultP.iterator();
		while(iteratorP.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorP.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("p").toString());
		}
		
		//select query to check result: subject renamed
		JSONArray resultS = kbClient.executeQuery(sparqlQueryS);
		
		//check result (subject)
		Iterator<Object> iteratorS = resultS.iterator();
		while(iteratorS.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorS.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("s").toString());
		}
	}
	
	/**
	 * Test the method renameURI, performing the renaming with multiple updates
	 * @throws URISyntaxException
	 * @throws ParseException
	 * @throws IOException
	 */
	@Test
	public void testRenameURIMultipleUpdates() throws URISyntaxException, ParseException, IOException {
		
		Path testFilePath = Paths.get(this.getClass().getResource("/ToolsTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");
		
		String target = "http://www.w3.org/2008/05/skos#altLabel";
		String replacement = "http://www.w3.org/2008/05/skos#replacement";
		
		//copy species.owl to temporary folder
		Files.copy(testFilePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		
		//create kbClient
		TripleStoreClientInterface kbClient = new FileBasedStoreClient(tempFilePath.toString());
		
		//perform update
		RenamingTool renamingTool = new RenamingTool(target, replacement);
		renamingTool.setUpdateSize(2); //update 2 triples at a time
		renamingTool.renameIRI(kbClient);
		
		//select query to check result: predicate renamed
		JSONArray resultP = kbClient.executeQuery(sparqlQueryP);
		
		//check result (predicate)
		Iterator<Object> iteratorP = resultP.iterator();
		while(iteratorP.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorP.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("p").toString());
		}
		
		//select query to check result: subject renamed
		JSONArray resultS = kbClient.executeQuery(sparqlQueryS);
		
		//check result (subject)
		Iterator<Object> iteratorS = resultS.iterator();
		while(iteratorS.hasNext()) {
			JSONObject ob = new JSONObject();
			ob = (JSONObject) iteratorS.next();
			assertEquals("http://www.w3.org/2008/05/skos#replacement", ob.get("s").toString());
		}
	}
	
	/**
	 * Test method to build countQuery
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testCountQuery() throws NoSuchMethodException, SecurityException, IllegalArgumentException, IllegalAccessException, NoSuchFieldException, InvocationTargetException {
		
		RenamingTool renamingTool = new RenamingTool("http://example.com/test/target","http://example.com/test/replacement");
		
		//get count variable
		assertNotNull(renamingTool.getClass().getDeclaredField("varCount"));
		Field field1 = renamingTool.getClass().getDeclaredField("varCount");
		field1.setAccessible(true);
		String varCount = (String) field1.get(renamingTool);
		
		//arguments for method
		String graph = "http://example.com/test/graph";
		WhereBuilder whereInput = new WhereBuilder().addWhere("?s","?p","?o");
	
		//expected query string
		String expected = "SELECT (COUNT(*) AS ?" + varCount + ") WHERE\n"+
				  "  { GRAPH <http://example.com/test/graph>\n"+
			      "      { ?s  ?p  ?o }\n  }\n";
		
		//invoke private method
		assertNotNull(renamingTool.getClass().getDeclaredMethod("countQuery", String.class, WhereBuilder.class));
		Method method = renamingTool.getClass().getDeclaredMethod("countQuery", String.class, WhereBuilder.class);
		method.setAccessible(true);
		String where = (String) method.invoke(renamingTool, graph, whereInput);
		assertEquals(removeWhiteSpace(expected), removeWhiteSpace(where));
	}
	
	/**
	 * Test method whereMatchURI and whereUpdateURI
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 * @throws NoSuchFieldException
	 */
	@Test
	public void testWhereIRI() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
		
		RenamingTool renamingTool = new RenamingTool("http://example.com/test/target","http://example.com/test/replacement");
		
		//Test method whereMatchURI
		String expectedMatch = "WHERE\n" +
				"  { ?s  ?p  ?o\n" +
				"    BIND(<http://example.com/test/target> AS ?targetURI)\n" +
				"    BIND(<http://example.com/test/replacement> AS ?replacementURI)\n" +
				"    FILTER ( ( ?s = ?targetURI ) || ( ( ?p = ?targetURI ) || ( ?o = ?targetURI ) ) )\n" +
				"  }\n";
		assertNotNull(renamingTool.getClass().getDeclaredMethod("whereMatchIRI"));
		Method method = renamingTool.getClass().getDeclaredMethod("whereMatchIRI");
		method.setAccessible(true);
		WhereBuilder whereMatch = (WhereBuilder) method.invoke(renamingTool);
		String strWhereMatch = whereMatch.toString();
		assertEquals(expectedMatch, strWhereMatch);
		
		//Test method whereUpdateURI
		String expectedUpdate = "WHERE\n"+
				"  { ?s  ?p  ?o\n"+
				"    BIND(<http://example.com/test/target> AS ?targetURI)\n"+
				"    BIND(<http://example.com/test/replacement> AS ?replacementURI)\n"+
				"    FILTER ( ( ?s = ?targetURI ) || ( ( ?p = ?targetURI ) || ( ?o = ?targetURI ) ) )\n"+ 
				"    BIND(if(isBlank(?s), ?s, if(( ?s = ?targetURI ), ?replacementURI, ?s)) AS ?newS)\n"+
				"    BIND(if(isBlank(?p), ?p, if(( ?p = ?targetURI ), ?replacementURI, ?p)) AS ?newP)\n"+
			   	"    BIND(if(isBlank(?o), ?o, if(( ?o = ?targetURI ), ?replacementURI, ?o)) AS ?newO)\n"+
				"  }\n";
		assertNotNull(renamingTool.getClass().getDeclaredMethod("whereUpdateIRI"));
		Method method2 = renamingTool.getClass().getDeclaredMethod("whereUpdateIRI");
		method2.setAccessible(true);
		WhereBuilder whereUpdate = (WhereBuilder) method2.invoke(renamingTool);	
		String strWhereUpdate = whereUpdate.toString();	
		assertEquals(expectedUpdate, strWhereUpdate);
	}
	
	/**
	 * Test method whereMatchString and whereUpdateString with optional filter expression
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws NoSuchFieldException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testWhereString() throws NoSuchMethodException, SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		
		ExprFactory exprFactory = new ExprFactory();
		
		String strTarget = "http://example.com/test/target";
		Expr exprTarget = exprFactory.asExpr(strTarget);
		
		String strMatch = "http://example.com/test/match";
		Expr exprMatch = exprFactory.asExpr(strMatch);
		
		String strReplacement = "http://example.com/test/replacement";
		
		RenamingTool renamingTool = new RenamingTool(strTarget,strReplacement);
		
		//set variables:	match = target
		assertNotNull(renamingTool.getClass().getDeclaredField("exprMatch"));
		Field field1 = renamingTool.getClass().getDeclaredField("exprMatch");
		field1.setAccessible(true);
		field1.set(renamingTool, exprTarget);
		
		assertNotNull(renamingTool.getClass().getDeclaredField("exprTarget"));
		Field field2 = renamingTool.getClass().getDeclaredField("exprTarget");
		field2.setAccessible(true);
		field2.set(renamingTool, exprTarget);
		
		//test optional filter expression
		
		//set filter
		ExprVar exprOldS = RenamingTool.exprS; //get variable
		Expr filterExpected = exprFactory.strstarts(exprFactory.str(exprOldS), exprFactory.asExpr("http://www.theworldavatar.com/"));
		renamingTool.setFilter(filterExpected);
		
		//assert filter variable set correctly
		assertNotNull(renamingTool.getClass().getDeclaredField("exprFilter"));
		Field field3 = renamingTool.getClass().getDeclaredField("exprFilter");
		field3.setAccessible(true);
		Expr filter = (Expr) field3.get(renamingTool);
		assertEquals(filterExpected,filter);
		
		//Test method whereMatchString
		String expectedMatch = "WHERE\n"+
				"  { ?s  ?p  ?o\n"+
				"    FILTER strstarts(str(?s), \"http://www.theworldavatar.com/\")\n"+
				"    BIND(contains(str(?s), \""+strTarget+"\") AS ?matchS)\n"+
				"    BIND(contains(str(?p), \""+strTarget+"\") AS ?matchP)\n"+
				"    BIND(contains(str(?o), \""+strTarget+"\") AS ?matchO)\n"+
				"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n"+ 
				"  }\n";
		assertNotNull(renamingTool.getClass().getDeclaredMethod("whereMatchString"));
		Method method = renamingTool.getClass().getDeclaredMethod("whereMatchString");
		method.setAccessible(true);
		WhereBuilder whereMatch = (WhereBuilder) method.invoke(renamingTool);	
		String strWhereMatch = whereMatch.toString();	
		assertEquals(expectedMatch, strWhereMatch);
	
		//Test whereMatchString with match != target
		expectedMatch = "WHERE\n"+
				"  { ?s  ?p  ?o\n"+
				"    FILTER strstarts(str(?s), \"http://www.theworldavatar.com/\")\n"+
				"    BIND(( contains(str(?s), \""+strMatch+"\") && contains(str(?s), \""+strTarget+"\") ) AS ?matchS)\n"+
				"    BIND(( contains(str(?p), \""+strMatch+"\") && contains(str(?p), \""+strTarget+"\") ) AS ?matchP)\n"+
				"    BIND(( contains(str(?o), \""+strMatch+"\") && contains(str(?o), \""+strTarget+"\") ) AS ?matchO)\n"+
				"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n"+ 
				"  }\n";
		field1.set(renamingTool, exprMatch); //set match != target
		whereMatch = (WhereBuilder) method.invoke(renamingTool);	
		strWhereMatch = whereMatch.toString();	
		assertEquals(expectedMatch, strWhereMatch);
		
		//Test method whereUpdateString
		String expectedUpdate = "WHERE\n"+
				"  { ?s  ?p  ?o\n"+
				"    FILTER strstarts(str(?s), \"http://www.theworldavatar.com/\")\n"+
				"    BIND(( contains(str(?s), \""+strMatch+"\") && contains(str(?s), \""+strTarget+"\") ) AS ?matchS)\n"+
				"    BIND(( contains(str(?p), \""+strMatch+"\") && contains(str(?p), \""+strTarget+"\") ) AS ?matchP)\n"+
				"    BIND(( contains(str(?o), \""+strMatch+"\") && contains(str(?o), \""+strTarget+"\") ) AS ?matchO)\n"+
				"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n"+ 
				"    BIND(if(isBlank(?s), ?s, if(?matchS, iri(replace(str(?s), \""+strTarget+"\", \""+strReplacement+"\")), ?s)) AS ?newS)\n"+ 
			    "    BIND(if(isBlank(?p), ?p, if(?matchP, iri(replace(str(?p), \""+strTarget+"\", \""+strReplacement+"\")), ?p)) AS ?newP)\n"+ 
			    "    BIND(if(isBlank(?o), ?o, if(?matchO, iri(replace(str(?o), \""+strTarget+"\", \""+strReplacement+"\")), ?o)) AS ?newO)\n"+ 
				"  }\n";
		assertNotNull(renamingTool.getClass().getDeclaredMethod("whereUpdateString"));
		Method method2 = renamingTool.getClass().getDeclaredMethod("whereUpdateString");
		method2.setAccessible(true); 
		WhereBuilder whereUpdate = (WhereBuilder) method2.invoke(renamingTool);	
		String strWhereUpdate = whereUpdate.toString();	
		assertEquals(expectedUpdate, strWhereUpdate);	
	}
	
	/**
	 * Test method buildSparqlUpdate with and without named graph
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 */
	@Test
	public void testBuildSparqlUpdate() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
		
		String expectedGraph = "DELETE {\n"+
			  "  GRAPH <http://example.com/test/graph> {\n"+
			  "    ?s ?p ?o .\n"+
			  "  }\n"+
			  "}\n"+
			  "INSERT {\n"+
			  "  GRAPH <http://example.com/test/graph> {\n"+
			  "    ?newS ?newP ?newO .\n"+
			  "  }\n"+
			  "}\n"+
			  "WHERE\n"+
			  "  { { SELECT  ?s ?p ?o ?newS ?newP ?newO\n"+
			  "      WHERE\n"+
			  "        { GRAPH <http://example.com/test/graph>\n"+
			  "            { ?s  ?p  ?o}}\n"+
			  "      LIMIT   99\n"+
			  "    }\n"+
			  "  }\n";
		
		String expectedQuads = "DELETE {\n"+
				  "  GRAPH ?g {\n"+
				  "    ?s ?p ?o .\n"+
				  "  }\n"+
				  "}\n"+
				  "INSERT {\n"+
				  "  GRAPH ?g {\n"+
				  "    ?newS ?newP ?newO .\n"+
				  "  }\n"+
				  "}\n"+
				  "WHERE\n"+
				  "  { { SELECT  ?s ?p ?o ?newS ?newP ?newO ?g\n"+
				  "      WHERE\n"+
				  "        { GRAPH ?g\n"+
				  "            { ?s  ?p  ?o}}\n"+
				  "      LIMIT   99\n"+
				  "    }\n"+
				  "  }\n";
		
		String expectedTriples = "DELETE {\n"+
				  "  ?s ?p ?o .\n"+
				  "}\n"+
				  "INSERT {\n"+
				  "  ?newS ?newP ?newO .\n"+
				  "}\n"+
				  "WHERE\n"+
				  "  { { SELECT  ?s ?p ?o ?newS ?newP ?newO\n"+
				  "      WHERE\n"+
				  "        { ?s  ?p  ?o}\n"+
				  "      LIMIT   99\n"+
				  "    }\n"+
				  "  }\n";
		
		RenamingTool renamingTool = new RenamingTool("","");
		
		//arguments
		String graph = "http://example.com/test/graph";
		WhereBuilder where = new WhereBuilder().addWhere("?s","?p","?o");
		int limit = 99;
		
		//call method with graph
		assertNotNull(renamingTool.getClass().getDeclaredMethod("buildSparqlUpdate", String.class, WhereBuilder.class, int.class));
		Method method1 = renamingTool.getClass().getDeclaredMethod("buildSparqlUpdate", String.class, WhereBuilder.class, int.class);
		method1.setAccessible(true); 
		UpdateRequest update = (UpdateRequest) method1.invoke(renamingTool, graph, where, limit);
		String strUpdate = update.toString();
		assertEquals(expectedGraph,strUpdate);
		
		//call method with no graph for quad store
		graph = null; 
		update = (UpdateRequest) method1.invoke(renamingTool, graph, where, limit);
		strUpdate = update.toString();
		assertEquals(expectedQuads,strUpdate);
		
		//call method with no graph for triple store
		graph = null; 
		renamingTool.setTripleStore();
		update = (UpdateRequest) method1.invoke(renamingTool, graph, where, limit);
		strUpdate = update.toString();
		assertEquals(expectedTriples,strUpdate);
	}

}