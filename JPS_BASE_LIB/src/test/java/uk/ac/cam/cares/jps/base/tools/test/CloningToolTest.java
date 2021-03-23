package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.Assert.*;
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

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.update.UpdateRequest;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;

import uk.ac.cam.cares.jps.base.tools.CloningTool;

/**
 * Unit tests for Cloning Tool
 * 
 * @author Casper Lindberg
 *
 */
public class CloningToolTest {

		// temporary folder for testing
		@Rule
		public TemporaryFolder tempFolder = new TemporaryFolder();
		
		private String filePath1;
		private String filePath2;
		
		/**
		 * Copy test resources into temporary folder.
		 * @throws URISyntaxException
		 * @throws IOException
		 */
		@Before
		public void setup() throws URISyntaxException, IOException {
			
			// Test file
			Path testResourcePath = Paths.get(this.getClass().getResource("/ToolsTest/clonetest1.owl").toURI());
			Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/clonetest1.owl");		
			Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
			filePath1 = tempFilePath.toString();
			
			Path testResourcePath2 = Paths.get(this.getClass().getResource("/ToolsTest/clonetest2.owl").toURI());
			Path tempFilePath2 = Paths.get(tempFolder.getRoot().toString() + "/clonetest2.owl");		
			Files.copy(testResourcePath2, tempFilePath2, StandardCopyOption.REPLACE_EXISTING);
			filePath2 = tempFilePath2.toString();
		}
		
		@Test
		public void testConstructorAndSetter() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			int stepSize = 99;
			CloningTool cloningTool = new CloningTool();
			
			Field field = null;
			
			assertNotNull(cloningTool.getClass().getDeclaredField("splitUpdate"));
			field = cloningTool.getClass().getDeclaredField("splitUpdate");
			field.setAccessible(true);
			boolean value = (boolean) field.get(cloningTool);
			assertTrue(value);
			
			//set single step clone
			cloningTool.setSingleStepClone();
			value = (boolean) field.get(cloningTool);
			assertFalse(value);
			
			assertNotNull(cloningTool.getClass().getDeclaredField("stepSize"));
			field = cloningTool.getClass().getDeclaredField("stepSize");
			field.setAccessible(true);
			int value2 = (int) field.get(cloningTool);
			assertEquals(1000000,value2);
			
			//set step size
			cloningTool.setCloneSize(stepSize);
			value2 = (int) field.get(cloningTool);
			assertEquals(stepSize,value2);
		}
		
		@Test
		public void testConstructorStepSize() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			int stepSize = 99;
			CloningTool cloningTool = new CloningTool(stepSize);
			
			Field field = null;
			
			//get private variables
			assertNotNull(cloningTool.getClass().getDeclaredField("splitUpdate"));
			field = cloningTool.getClass().getDeclaredField("splitUpdate");
			field.setAccessible(true);
			boolean value = (boolean) field.get(cloningTool);
			assertTrue(value);
			
			assertNotNull(cloningTool.getClass().getDeclaredField("stepSize"));
			field = cloningTool.getClass().getDeclaredField("stepSize");
			field.setAccessible(true);
			int value2 = (int) field.get(cloningTool);
			assertEquals(stepSize,value2);
		}
	
		/**
		 *  Test clone tool from a FileBasedKBClient to another FileBasedKBClient
		 */
		@Test
		public void testClone() {
			
			FileBasedKnowledgeBaseClient source = new FileBasedKnowledgeBaseClient();
			source.load("http://example.com/test1", filePath1);
			source.load("http://example.com/test2", filePath2);
			
			FileBasedKnowledgeBaseClient target = new FileBasedKnowledgeBaseClient();
			
			CloningTool cloningTool = new CloningTool();
			cloningTool.setSingleStepClone();
			
			//copy graph 1
			cloningTool.clone(source, target, "http://example.com/test1");
			
			//check copied
			assertEquals("[{\"O\":\"OH\"}]", target.execute(getQuery("http://example.com/test1", "1")));
			assertEquals("[{\"O\":\"O\"}]", target.execute(getQuery("http://example.com/test1", "2")));
			
			//copy graph 2
			cloningTool.clone(source, target, "http://example.com/test2");
			
			//check copied
			assertEquals("[{\"O\":\"O2\"}]", target.execute(getQuery("http://example.com/test2", "3")));
			assertEquals("[{\"O\":\"N2\"}]", target.execute(getQuery("http://example.com/test2", "4")));
		}
		
		/**
		 * Get test query.
		 * 
		 * @param graph
		 * @param species
		 * @return
		 */
		private String getQuery(String graph, String species) {
				
			String G = "<" + graph + ">";
			String S = "<http://www.theworldavatar.com/kb/species/species.owl#species_"+ species +">";
			Var O = Var.alloc("O");
			
			SelectBuilder builder = new SelectBuilder()
					.addVar(O)
					.addGraph(G, S,"<http://www.w3.org/2008/05/skos#altLabel>",O);
					
			return builder.build().toString();
		}
		
		//TODO testClone
		//TODO testCloneGraph
		//TODO testPerfromSingleStepClone
		//TODO testPerfromClone
		//TODO testCreateTag
		//TODO testCheckCount
		//TODO testCheckTags
		
		//// Count methods
		
		//TODO testCountTriples
		
		@Test
		public void testCountQuery() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
			
			CloningTool cloningTool = new CloningTool();
			
			//Check count variable
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("varCount"));
			field = cloningTool.getClass().getDeclaredField("varCount");
			field.setAccessible(true);
			String countValue = (String) field.get(cloningTool);
			assertEquals("count",countValue);
			
			Var[] sparqlArgs = getSparqlArgs();
			Var varS = sparqlArgs[0];
			Var varP = sparqlArgs[1];
			Var varO = sparqlArgs[2];
			
			//test arguments -- default graph
			WhereBuilder where = new WhereBuilder().addWhere(varS, varP, varO);
			String graph = null;
			
			String expectedValue = "SELECT (COUNT(*) AS ?count) WHERE\n"+
					"  { ?s  ?p  ?o }\n";
			
			Method method = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("countQuery", String.class, WhereBuilder.class));
			method = cloningTool.getClass().getDeclaredMethod("countQuery", String.class, WhereBuilder.class);
			method.setAccessible(true);
			String value = (String) method.invoke(cloningTool, graph, where);
			assertEquals(expectedValue, value.toString());
			
			//test argument -- graph
			graph = "http://example.com";
			
			expectedValue = "SELECT (COUNT(*) AS ?count) WHERE\n"+
					"  { GRAPH <"+graph+">\n"+
				    "      { ?s  ?p  ?o }\n"+
		  			"  }\n";
			value = (String) method.invoke(cloningTool, graph, where);
			assertEquals(expectedValue, value.toString());
		}
		
		//// Sparql builder methods
		
		@Test
		public void testBuildConstruct() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
			
			CloningTool cloningTool = new CloningTool();
			
			Var[] sparqlArgs = getSparqlArgs();
			Var varS = sparqlArgs[0];
			Var varP = sparqlArgs[1];
			Var varO = sparqlArgs[2];
			
			//test arguments -- default graph
			WhereBuilder where = new WhereBuilder().addWhere(varS, varP, varO);
			String graph = null;
			
			String expectedValue = "CONSTRUCT \n"+
						"  { \n"+
						"    ?s ?p ?o .\n"+
						"  }\n"+
						"WHERE\n"+
						"  { ?s  ?p  ?o}\n";
			
			Method method = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildConstruct", String.class, WhereBuilder.class));
			method = cloningTool.getClass().getDeclaredMethod("buildConstruct", String.class, WhereBuilder.class);
			method.setAccessible(true);
			Query value = (Query) method.invoke(cloningTool, graph, where);
			assertEquals(expectedValue, value.toString());
			
			//test argument -- graph
			graph = "http://example.com";
			
			expectedValue = "CONSTRUCT \n"+
					"  { \n"+
					"    ?s ?p ?o .\n"+
					"  }\n"+
					"WHERE\n"+
					"  { GRAPH <"+graph+">\n"+
					"      { ?s  ?p  ?o}}\n";
			
			value = (Query) method.invoke(cloningTool, graph, where);
			assertEquals(expectedValue, value.toString());
		}
		
		@Test
		public void testBuildInsert() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
			
			CloningTool cloningTool = new CloningTool();
			
			//test arguments -- default graph
			String graph = null;
			Model model = getTestModel();
			
			String expectedValue = "INSERT DATA {\n"+
						"  <http://example.com/S> <http://example.com/P> <http://example.com/O> .\n"+
						"}\n";
			
			Method method = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildInsert", String.class, Model.class));
			method = cloningTool.getClass().getDeclaredMethod("buildInsert", String.class, Model.class);
			method.setAccessible(true);
			UpdateRequest value = (UpdateRequest) method.invoke(cloningTool, graph, model);
			assertEquals(expectedValue, value.toString());
			
			//test argument -- graph
			graph = "http://example.com";
			
			expectedValue = "INSERT DATA {\n"+
					"  GRAPH <http://example.com> {\n"+
					"    <http://example.com/S> <http://example.com/P> <http://example.com/O> .\n"+
					"  }\n}\n";
			
			value = (UpdateRequest) method.invoke(cloningTool, graph, model);
			assertEquals(expectedValue, value.toString());
		}
		
		@Test
		public void testBuildTagUpdate() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
			
			CloningTool cloningTool = new CloningTool();
			
			Var[] sparqlArgs = getSparqlArgs();
			Var varS = sparqlArgs[0];
			Var varP = sparqlArgs[1];
			Var varO = sparqlArgs[2];
			
			//test arguments -- default graph
			int limit = 999;
			WhereBuilder where = new WhereBuilder().addWhere(varS, varP, varO);
			String graph = null;
			
			//expected
			String expectedValue = "DELETE {\n"+
					"  ?s ?p ?o .\n"+
					"}\n"+
					"INSERT {\n"+
					"  ?newS ?p ?o .\n"+
					"}\n"+
					"WHERE\n"+
					"  { { SELECT  ?s ?p ?o ?newS\n"+
					"      WHERE\n"+
					"        { ?s  ?p  ?o}\n"+
					"      LIMIT   "+limit+
					"\n    }\n  }\n";
			
			Method method = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildTagUpdate", String.class, WhereBuilder.class, int.class));
			method = cloningTool.getClass().getDeclaredMethod("buildTagUpdate", String.class, WhereBuilder.class, int.class);
			method.setAccessible(true);
			UpdateRequest value = (UpdateRequest) method.invoke(cloningTool, graph, where, limit);
			assertEquals(expectedValue, value.toString());
			
			//test arguments -- graph
			graph = "http://example.com";
			
			expectedValue = "DELETE {\n"+
					"  GRAPH <"+graph+"> {\n"+
					"    ?s ?p ?o .\n"+
					"  }\n}\n"+
					"INSERT {\n"+
					"  GRAPH <"+graph+"> {\n"+
					"    ?newS ?p ?o .\n"+
					"  }\n}\n"+
					"WHERE\n"+
					"  { { SELECT  ?s ?p ?o ?newS\n"+
					"      WHERE\n"+
					"        { GRAPH \""+graph+"\"\n"+
					"            { ?s  ?p  ?o}}\n"+
					"      LIMIT   "+limit+
					"\n    }\n  }\n";
			
			UpdateRequest value2 = (UpdateRequest) method.invoke(cloningTool, graph, where, limit);
			assertEquals(expectedValue, value2.toString());
		}
		
		private Var[] getSparqlArgs() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			CloningTool cloningTool = new CloningTool();
			
			//Get sparql variables
			assertNotNull(cloningTool.getClass().getDeclaredField("varS"));
			assertNotNull(cloningTool.getClass().getDeclaredField("varP"));
			assertNotNull(cloningTool.getClass().getDeclaredField("varO"));
			Field fieldS = cloningTool.getClass().getDeclaredField("varS");
			fieldS.setAccessible(true);
			Var varS = (Var) fieldS.get(cloningTool);
			assertEquals("?s", varS.toString());
			
			Field fieldP = cloningTool.getClass().getDeclaredField("varP");
			fieldP.setAccessible(true);
			Var varP = (Var) fieldP.get(cloningTool);
			assertEquals("?p", varP.toString());
			
			Field fieldO = cloningTool.getClass().getDeclaredField("varO");
			fieldO.setAccessible(true);
			Var varO = (Var) fieldO.get(cloningTool);
			assertEquals("?o", varO.toString());
			
			Var[] args = {varS,varP,varO};
			return args;
		}
		
		//// Test filter expressions
		
		@Test
		public void testExpressions() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException, NoSuchFieldException {
			
			CloningTool cloningTool = new CloningTool();
			
			String expectedTag = "_Tag";
			
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String tagValue = (String) field.get(cloningTool);
			assertEquals(expectedTag,tagValue);
			
			Method method = null;
			Expr value = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildExprTagN", int.class));
			method = cloningTool.getClass().getDeclaredMethod("buildExprTagN", int.class);
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool, 1);
			assertEquals("\"_1"+expectedTag+"\"", value.toString());
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprFilterOutBlanks"));
			method = cloningTool.getClass().getDeclaredMethod("exprFilterOutBlanks");
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool);
			assertEquals("(&& (! (isBlank ?s)) (&& (! (isBlank ?p)) (! (isBlank ?o))))", value.toString());
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprTagged"));
			method = cloningTool.getClass().getDeclaredMethod("exprTagged");
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool);
			assertEquals("(strends (str ?s) \""+expectedTag+"\")", value.toString());
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprNotTagged"));
			method = cloningTool.getClass().getDeclaredMethod("exprNotTagged");
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool);
			assertEquals("(! (strends (str ?s) \""+expectedTag+"\"))", value.toString());
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprBindIriRemoveTag", Expr.class));
			method = cloningTool.getClass().getDeclaredMethod("exprBindIriRemoveTag", Expr.class);
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool, (Expr) null);
			assertEquals("(iri (replace (str ?s) NONE \"\"))", value.toString());
		}
		
		//// Test sparql builder for single step clone 
		
		/**
		 * Test sparql construct builder.
		 * 
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 * @throws InvocationTargetException
		 * @throws NoSuchMethodException
		 * @throws SecurityException
		 */
		@Test
		public void testBuildSparqlConstruct() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
			
			//Expected result
			String expected = "CONSTRUCT \n"+
					"  { \n"+
					"    ?s ?p ?o .\n"+
					"  }\n"+
					"WHERE\n"+
					"  { GRAPH </test/target>\n"+
					"      { ?s  ?p  ?o}}\n";
			
			//Invoke method
			CloningTool cloningTool = new CloningTool();
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildSparqlConstruct", String.class));
			Method method = cloningTool.getClass().getDeclaredMethod("buildSparqlConstruct", String.class);
			method.setAccessible(true);		
			Query result = (Query) method.invoke(cloningTool, "/test/target");
			
			assertEquals(expected, result.toString());
		}
		
		/**
		 * Test sparql construct builder. Null graph argument.
		 * 
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 * @throws InvocationTargetException
		 * @throws NoSuchMethodException
		 * @throws SecurityException
		 */
		@Test
		public void testBuildSparqlConstructNoGraph() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
			
			//Expected result
			String expected = "CONSTRUCT \n"+
					"  { \n"+
					"    ?s ?p ?o .\n"+
					"  }\n"+
					"WHERE\n"+
					"  { ?s  ?p  ?o}\n";
			
			//Invoke method
			CloningTool cloningTool = new CloningTool();
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildSparqlConstruct", String.class));
			Method method = cloningTool.getClass().getDeclaredMethod("buildSparqlConstruct", String.class);
			method.setAccessible(true);		
			Query result = (Query) method.invoke(cloningTool, new Object[]{ null });
			
			assertEquals(expected, result.toString());
		}
		
		/**
		 * Test sparql update builder.
		 * 
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 * @throws InvocationTargetException
		 * @throws NoSuchMethodException
		 * @throws SecurityException
		 */
		@Test 
		public void testBuildSparqlUpdate() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
			
			//Expected result
			String expected = "INSERT DATA {\n"+
					"  GRAPH </test/target> {\n"+ 
					"    <http://example.com/S> <http://example.com/P> <http://example.com/O> .\n"+
					"  }\n}\n";
			
			//Test model
			Model model = getTestModel();
			
			//Invoke method
			CloningTool cloningTool = new CloningTool();
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildSparqlUpdate", String.class, Model.class));
			Method method = cloningTool.getClass().getDeclaredMethod("buildSparqlUpdate", String.class, Model.class);
			method.setAccessible(true);
			
			UpdateRequest result = (UpdateRequest) method.invoke(cloningTool, "/test/target", model);
			
			String strResult = result.toString();
			assertEquals(expected, strResult);
		}
		
		/**
		 * Test sparql update builder. No graph specified.
		 * 
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 * @throws InvocationTargetException
		 * @throws NoSuchMethodException
		 * @throws SecurityException
		 */
		@Test 
		public void testBuildSparqlUpdateNullGraph() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
			
			//Expected result
			String expected = "INSERT DATA {\n"+ 
					"  <http://example.com/S> <http://example.com/P> <http://example.com/O> .\n"+
					"}\n";
			
			//Test model
			Model model = getTestModel();
			
			// access private member
			CloningTool cloningTool = new CloningTool();
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildSparqlUpdate", String.class, Model.class));
			Method method = cloningTool.getClass().getDeclaredMethod("buildSparqlUpdate", String.class, Model.class);
			method.setAccessible(true);
			
			UpdateRequest result = (UpdateRequest) method.invoke(cloningTool, null, model);
			
			String strResult = result.toString();
			assertEquals(expected, strResult);
		}
		
		/**
		 * Create a test model. 
		 * @return
		 */
		private Model getTestModel() {
			
			//Test model
			Model model = ModelFactory.createDefaultModel();	
			Node  s = ResourceFactory.createResource("http://example.com/S").asNode();
			Node  p = ResourceFactory.createResource("http://example.com/P").asNode();   
			Node  o = ResourceFactory.createResource("http://example.com/O").asNode();
			model.add( model.asStatement(new Triple(s,p,o)));
			return model;
		}
}
