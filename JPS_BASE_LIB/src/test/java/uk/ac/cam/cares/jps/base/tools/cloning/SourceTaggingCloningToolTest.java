package uk.ac.cam.cares.jps.base.tools.cloning;

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

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
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

import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;

/**
 * Unit tests for Source Tagging Cloning Tool
 * 
 * @author Casper Lindberg
 *
 */
public class SourceTaggingCloningToolTest {

		// temporary folder for testing
		@Rule
		public TemporaryFolder tempFolder = new TemporaryFolder();
		
		private String filePath1;
		private String filePath2;
		private String testContext = "http://example.com/test";
		
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
		public void testConstructorTripleStore() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			//default constructor
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool(false);
			
			Field field = null;
						
			assertNotNull(cloningTool.getClass().getDeclaredField("stepSize"));
			field = cloningTool.getClass().getDeclaredField("stepSize");
			field.setAccessible(true);
			int value2 = (int) field.get(cloningTool);
			assertEquals(1000000,value2);
						
			assertNotNull(cloningTool.getClass().getDeclaredField("quads"));
			field = cloningTool.getClass().getDeclaredField("quads");
			field.setAccessible(true);
			assertFalse((boolean) field.get(cloningTool));
		}
		
		@Test
		public void testConstructorQuadStore() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			//default constructor
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool(true);
			
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("quads"));
			field = cloningTool.getClass().getDeclaredField("quads");
			field.setAccessible(true);
			
			assertTrue((boolean) field.get(cloningTool));
		}
		
		@Test
		public void testConstructorStepSize() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			int stepSize = 99;
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool(stepSize, false);
			
			Field field = null;
						
			assertNotNull(cloningTool.getClass().getDeclaredField("stepSize"));
			field = cloningTool.getClass().getDeclaredField("stepSize");
			field.setAccessible(true);
			int value2 = (int) field.get(cloningTool);
			assertEquals(stepSize,value2);
		}
			
		/**
		 * Test clone (in quads mode) 
		 */
		@Test
		public void testCloneQuads() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException, NoSuchFieldException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool(1, true);
			TripleStoreClientInterface source = createTestClient();
			FileBasedStoreClient target = new FileBasedStoreClient();
			
			cloningTool.clone(source, target);
			
			//check countTotal set
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("countTotal"));
			field = cloningTool.getClass().getDeclaredField("countTotal");
			field.setAccessible(true);
			int intValue = (int) field.get(cloningTool);
			assertEquals(12,intValue);
			
			//check tag variable set
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String value = (String) field.get(cloningTool);
			assertTrue(value.contains("_Tag"));
			assertTrue(value.length() > 4);
						
			//check no tags
			assertTrue(cloningTool.checkNoTags(source, null));
			assertTrue(cloningTool.checkNoTags(target, null));
			
			//check count 
			assertTrue(cloningTool.checkCount(target,null));
			
			//check clone
			assertEquals("[{\"O\":\"OH\"}]", target.execute(getQuery(null, "1")));
			assertEquals("[{\"O\":\"O\"}]", target.execute(getQuery(null, "2")));
			assertEquals("[{\"O\":\"O2\"}]", target.execute(getQuery(null, "3")));
			assertEquals("[]", target.execute(getQuery(null, "4")));
			assertEquals("[]", target.execute(getQuery(testContext, "4")));	//named graph not cloned by FileBasedClient
		}
	
		/**
		 * Test clone (in triples mode)
		 */
		@Test
		public void testCloneTriples() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException, NoSuchFieldException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool(1, false);
			TripleStoreClientInterface source = createTestClient();
			FileBasedStoreClient target = new FileBasedStoreClient();
			
			cloningTool.clone(source, target);
			
			//check countTotal set
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("countTotal"));
			field = cloningTool.getClass().getDeclaredField("countTotal");
			field.setAccessible(true);
			int intValue = (int) field.get(cloningTool);
			assertEquals(12,intValue);
			
			//check tag variable set
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String value = (String) field.get(cloningTool);
			assertTrue(value.contains("_Tag"));
			assertTrue(value.length() > 4);
						
			//check no tags
			assertTrue(cloningTool.checkNoTags(source, null));
			assertTrue(cloningTool.checkNoTags(target, null));
			
			//check count 
			assertTrue(cloningTool.checkCount(target,null));
			
			//check clone
			assertEquals("[{\"O\":\"OH\"}]", target.execute(getQuery(null, "1")));
			assertEquals("[{\"O\":\"O\"}]", target.execute(getQuery(null, "2")));
			assertEquals("[{\"O\":\"O2\"}]", target.execute(getQuery(null, "3")));
			assertEquals("[]", target.execute(getQuery(null, "4")));
			assertEquals("[]", target.execute(getQuery(testContext, "4")));	//named graph not cloned by FileBasedClient
		}
				
		/**
		 * Clone a named graph
		 */
		@Test
		public void testCloneWithNamedGraph() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool(1, true);
			TripleStoreClientInterface source = createTestClient();
			FileBasedStoreClient target = new FileBasedStoreClient();
			
			cloningTool.clone(source, testContext, target, testContext);
			
			//check countTotal set
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("countTotal"));
			field = cloningTool.getClass().getDeclaredField("countTotal");
			field.setAccessible(true);
			int intValue = (int) field.get(cloningTool);
			assertEquals(6,intValue);
			
			//check tag variable set
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String value = (String) field.get(cloningTool);
			assertTrue(value.contains("_Tag"));
			assertTrue(value.length() > 4);

			//check count 
			assertTrue(cloningTool.checkCount(target,testContext));
			
			//check copied
			assertEquals("[]", target.execute(getQuery(testContext, "1")));
			assertEquals("[]", target.execute(getQuery(testContext, "2")));
			assertEquals("[]", target.execute(getQuery(testContext, "3")));
			assertEquals("[{\"O\":\"N2\"}]", target.execute(getQuery(testContext, "4")));			
		}
		
		@Test
		public void testCreateTag() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
			Method method = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("createTag"));
			method = cloningTool.getClass().getDeclaredMethod("createTag");
			method.setAccessible(true);
			method.invoke(cloningTool);
			
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String value = (String) field.get(cloningTool);
			assertTrue(value.contains("_Tag"));
			assertTrue(value.length() > 4);	//hash added to end of tag
		}
		
		
		@Test
		public void testCheckCount() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			TripleStoreClientInterface kbClient = createTestClient();
			
			assertFalse(cloningTool.checkCount(kbClient, null));
			
			//set countTotal variable
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("countTotal"));
			field = cloningTool.getClass().getDeclaredField("countTotal");
			field.setAccessible(true);
			field.set(cloningTool, 12);
		
			assertTrue(cloningTool.checkCount(kbClient, null));
		}
		
		@Test 
		public void testCheckTags() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
			TripleStoreClientInterface kbClient = createTestClient();
			String graph = null;
			
			assertTrue(cloningTool.checkNoTags(kbClient, graph));
			assertTrue(cloningTool.checkNoTags(kbClient, testContext));
			
			//Get tag
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String tag = (String) field.get(cloningTool);
			
			//Test model
			Model model = ModelFactory.createDefaultModel();	
			Node  s = ResourceFactory.createResource("http://example.com/S"+tag).asNode();
			Node  p = ResourceFactory.createResource("http://example.com/P").asNode();   
			Node  o = ResourceFactory.createResource("http://example.com/O").asNode();
			model.add( model.asStatement(new Triple(s,p,o)));
			UpdateBuilder builder = new UpdateBuilder().addInsert(model);
			
			kbClient.executeUpdate(builder.buildRequest());
			assertFalse(cloningTool.checkNoTags(kbClient, graph));			
		}
		
		//// Count methods
		
		@Test
		public void testCountTriples() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			TripleStoreClientInterface kbClient = createTestClient();
			
			Var[] sparqlArgs = getSparqlArgs();
			Var varS = sparqlArgs[0];
			Var varP = sparqlArgs[1];
			Var varO = sparqlArgs[2];
			
			//test arguments -- default graph
			WhereBuilder where = new WhereBuilder().addWhere(varS, varP, varO);
			String graph = null;
			int expectedValue = 12;
			
			Method method = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("countTriples", TripleStoreClientInterface.class, String.class, WhereBuilder.class));
			method = cloningTool.getClass().getDeclaredMethod("countTriples", TripleStoreClientInterface.class, String.class, WhereBuilder.class);
			method.setAccessible(true);
			int value = (int) method.invoke(cloningTool, kbClient, graph, where);
			assertEquals(expectedValue, value);
			
			//test argument -- graph
			graph = testContext;
			expectedValue = 6; 
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("countTriples", TripleStoreClientInterface.class, String.class, WhereBuilder.class));
			method = cloningTool.getClass().getDeclaredMethod("countTriples", TripleStoreClientInterface.class, String.class, WhereBuilder.class);
			method.setAccessible(true);
			value = (int) method.invoke(cloningTool, kbClient, graph, where);
			assertEquals(expectedValue, value);
		}
		
		@Test
		public void testCountQuery() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
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
			assertEquals(CloningToolTestHelper.removeWhiteSpace(expectedValue), CloningToolTestHelper.removeWhiteSpace(value.toString()));
			
			//test argument -- graph
			graph = "http://example.com";
			
			expectedValue = "SELECT (COUNT(*) AS ?count) WHERE\n"+
					"  { GRAPH <"+graph+">\n"+
				    "      { ?s  ?p  ?o }\n"+
		  			"  }\n";
			value = (String) method.invoke(cloningTool, graph, where);
			assertEquals(CloningToolTestHelper.removeWhiteSpace(expectedValue), CloningToolTestHelper.removeWhiteSpace(value.toString()));
		}
		
		//// Sparql builder methods
		
		@Test
		public void testBuildConstruct() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
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
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
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
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
			Var[] sparqlArgs = getSparqlArgs();
			Var varS = sparqlArgs[0];
			Var varP = sparqlArgs[1];
			Var varO = sparqlArgs[2];
			
			//test arguments -- default graph in triples mode
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
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildTagUpdate", String.class, WhereBuilder.class, int.class, boolean.class));
			method = cloningTool.getClass().getDeclaredMethod("buildTagUpdate", String.class, WhereBuilder.class, int.class, boolean.class);
			method.setAccessible(true);
			UpdateRequest value = (UpdateRequest) method.invoke(cloningTool, graph, where, limit, false);
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
					"        { GRAPH <"+graph+">\n"+
					"            { ?s  ?p  ?o}}\n"+
					"      LIMIT   "+limit+
					"\n    }\n  }\n";
			
			UpdateRequest value2 = (UpdateRequest) method.invoke(cloningTool, graph, where, limit, true);
			assertEquals(expectedValue, value2.toString());
			
			//test arguments -- default graph in quads mode 
			expectedValue = "DELETE {\n"+
					"  GRAPH ?g {\n"+
					"    ?s ?p ?o .\n"+
					"  }\n}\n"+
					"INSERT {\n"+
					"  GRAPH ?g {\n"+
					"    ?newS ?p ?o .\n"+
					"  }\n}\n"+
					"WHERE\n"+
					"  { { SELECT  ?s ?p ?o ?newS ?g\n"+
					"      WHERE\n"+
					"        { GRAPH ?g\n"+
					"            { ?s  ?p  ?o}}\n"+
					"      LIMIT   "+limit+
					"\n    }\n  }\n";
			
			graph = null;
			UpdateRequest value3 = (UpdateRequest) method.invoke(cloningTool, graph, where, limit, true);
			assertEquals(expectedValue, value3.toString());
			
		}
		
		//// Test filter expressions
		
		@Test
		public void testExpressions() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException, NoSuchFieldException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
			String expectedTag = "_Tag";
			
			Field field = null;
			assertNotNull(cloningTool.getClass().getDeclaredField("strTag"));
			field = cloningTool.getClass().getDeclaredField("strTag");
			field.setAccessible(true);
			String tagValue = (String) field.get(cloningTool);
			assertTrue(tagValue.startsWith(expectedTag));
			
			Method method = null;
			Expr value = null;
			assertNotNull(cloningTool.getClass().getDeclaredMethod("buildExprTagN", int.class));
			method = cloningTool.getClass().getDeclaredMethod("buildExprTagN", int.class);
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool, 1);
			String strValue = value.toString();
			assertTrue(strValue.startsWith("\"_1"+expectedTag));
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprFilterOutBlanks"));
			method = cloningTool.getClass().getDeclaredMethod("exprFilterOutBlanks");
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool);
			assertEquals("(&& (! (isBlank ?s)) (&& (! (isBlank ?p)) (! (isBlank ?o))))", value.toString());
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprTagged"));
			method = cloningTool.getClass().getDeclaredMethod("exprTagged");
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool);
			strValue = value.toString();
			assertTrue(strValue.startsWith("(strends (str ?s) \""+expectedTag));
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprNotTagged"));
			method = cloningTool.getClass().getDeclaredMethod("exprNotTagged");
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool);
			strValue = value.toString();
			assertTrue(strValue.startsWith("(! (strends (str ?s) \""+expectedTag));
			
			assertNotNull(cloningTool.getClass().getDeclaredMethod("exprBindIriRemoveTag", Expr.class));
			method = cloningTool.getClass().getDeclaredMethod("exprBindIriRemoveTag", Expr.class);
			method.setAccessible(true);
			value = (Expr) method.invoke(cloningTool, (Expr) null);
			assertEquals("(iri (replace (str ?s) NONE \"\"))", value.toString());
		}
				
		///// Helper functions
		
		/**
		 * Create a test model. 
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
		
		private TripleStoreClientInterface createTestClient() {
			FileBasedStoreClient kbClient = new FileBasedStoreClient();
			kbClient.load(filePath1);
			kbClient.load(testContext, filePath2);			
			return kbClient;
		}
		
		/**
		 * Get test query.
		 */
		private String getQuery(String graph, String species) {
				
			String G = "<" + graph + ">";
			String S = "<http://www.theworldavatar.com/kb/species/species.owl#species_"+ species +">";
			Var O = Var.alloc("O");
			
			SelectBuilder builder = new SelectBuilder()
					.addVar(O);
			if(graph == null) {
				builder.addWhere(S,"<http://www.w3.org/2008/05/skos#altLabel>",O);
			}else {
				builder.addGraph(G, S,"<http://www.w3.org/2008/05/skos#altLabel>",O);
			}					
			return builder.build().toString();
		}

		/**'
		 * Get SPARQL ?s ?p ?o variables 
		 */
		private Var[] getSparqlArgs() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
			
			SourceTaggingCloningTool cloningTool = new SourceTaggingCloningTool();
			
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
		
}
