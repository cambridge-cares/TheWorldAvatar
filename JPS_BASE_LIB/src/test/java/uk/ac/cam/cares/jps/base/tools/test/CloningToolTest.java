package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.sparql.core.Var;
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
