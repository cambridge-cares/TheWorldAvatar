package uk.ac.cam.cares.jps.accessagent.integrationtest;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

import uk.ac.cam.cares.jps.base.query.StoreRouter;

/**
 * Contains shared methods for Access Agent integration tests
 * @author csl37
 */
public class TestHelper {

	/**
	 * Get the OntoKGRouter endpoint from the StoreRouter class
	 * @return
	 */
	public static String getRouterEndpoint() {
		StoreRouter router = new StoreRouter();
		Field field;
		String ontokgrouterEndpoint = null;
		try {
			assertNotNull(router.getClass().getDeclaredField("STOREROUTER_ENDPOINT"));
			field = router.getClass().getDeclaredField("STOREROUTER_ENDPOINT");
			field.setAccessible(true);
			ontokgrouterEndpoint = (String) field.get(null);
		} catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
			fail("Failed to get OntoKGRouter endpoint!");
			e.printStackTrace();
		}	
		return ontokgrouterEndpoint;
	}
	
	/**
	* Remove all white spaces and non-visible characters
	* @param str
	* @return
	*/
	public static String removeWhiteSpace(String string) {
		return string.replaceAll("\\s+","");
	}
	
	/**
	* Returns the test Sparql update.
	* 
	* @return UpdateRequest
	* @throws ParseException
	*/
	public static String getUpdateRequest() throws ParseException {
	
		//DELETE {?s ?p ?o} 
		//INSERT {?s ?p \"TEST\" } 
		//WHERE {?s ?p ?o.
		//		 FILTER(?s = <http://www.example.com/test/s> && ?p = <http://www.example.com/test/p>)}
		
		WhereBuilder where = new WhereBuilder()
				.addWhere("?s", "?p", "?o")
				.addFilter("?s = <http://www.example.com/test/s> && ?p = <http://www.example.com/test/p>");
		
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add where 
		builder.addInsert("?s", "?p", "TEST")
			.addDelete("?s", "?p", "?o")
			.addWhere(where);
		
		return builder.buildRequest().toString();
	}
}
