package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;

/**
 * Mock store client based on an in-memory LocalStoreClient.
 * This class is designed as a mock StoreClientInterface type 
 * object for testing purposes. 
 * @author csl37
 *
 */
public class MockStoreClient extends LocalStoreClient {
	
	public MockStoreClient() {
		super();
	}
	
	/**
	 * Add a single triple to the store
	 * @param subject
	 * @param predicate
	 * @param object
	 */
	public void addTriple(String s, String p, String o) {
		
		UpdateBuilder builder = new UpdateBuilder();
		builder.addInsert(s, p, o);
		executeUpdate(builder.buildRequest());	
	}

}
