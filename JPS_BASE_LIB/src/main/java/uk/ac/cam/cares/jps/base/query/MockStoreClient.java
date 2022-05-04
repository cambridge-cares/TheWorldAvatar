package uk.ac.cam.cares.jps.base.query;

import java.util.Iterator;

import org.apache.jena.arq.querybuilder.UpdateBuilder;

/**
 * MockStoreClient based on the in-memory LocalStoreClient class.
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
		
	/**
	 * Clear the mock dataset
	 */
	public void clear() {
		
		conn.delete();
		Iterator<String> it = dataset.listNames();
		while(it.hasNext()) {
			conn.delete(it.next());
		}
	}
}
