package uk.ac.cam.cares.jps.base.query;

import java.util.Iterator;

import org.apache.jena.arq.querybuilder.UpdateBuilder;

/**
 * MockStoreClient is based on the in-memory LocalStoreClient class.
 * This class is designed as a mock StoreClientInterface type 
 * object to avoid the need to connect to a file-based or remote store client. 
 * <p>
 * An example use of this class is in testing an agent or class that interacts 
 * with a StoreClientInterface type object. Passing a MockStoreClient (possibly 
 * loaded with test data using the {@link uk.ac.cam.cares.jps.base.query.MockStoreClient#load load} 
 * and {@link uk.ac.cam.cares.jps.base.query.MockStoreClient#addTriple addTriple} methods) 
 * will permit testing of different interactions with the StoreClientInterface without 
 * writing files to disk or having to set-up a test remote triple store.
 * 
 * @author csl37
 *
 */
public class MockStoreClient extends LocalStoreClient {
	
	public MockStoreClient() {
		super();
	}
	
	/**
	 * Load file into dataset
	 * @param path
	 */
	public void load(String path) {
		conn.load(path);
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
	 * Add a single quad to the store
	 * @param graph
	 * @param subject
	 * @param predicate
	 * @param object
	 */
	public void addQuad(String graph, String s, String p, String o) {
		
		UpdateBuilder builder = new UpdateBuilder();
		builder.addInsert(graph, s, p, o);
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
