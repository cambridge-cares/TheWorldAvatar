package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.arq.querybuilder.UpdateBuilder;

public class MockStoreClient extends LocalStoreClient {
	
	public MockStoreClient() {
		super();
	}
	
	public void addTriple(String s, String p, String o) {
		UpdateBuilder builder = new UpdateBuilder();
		builder.addInsert(s, p, o);
		executeUpdate(builder.buildRequest());	
	}

}
