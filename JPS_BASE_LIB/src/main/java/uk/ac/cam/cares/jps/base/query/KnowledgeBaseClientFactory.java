package uk.ac.cam.cares.jps.base.query;

/*
 * Class to create knowledgebaseclients
 */
public class KnowledgeBaseClientFactory {

	public static KnowledgeBaseClient create(String url) {
		
		if(isRemote(url)) {
			return new RemoteKnowledgeBaseClient(url);
		} else {
			return new FileBasedKnowledgeBaseClientRDFConnection(url);
		}
	}
	
	public static KnowledgeBaseClient create(String queryEndpoint, String updateEndpoint) {
		
		if(isRemote(queryEndpoint) && isRemote(queryEndpoint)) {
			return new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		} else {
			return new FileBasedKnowledgeBaseClientRDFConnection(queryEndpoint);
		}
	}
	
	public static KnowledgeBaseClient create(String queryEndpoint, String updateEndpoint, String query) {
		
		if(isRemote(queryEndpoint) && isRemote(queryEndpoint)) {
			return new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint, query);
		} else {
			return new FileBasedKnowledgeBaseClientRDFConnection(queryEndpoint, query);
		}
	}
	
	
	private static boolean isRemote(String url) { 
		return url.startsWith("http:");
	}
}
