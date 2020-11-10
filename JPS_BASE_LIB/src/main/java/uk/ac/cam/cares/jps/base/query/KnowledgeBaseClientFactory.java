package uk.ac.cam.cares.jps.base.query;

/*
 * Class to create knowledgebaseclients
 */
public class KnowledgeBaseClientFactory {

	public static KnowledgeBaseClient create(String url) {
		
		if(isRemote(url)) {
			return new RemoteKnowledgeBaseClient(url);
		} else {
			return new FileBasedKnowledgeBaseClient(url);
		}
	}
	
	public static KnowledgeBaseClient create(String queryEndpoint, String updateEndpoint) {
		
		if(isRemote(queryEndpoint) && isRemote(updateEndpoint)) {
			return new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		} else {
			return new FileBasedKnowledgeBaseClient(queryEndpoint);
		}
	}
	
	public static KnowledgeBaseClient create(String queryEndpoint, String updateEndpoint, String query) {
		
		if(isRemote(queryEndpoint) && isRemote(updateEndpoint)) {
			return new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint, query);
		} else {
			if(queryEndpoint == null) {
				return new FileBasedKnowledgeBaseClient(updateEndpoint, query);
				//return new FileBasedKnowledgeBaseClientONTAPI(updateEndpoint, query);
			}else {
				return new FileBasedKnowledgeBaseClient(queryEndpoint, query);
				//return new FileBasedKnowledgeBaseClientONTAPI(queryEndpoint, query);
			}
		}
	}
	
	
	private static boolean isRemote(String url) {
		if (url == null) {
			return false;
		}else {
			return url.startsWith("http:");
		}
	}
}
