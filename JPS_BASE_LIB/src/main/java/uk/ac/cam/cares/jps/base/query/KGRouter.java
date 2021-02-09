package uk.ac.cam.cares.jps.base.query;

public class KGRouter{
	public static final String HTTP="http://";
	public static final String HTTPS="https://";
	
	public static KnowledgeBaseClient getKnowledgeBaseClient(String targetResource){
		if(targetResource!=null && (targetResource.trim().startsWith(HTTP) || targetResource.trim().startsWith(HTTPS))){
			
		}
		return null;
	}
}
