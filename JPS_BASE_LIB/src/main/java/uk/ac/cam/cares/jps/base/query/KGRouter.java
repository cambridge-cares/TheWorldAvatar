package uk.ac.cam.cares.jps.base.query;

public class KGRouter{
	public static final String HTTP="http://";
	public static final String HTTPS="https://";
	public static final String EMPTY = "";
	private static final String KGROUTER_ENDPOINT = "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql";
	static KGRouter kgRouter = null;
	
	/**
	 * Based on a target resource provided as the input, it returns the<br>
	 * corresponding KnowledgeBaseClient. It supports two types of resources:<br>
	 * a) repositories/namespaces and b) ontology files. Some examples of these<br>
	 * resources are provided below:<br>
	 * a) some example repositories/namespaces are:<br>
	 *    - http://ontokin
	 *    - http://ontospecies
	 *    - http://ontocompchem
	 * b) some example ontology files are:<br>
	 *    - C://path/to/an/abox.owl (On Windows)
	 *    - /home/path/to/an/abox.owl (On Linux)
	 * 
	 * @param targetResource
	 * @return
	 */
	public static KnowledgeBaseClient getKnowledgeBaseClient(String targetResource, boolean isQueryOperation, boolean isUpdateOperation){
		String queryIRI = null;
		String updateIRI = null;
		if(targetResource!=null && (targetResource.trim().startsWith(HTTP))){
			if(kgRouter==null){
				kgRouter = new KGRouter();
			}
			if(isQueryOperation){
				queryIRI = kgRouter.getQueryIRI(KGROUTER_ENDPOINT, targetResource.replace(HTTP, EMPTY));
			}
			if(isUpdateOperation){
				updateIRI = kgRouter.getUpdateIRI(KGROUTER_ENDPOINT, targetResource.replace(HTTP, EMPTY));
			}
		}
		return null;
	}
	
	/**
	 * Retrieves the query IRI of the target repository/namespace. 
	 * 
	 * @param kgrouterEndpoint
	 * @param targetResourceName
	 * @param isQueryOperation
	 * @param isUpdateOperation
	 * @return
	 */
	private String getQueryIRI(String kgrouterEndpoint, String targetResourceName){
		
		return null;
	}
	
	/**
	 * Retrieves the update IRI of the target repository/namespace.
	 * 
	 * @param kgrouterEndpoint
	 * @param targetResourceName
	 * @return
	 */
	private String getUpdateIRI(String kgrouterEndpoint, String targetResourceName){
	
		return null;
	}
	
}
