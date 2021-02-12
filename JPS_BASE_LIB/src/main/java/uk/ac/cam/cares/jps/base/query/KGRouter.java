package uk.ac.cam.cares.jps.base.query;

public class KGRouter{
	public static final String HTTP="http://";
	public static final String HTTPS="https://";
	
	/**
	 * Based on a target resource provided as the input, it returns the<br>
	 * corresponding KnowledgeBaseClient. It supports two types of resources:<br>
	 * a) repositories/namespaces and b) ontology files. Some examples of these<br>
	 * resources are provided below:<br>
	 * a) some example repositories/namespaces are:<br>
	 *    - ontokin
	 *    - ontospecies
	 *    - ontocompchem
	 * b) some example ontology files are:<br>
	 *    - C://path/to/an/abox.owl (On Windows)
	 *    - /home/path/to/an/abox.owl (On Linux)
	 * 
	 * @param targetResource
	 * @return
	 */
	public static KnowledgeBaseClient getKnowledgeBaseClient(String targetResource){
		if(targetResource!=null && (targetResource.trim().startsWith(HTTP) || targetResource.trim().startsWith(HTTPS))){
			
		}
		return null;
	}
}
