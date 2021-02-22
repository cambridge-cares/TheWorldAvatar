package uk.ac.cam.cares.jps.base.query;

import java.io.File;


import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;

public class KGRouter{
	public static final String HTTP="http://";
	public static final String HTTPS="https://";
	public static final String EMPTY = "";
	private static final String KGROUTER_ENDPOINT = "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql";
	public static final String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	public static final String ONTOKGROUTER = "http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#";
	static KGRouter kgRouter = null;
	
	/**
	 * Based on a target resource IRI or path provided as the input, it returns the<br>
	 * corresponding KnowledgeBaseClient. It supports two types of resources:<br>
	 * a) a repository/namespace and b) an ontology file. Some examples of these<br>
	 * resources are provided below:<br>
	 * a) Example repositories/namespaces are:<br>
	 *    - http://ontokin
	 *    - http://ontospecies
	 *    - http://ontocompchem
	 * b) Example ontology files are:<br>
	 *    - C://path/to/an/abox.owl (On Windows)
	 *    - /home/path/to/an/abox.owl (On Linux)
	 * 
	 * @param targetResourceIRIOrPath the IRI of an RDF/OWL repository/namespace or the path to an RDF/OWL file. 
	 * @param isQueryOperation true/false
	 * @param isUpdateOperation true/false
	 * @return
	 */
	public static KnowledgeBaseClient getKnowledgeBaseClient(String targetResourceIRIOrPath, boolean isQueryOperation, boolean isUpdateOperation) throws Exception{
		String queryIRI = null;
		String updateIRI = null;
		KnowledgeBaseClient kbClient = null;
		if (targetResourceIRIOrPath != null && !targetResourceIRIOrPath.isEmpty()) {
			if (targetResourceIRIOrPath.trim().startsWith(HTTP)) {
				if (kgRouter == null) {
					kgRouter = new KGRouter();
				}
				if (isQueryOperation) {
//					queryIRI = kgRouter.getQueryIRI(KGROUTER_ENDPOINT, targetResourceIRIOrPath.replace(HTTP, EMPTY));
				}
				if (isUpdateOperation) {
					updateIRI = kgRouter.getUpdateIRI(KGROUTER_ENDPOINT, targetResourceIRIOrPath.replace(HTTP, EMPTY));
				}
				if (queryIRI != null && !queryIRI.isEmpty()) {
					kbClient = new RemoteKnowledgeBaseClient(queryIRI);
				}
				if (updateIRI != null && !updateIRI.isEmpty()) {
					if (kbClient == null) {
						kbClient = new RemoteKnowledgeBaseClient();
					}
					kbClient.setUpdateEndpoint(updateIRI);
				}
			}else{
				File file = new File(targetResourceIRIOrPath);
				if(file.exists()){
					kbClient = new FileBasedKnowledgeBaseClient(targetResourceIRIOrPath);
				}
			}
		}
		return kbClient;
	}

	
	public static void main(String[] args) throws Exception{
		KGRouter kgRouter = new KGRouter();
//		System.out.println(kgRouter.getQueryIRI(null, null));
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
