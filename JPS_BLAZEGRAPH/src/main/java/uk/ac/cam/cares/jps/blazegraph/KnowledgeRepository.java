package uk.ac.cam.cares.jps.blazegraph;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.logging.Logger;

import org.apache.log4j.spi.LoggerFactory;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.GraphQueryResult;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.rio.RDFFormat;

import com.bigdata.rdf.sail.webapp.SD;
import com.bigdata.rdf.sail.webapp.client.IPreparedTupleQuery;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository.AddOp;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;

import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;

/**
 * This class has been designed and developed to enable performing CRUD<br>
 * operations on any knowledge-graph built using Blazegraph.  
 * 
 * @author msff2
 *
 */
public class KnowledgeRepository {
	Logger log = Logger.getLogger(KnowledgeRepository.class.getName());
	
	private String endPointURL;
	private String repositoryName;
	private RDFStoreType storeType;
	private String ontologyFilePath;
	private String ontologyDirectory;
	private String query;
	
	/**
	 * Creates the instance of the current repository (knowledge base) if it<br>
	 * exists and returns it.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param storeType the name of knowledge storage, e.g. Blazegraph and RDF4J.
	 */
	public RemoteRepository getRepository(String endPointURL, String repositoryName, RDFStoreType storeType) throws Exception{
		RemoteRepository repository = null;
		if(storeType.toString().equals(RDFStoreType.BLAZEGRAPH.toString())){
			RemoteRepositoryManager repositoryManager = new RemoteRepositoryManager(endPointURL, false);
			if(repositoryExists(endPointURL, repositoryName, repositoryManager)){
				repository = repositoryManager.getRepositoryForNamespace(repositoryName);
				repositoryManager.close();
				return repository; 
			}
		}
		return repository;
	}
	
	/**
	 * Checks the availability of a repository (knowledge base) on a triple store.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param repositoryManager an instance of the repository manager.
	 * @return
	 * @throws Exception
	 */
	private boolean repositoryExists(String endPointURL, String repositoryName, RemoteRepositoryManager repositoryManager) throws Exception{
		final GraphQueryResult res = repositoryManager.getRepositoryDescriptions();
		try{
			while(res.hasNext()){
				final Statement stmt = res.next();
				if (stmt.getPredicate().toString().equals(SD.KB_NAMESPACE.stringValue())) {
					if(repositoryName.equals(stmt.getObject().stringValue())){
						return true;
					}
				}
			}
		} finally {
			res.close();
		}
		return false;
	}
	
	/**
	 * Uploads a single ontology file to the current repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param ontologyFilePath the absolute path to the ontology file, e.g. 
	 * C:/path/to/the/ontology/ontokin.owl and C:/path/to/the/ontology/ABF.owl.
	 * @throws Exception
	 */
	public void uploadOntology(String endPointURL, String repositoryName, String ontologyFilePath)
			throws Exception {
		RemoteRepository repository = getRepository(endPointURL, repositoryName,
				RDFStoreType.BLAZEGRAPH);
		if (repository != null) {
			final InputStream is = new FileInputStream(new File(ontologyFilePath));
			try {
				repository.add(new AddOp(is, RDFFormat.forMIMEType("application/xml")));
			} finally {
				is.close();
			}
		} else{
			log.info("The following repository does not exist: "+endPointURL+repositoryName);
			log.info("Create a repository with this name and try again.");
		}
	}
	
	/**
	 * Uploads all ontology files available under the given folder to<br>
	 * the current repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param ontologyDirectory the path to the folder containing a list<br>
	 * of ontologies, e.g. C:/path/to/the/ontology_folder.
	 * @throws Exception
	 */
	public void uploadOntologies(String endPointURL, String repositoryName, String ontologyDirectory) throws Exception{
		File dir = new File(ontologyDirectory);
		if(dir.isDirectory()){
			int i = 0;
			for(File file:dir.listFiles()){
				if(file.isFile()){
					uploadOntology(endPointURL, repositoryName, file.getAbsolutePath());
					log.info("["+ ++i+"] Uploaded "+file.getAbsolutePath());
				}
			}
		}
	}

	/**
	 * Performs any SPARQL query against the provided repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param storeType the name of knowledge storage, e.g. Blazegraph and RDF4J.
	 * @param query the query that is being performed.
	 * @return
	 * @throws Exception
	 */
	public String query(String endPointURL, String repositoryName, RDFStoreType storeType, String query) throws Exception {
		StringBuilder json = new StringBuilder();
		RemoteRepository repository = getRepository(endPointURL, repositoryName, storeType);
		final IPreparedTupleQuery tupleQuery = repository.prepareTupleQuery(query);
		final TupleQueryResult result = tupleQuery.evaluate();
		System.out.println(result);
		try {
			json = getResultInJson(json, result);	
		} finally {
			result.close();
		}
		return json.toString();
	}

	/**
	 * Produces and returns the given result in JSON format.
	 * 
	 * @param json
	 * @param result
	 * @return
	 */
	private StringBuilder getResultInJson(StringBuilder json, TupleQueryResult result) {
		json.append("{\n");
		json.append("  \"head\" : {\n");
		json.append("    \"vars\" : [\n");
		try{
		// flag to close the header variables created above and to start the results
		boolean flag = true; 
		// we just iterate over all solutions in the result...
		while (result.hasNext()) {
			BindingSet solution = result.next();
			int count = 0;
			int size = solution.getBindingNames().size();
			if(flag){
				for(String bindingName: solution.getBindingNames()){
					json.append("      \"");
					json.append(bindingName);
					json.append("\"");
					if(++count<size){
						json.append(",");
					}
					json.append("\n");
				}
				json.append("    ]\n");
				json.append("  },\n");
				json.append("  \"results\" : {\n");
				json.append("    \"bindings\" : [\n");
				flag = false;
			}
			count = 0;
			json.append("      {\n");
			for (String bindingName : solution.getBindingNames()) {
				json.append("        \"");
				json.append(bindingName);
				json.append("\" : {\n");
				json.append("          \"value\" : ");
				json.append(jsonifyString(solution.getValue(bindingName).toString()));
				json.append("\n        }");
				if(++count<size){
					json.append(",\n");
				}else{
					json.append("\n");
				}
			}
			json.append("      },\n");
		}
		json.replace(json.lastIndexOf(","), json.lastIndexOf(",")+1, "");
		}catch(QueryEvaluationException e){
			log.info(e.getMessage());
		}
		json.append("    ]\n");
		json.append("  }\n");
		json.append("}\n");
		return json;
	}
	
	/**
	 * Converts a value string into its JSON equivalent.</br>
	 * However, currently it cannot produce a valid JSON equivalent</br>
	 * for comments.
	 * 
	 * @param value
	 * @return
	 */
	private String jsonifyString(String value){
		String stringType = "^^<http://www.w3.org/2001/XMLSchema#string>";
		String integerType = "^^<http://www.w3.org/2001/XMLSchema#integer>";
		String floatType = "^^<http://www.w3.org/2001/XMLSchema#float>";
		if(value.contains(stringType)){
			value = value.replace(stringType, "");
		} else if(value.contains(integerType)){
			value = value.replace(integerType, "");
			value = replaceInvertedComma(value);
		} else if(value.contains(floatType)){
			value = value.replace(floatType, "");
			value = replaceInvertedComma(value);
		} else {
			value = "\""+value+"\"";
		}
		return value;
	}
	
	/**
	 * Removes the start and end inverted commas from a string.
	 * 
	 * @param value
	 * @param type
	 * @return
	 */
	private String replaceInvertedComma(String value){
		if(value.startsWith("\"")){
			value = value.replaceFirst("\"", "");
		}
		if(value.endsWith("\"")){
			value = value.substring(0, value.length()-1);
		}		
		return value;
	}

	public String getEndPointURL() {
		return endPointURL;
	}

	public void setEndPointURL(String endPointURL) {
		this.endPointURL = endPointURL;
	}

	public String getRepositoryName() {
		return repositoryName;
	}

	public void setRepositoryName(String repositoryName) {
		this.repositoryName = repositoryName;
	}

	public RDFStoreType getStoreType() {
		return storeType;
	}

	public void setStoreType(RDFStoreType storeType) {
		this.storeType = storeType;
	}

	public String getOntologyFilePath() {
		return ontologyFilePath;
	}

	public void setOntologyFilePath(String ontologyFilePath) {
		this.ontologyFilePath = ontologyFilePath;
	}

	public String getOntologyDirectory() {
		return ontologyDirectory;
	}

	public void setOntologyDirectory(String ontologyDirectory) {
		this.ontologyDirectory = ontologyDirectory;
	}

	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
	
	
}
