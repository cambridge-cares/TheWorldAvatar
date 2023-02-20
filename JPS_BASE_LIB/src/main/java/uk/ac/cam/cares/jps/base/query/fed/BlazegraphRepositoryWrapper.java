package uk.ac.cam.cares.jps.base.query.fed;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.apache.http.client.methods.HttpGet;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.openrdf.query.BindingSet;
import org.openrdf.query.TupleQueryResult;

import com.bigdata.rdf.rio.json.BigdataSPARQLResultsJSONWriter;
import com.bigdata.rdf.sail.webapp.client.IPreparedTupleQuery;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

/**
 * The class wraps some functionality of Blazegraph server such as executing SPARQL queries, 
 * creating new namespaces, and querying all existing namespaces.
 * Blazegraph's term "namespace" means repository or RDF dataset. 
 * Blazegraph's service URL has usually the form {@link http://www.theworldavatar.com/blazegraph}.
 * The endpoint URL is used for SPARQL queries on a dataset (namespace) and has the form e.g. <br>
 * <br>
 * {@link http://www.theworldavatar.com/blazegraph/namespace/ontocompchem/sparql}<br>
 * <br>
 * For more information about Blazegraph's REST API see {@link https://github.com/blazegraph/database/wiki/REST_API} .
 */
public class BlazegraphRepositoryWrapper {
	
	static final Logger LOGGER = LogManager.getLogger(BlazegraphRepositoryWrapper.class);
	public static final String BLAZEGRAPH_DEFAULT_NAMESPACE = "kb";
	
	private String defaultNamespace = null;
	private RemoteRepositoryManager manager = null;
	
	public BlazegraphRepositoryWrapper(String serviceUrl, String defaultNamespace) {
		this.defaultNamespace = defaultNamespace;
		this.manager = new RemoteRepositoryManager(serviceUrl);
		LOGGER.info("initialized with serviceUrl=" + serviceUrl + ", defaultNamespace=" + defaultNamespace);
	}
	
	public BlazegraphRepositoryWrapper(String serviceOrEndpointUrl) {
		this(getParts(serviceOrEndpointUrl)[0], getParts(serviceOrEndpointUrl)[1]);
	}
	
	public RemoteRepositoryManager getManager() {
		return manager;
	}
	
	/**
	 * If the endpoint URL is given, the pair [service-url, namespace] is returned.
	 * If the service URL is given, the pair [service-url, null] is returned. 
	 * 
	 * @param serviceOrEndpointUrl
	 * @return
	 */
	public static String[] getParts(String serviceOrEndpointUrl) {
		String serviceUrl = serviceOrEndpointUrl;
		String defaultNamespace = null; 
		int i1 = serviceUrl.lastIndexOf("/namespace/");
		int i2 = serviceUrl.lastIndexOf("/sparql");
		if (i1 >= 0 && i2 >= 0) {
			defaultNamespace = serviceUrl.substring(i1 + "/namespace/".length(), i2);
			serviceUrl = serviceUrl.substring(0, i1);
		} 
		
		return new String[] {serviceUrl, defaultNamespace};
	}
	
	public static String getPathForBlazegraph(String namespace) {
		return "/namespace/" + namespace + "/sparql";
	}
	
	/**
	 * see {@link com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager#getRepositoryProperties(String)}
	 * 
	 * @param namespace
	 * @return
	 */
	public Properties getRepositoryProperties(String namespace) {
		try {
			return manager.getRepositoryProperties(namespace);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	private RemoteRepository getRepository(String namespace) {
		try {
			return manager.getRepositoryForNamespace(namespace);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	/**
	 * Creates a new dataset (namespace). If props is null, the properties
	 * from the default namespace "kb" are copied. 
	 * If the default namespace does not exist, an exception is thrown. 
	 * 
	 * @param namespace the dataset name
	 * @param props	the properties for creating the journal file, BTree etc.
	 */
	public void createNamespace(String namespace, Properties props) {
		
		if (props == null) {
			// use standard properties from default namespace kb for the new namespace
			// however, the default namespace kb may not exist
			props = getRepositoryProperties(BLAZEGRAPH_DEFAULT_NAMESPACE);
		}
		try {
			this.manager.createRepository(namespace, props);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	/**
	 * Executes the SPARQL query on the given namespace (dataset). 
	 * The result set is a string in the same JSON format as in {@link JenaResultSetFormatter#convertToSimplifiedList(ResultSet)}
	 * 
	 * @param sparql
	 * @param namespace
	 * @return
	 */
	public String query(String sparql, String namespace) {	
				
		if (namespace == null) {
			namespace = defaultNamespace;
			if (namespace == null) {
				throw new JPSRuntimeException("namespace and default namespace are null");
			}
		}
		
		String resultAsJsonString = null;
		try {
			RemoteRepository repo = getRepository(namespace);
			IPreparedTupleQuery query = repo.prepareTupleQuery(sparql);
			TupleQueryResult result = query.evaluate();
		
			try (OutputStream out = new ByteArrayOutputStream()) {
	
				BigdataSPARQLResultsJSONWriter writer = new BigdataSPARQLResultsJSONWriter(out);
				List<String> columnHeaders = new LinkedList<String>();
		        columnHeaders.addAll(result.getBindingNames());
		        writer.startQueryResult(columnHeaders);
				
				while (result.hasNext()) {
					BindingSet b = result.next();
		            writer.handleSolution(b);
				}
				
				writer.endQueryResult();
				resultAsJsonString = out.toString();
				
				result.close();
			}
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return JenaResultSetFormatter.convertToSimplifiedJsonArray(resultAsJsonString).toString();
	}
	
	/**
	 * Returns an RDF graph serialized in RDF+XML format. 
	 * The RDF graph contains information about all namespaces (datasets) available at the Blazegraph server. 
	 * 
	 * @return RDF graph in RDF+XML format
	 */
	private String queryNamespacesFull() {
		String url = manager.getBaseServiceURL() + "/namespace";
		LOGGER.debug("querying namespaces: " + url);	
		// String accept = "application/x-turtle";
		String accept = "application/rdf+xml";
		HttpGet request = Http.get(url, accept);
		return Http.execute(request);
	}
	
	/**
	 * Returns the list of namespaces (datasets) available at the Blazegraph server.
	 * 
	 * @param asEndpointUrl if true returns the full endpoint URL for each namespace otherwise just the namespace
	 * @return
	 */
	public List<String> queryNamespaces(boolean asEndpointUrl) {
		String full = queryNamespacesFull();
		return queryNamespaces(asEndpointUrl, full);
	}
		
	public List<String> queryNamespaces(boolean asEndpointUrl, String rdfgraphWithNamespaces) {
		OntModel model = JenaHelper.createModel();
		JenaHelper.readFromString(rdfgraphWithNamespaces, model);
		String sparql = "SELECT ?endpoint WHERE { "
				+ "?dataset a <http://rdfs.org/ns/void#Dataset> . "
				+ "?dataset <http://rdfs.org/ns/void#sparqlEndpoint> ?endpoint . }";
		ResultSet result = JenaHelper.query(model, sparql);
		String json = JenaResultSetFormatter.convertToJSONW3CStandard(result);	
		Set<String> endpoints = new HashSet<String>();
		for (String[] row : JenaResultSetFormatter.convertToListofStringArrays(json, "endpoint")) {
			String namespace = row[0];
			if (!asEndpointUrl) {
				namespace = namespace.replaceFirst("/sparql", "");
				int i = namespace.lastIndexOf("/");
				namespace = namespace.substring(i+1);
			} else {
				// CityKG does not seem to be configured correctly 
				// the endpoint URLS contain localhost:9999
				// we replace this here
				for (String s : new String[] {"localhost:9999/blazegraph", "localhost:9999/bigdata/LBS"}) {
					if (namespace.contains(s)) {
						String old = namespace;
						namespace = namespace.replace(s, "www.theworldavatar.com:83/citieskg");
						LOGGER.warn("changed endpoint url from " + old + " to " + namespace);
					}
				}

			}
			endpoints.add(namespace);
		}
		return new ArrayList<String>(endpoints);
	}
	
	/**
	 * Closes the repository manager.
	 */
	public void close() {
		try {
			manager.close();
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} finally {
			manager = null;
		}
	}
}
