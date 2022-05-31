package uk.ac.cam.cares.jps.base.query.fed;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import org.apache.commons.io.FileUtils;
import org.apache.http.client.methods.HttpGet;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

/**
 * This class creates a schema-level index from  
 * <a href="https://www.w3.org/TR/2013/REC-sparql11-service-description-20130321/">service descriptions</a>.
 * A service description is an RDF graph containing information about a SPARQL service and its SPARQL endpoints.
 * Among other things, the RDF graph usually contains features of the SPARQL service 
 * and the number of occurrences of relations and classes in triples of the datasets.
 * This class uses the IRIs of relations and classes for indexing. 
 * Each relation and class IRI is mapped to a "postings" list of endpoints / datasets.
 * An endpoint / dataset is added to the "postings" list for an IRI if it contains at least 
 * one triple with this IRI as predicate or object, resp.
 * The created index allows to perform conjunctive keyword-based queries.
 * Since the keywords are relation and class IRIs, 
 * the query result can be used for automated endpoint selection.
 *
 */
public class ServiceDescriptionIndexer {
	
	static final Logger LOGGER = LogManager.getLogger(ServiceDescriptionIndexer.class);
	
	/**
	 * A helper class to store relevant information filtered from a service description.
	 *
	 */
	public class ServiceDescriptionSummary {
		public Integer id = null;
		public String path = null;
		public String endpointURL = null;
		public Long ntriples = null;
		public Long nentities = null;
		public Long nproperties = null;
		public Long nclasses = null;
		
		public ServiceDescriptionSummary(int id) {
			this.id = id;
		}
		
		@Override
		public String toString() {
			return "Summary[#t=" + ntriples + ",#e=" +nentities 
					+ ",#c=" + nclasses + ",#p=" + nproperties + ",url=" + endpointURL + "]";
		}
	}
	
	public class PostingsListElement implements Comparable<PostingsListElement>{
		/**
		 * ID of the summary of the corresponding service description
		 */
		public Integer id = null;
		/**
		 * The number of triples in the corresponding datasets wrt. to the index key (relation or class IRI)
		 */
		public Long tripleNumber = null;
		
		public PostingsListElement(Integer id, Long number) {
			this.id = id;
			this.tripleNumber = number;
		}

		/**
		 * Elements in the postings list are ordered by their ID.
		 */
		@Override
		public int compareTo(PostingsListElement element) {
			return this.id.compareTo(element.id);
		}
	}
	
	private final static String INDEX_NAME = "SCHEMA";
	/**
	 * Each summary in the list represents the relevant information of a service description
	 */
	private List<ServiceDescriptionSummary> summaries = new ArrayList<ServiceDescriptionIndexer.ServiceDescriptionSummary>();
	private SimpleMultiIndex<String, PostingsListElement> index = new SimpleMultiIndex<String, PostingsListElement>();
	
	public ServiceDescriptionIndexer() {
	}
	
	private Map<String, Set<PostingsListElement>> getSchemaIndex() {
		return index.getIndex(INDEX_NAME);
	}
	
	/**
	 * Returns all indexed relation and class IRIs
	 * 
	 * @return
	 */
	public Set<String> getKeys() {
		return getSchemaIndex().keySet();
	}
	
	public Set<PostingsListElement> getPostingsList(String key) {
		return index.getPostingsList(INDEX_NAME, key);
	}
	
	/**
	 * Returns a list of summaries for all added service descriptions.
	 * 
	 * @return
	 */
	public List<ServiceDescriptionSummary> getSummaries() {
		return summaries;
	}
	
	/**
	 * Creates a map between IRIs (classes or properties depending on the given type) 
	 * and the number of triples in the service description.
	 * 
	 * @param model contains the service description as RDF graph
	 * @param sparql to query the RDF graph
	 * @param type either class or property
	 * @return
	 */
	private static Map<String, Long> getNumberOfTriples(OntModel model, String sparql, String type) {
		ResultSet result = JenaHelper.query(model, sparql);
		JSONObject joOrig = JenaResultSetFormatter.convertToSimplifiedList(result);
		JSONArray list = joOrig.getJSONArray("results");

		Iterator<Object> it = list.iterator();
		Map<String, Long> map = new HashMap<String, Long>();
		while (it.hasNext()) {
			JSONObject current = (JSONObject) it.next();
			map.put(current.getString(type), current.getLong("ntriples"));
		}
		return map;
	}

	/**
	 * Queries the service description for the number of triples wrt. classes.
	 * 
	 * @param model contains the service description as RDF graph
	 * @return
	 */
	public static Map<String, Long> getNumberOfClassTriples(OntModel model) {
		String sparql = "SELECT * WHERE { \r\n"
				+ "	?s <http://rdfs.org/ns/void#class> ?class . \r\n"
				+ "	?s <http://rdfs.org/ns/void#triples> ?ntriples . \r\n"
				+ "}";
		return getNumberOfTriples(model, sparql, "class");
	}
	
	/**
	 * Queries the service description for the number of triples wrt. properties.
	 * 
	 * @param model contains the service description as RDF graph
	 * @return
	 */
	public static Map<String, Long> getNumberOfPropertyTriples(OntModel model) {
		String sparql = "SELECT * WHERE { \r\n"
				+ "	?s <http://rdfs.org/ns/void#property> ?property . \r\n"
				+ "	?s <http://rdfs.org/ns/void#triples> ?ntriples . \r\n"
				+ "}";
		return getNumberOfTriples(model, sparql, "property");
	}
	
	/**
	 * Adds new service descriptions for indexing. 
	 * Three different types are allowed: 
	 * <p>
	 * (1) A local file given by its path: 
	 * The file represents a (serialized) service description stored locally.<br>
	 * (2) A local directory given by its directory path:
	 * All files in the directory and subdirectories are read recursively. 
	 * Each file represents a (serialized) service description store locally.<br>
	 * (3) A URL that can be requested and returns service description 
	 * (usually, according to the
	 * <a href="https://www.w3.org/TR/2013/REC-sparql11-service-description-20130321/#accessing">
	 * SPARQL protocol</a>). 
	 * 
	 * @param dirFileorUrl a (mixed) list of directories, files or URLs
	 */
	public void addServiceDescription(String... dirFileorUrl) {
		
		for (String path : dirFileorUrl) {
			File file = new File(path);
			if (file.isDirectory()) {
				Collection<File> files = FileUtils.listFiles(file, null, true);
				for (File f : files) {
					add(f.getAbsolutePath());
				}
			} else {
				add(path);
			}
		}
	}
	
	/**
	 * Requests the service description from the given URL
	 * 
	 * @param url
	 * @return
	 */
	public static String queryServiceDescription(String url) {
		String accept = "application/rdf+xml";
		HttpGet request = Http.get(url, accept);
		return Http.execute(request);
	}
	
	/**
	 * Reads the file or request the URL, 
	 * queries relevant information from the corresponding service description,
	 * and adds it to index
	 * 
	 * @param path a file or URL
	 */
	private void add(String path) {
		
		OntModel model = null;
		if (path.startsWith("http")) {
			String descr = queryServiceDescription(path);
			model = JenaHelper.createModel();
			JenaHelper.readFromString(descr, model);
		} else {
			model = JenaHelper.createModel(path);
		}
			
		ServiceDescriptionSummary summary = createSummary();
		fillSummary(summary, path,  model);
		
		// add classes and their counts to index
		Map<String, Long> map = getNumberOfClassTriples(model);
		for (String key : map.keySet()) {
			PostingsListElement elem = new PostingsListElement(summary.id, map.get(key));
			index.add(INDEX_NAME, key, elem);
		}
		
		// add properties and their counts to index
		map = getNumberOfPropertyTriples(model);
		for (String key : map.keySet()) {
			PostingsListElement elem = new PostingsListElement(summary.id, map.get(key));
			index.add(INDEX_NAME, key, elem);
		}
		
		
		LOGGER.debug("Added service description=" + summary + " from path=" + path);
	}
	
	/**
	 * Queries the service description for information that is relevant for indexing
	 * and conjunctive queries and adds the information to the given summary.
	 * 
	 * @param summary empty summary to add relevant information
	 * @param path file or URL
	 * @param model contains the service description as RDF graph 
	 */
	public void fillSummary(ServiceDescriptionSummary summary, String path, OntModel model) {
		
		summary.path = path; 
		
		String sparql = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \r\n"
				+ "PREFIX sd: <http://www.w3.org/ns/sparql-service-description#>\r\n"
				+ "PREFIX void: <http://rdfs.org/ns/void#>\r\n"
				+ "SELECT * WHERE {\r\n"
				+ "  ?dataset rdf:type sd:Dataset .\r\n"
				+ "  ?dataset sd:defaultGraph ?graph .\r\n"
				+ "  ?graph void:triples ?ntriples .\r\n"
				+ "  ?graph void:entities ?nentities .\r\n"
				+ "  ?graph void:properties ?nproperties .\r\n"
				+ "  ?graph void:classes ?nclasses .\r\n"
				+ "}";

		ResultSet result = JenaHelper.query(model, sparql);
		JSONObject jo = JenaResultSetFormatter.convertToSimplifiedList(result);
		JSONArray list = jo.getJSONArray("results");
		
		Iterator<Object> it = list.iterator();
		JSONObject current = (JSONObject) it.next();
		if (it.hasNext()) {
			throw new JPSRuntimeException("The query result set contains more than one row, service description=" + path);
		}
		
		
		
		// the following "lambda method" get returns null if the key is not found
		// in contrast to current.get which throws an exception in this case
		Function<String, Long> get = ( s -> { 
			if (current.has(s)) 
				{return current.getLong(s); }; 
			return null; 
		}); 
		
		summary.ntriples = get.apply("ntriples");
		summary.nentities = get.apply("nentities");
		summary.nproperties = get.apply("nproperties");
		summary.nclasses = get.apply("nclasses");
		
		
		// query endpoint URL
		sparql = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \r\n"
				+ "PREFIX sd: <http://www.w3.org/ns/sparql-service-description#>\r\n"
				+ "PREFIX void: <http://rdfs.org/ns/void#>\r\n"
				+ "SELECT * WHERE {\r\n"
				+ "  ?service sd:endpoint ?endpoint .\r\n"
				+ "}";
		
		result = JenaHelper.query(model, sparql);
		QuerySolution s = result.nextSolution();
		String endpointURL = s.get("endpoint").asResource().getURI();
		// HACK: The Blazegraph instance for citiesKG does not provide the correct endpoint URL 
		// e.g. the port in http://localhost:9999/blazegraph/namespace/singaporeEPSG24500/sparql is incorrect
		// maybe it is not configured correctly
		if (endpointURL.startsWith("http://localhost:9999")) {
			int i = endpointURL.indexOf("namespace/");
			String namespace = endpointURL.substring(i, endpointURL.length());
			summary.endpointURL = "http://www.theworldavatar.com:83/citieskg/" + namespace;
			LOGGER.info("changed endpoint URL from " + endpointURL + " to " + summary.endpointURL);
		} else {
			summary.endpointURL = endpointURL;
		}
	}
	
	/**
	 * Creates a new empty summary with an increased id and adds it to the list of all summaries.
	 * 
	 * @return empty summary
	 */
	public synchronized ServiceDescriptionSummary createSummary() {
		int id = summaries.size();
		ServiceDescriptionSummary summary = new ServiceDescriptionSummary(id);
		summaries.add(summary);
		return summary;
	}
	
	/**
	 * Performs a conjunctive query for the given list of keywords.
	 * The keywords must be relation or class IRIs.
	 * The method uses the index previously created from service descriptions 
	 * to find all endpoints / datasets that contain at least one relevant triple
	 * for each IRI in the list of keywords.   
	 *  
	 * @param keywords a list of relation and class IRIs
	 * @return list of summaries containing query relevant endpoint URLs
	 */
	public List<ServiceDescriptionSummary> conjunctiveQuery(List<String> keywords) {
		
		List<ServiceDescriptionSummary> result = new ArrayList<ServiceDescriptionSummary>();
		
		// for each service descriptions, calculate the number of supported given keys
		Map<Integer, Integer> mapId2Count = new HashMap<Integer, Integer>();
		for (String key : keywords) {
			Set<PostingsListElement> set = index.getPostingsList(INDEX_NAME, key);
			if (set == null) {
				LOGGER.warn("no postings list was found for key=" + key);
				break;
			}
			for (PostingsListElement elem : set) {
				int id = elem.id;
				if (mapId2Count.containsKey(id)) {
					int count = mapId2Count.get(id) + 1;
					mapId2Count.put(id,  count);
				} else {
					mapId2Count.put(id, 1);
				}
			}
		}
		
		// find all service descriptions that support all given keys
		for (Integer id : mapId2Count.keySet()) {
			int count = mapId2Count.get(id);
			if (count == keywords.size()) {
				ServiceDescriptionSummary summary = getSummaries().get(id);
				result.add(summary);
			}
		}
		
		return result;
	}
	
	/**
	 * Returns the endpoints URLs corresponding to all added service descriptions. 
	 *  
	 * @return endpoint URLs
	 */
	public List<String> getEndpointUrls() {
		List<String> urls = new ArrayList<String>();
		for (ServiceDescriptionSummary summary : getSummaries()) {
			urls.add(summary.endpointURL);
		}
		return urls;
	}
}
