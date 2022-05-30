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
 * A service description is an RDF graph containing information about a SPAQRL service and its SPARQL endpoints
 * such as available endpoints and number of occurrences of relations 
 * and classes in the triples of corresponding RDF datasets.
 * The IRIs of the relations and classes are used for indexing. 
 * Each relation and class IRI is mapped to a "postings" list of endpoints / datasets
 * containing at least one triple with this IRI as predicate or object, resp.
 * This class also allows to perform conjunctive queries 
 * which can be used for automated endpoint selection.
 *
 */
public class ServiceDescriptionIndexer {
	
	static final Logger LOGGER = LogManager.getLogger(ServiceDescriptionIndexer.class);
	
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
	}
	
	public class PostingsListElement implements Comparable<PostingsListElement>{
		public Integer id = null;
		public Long number = null;
		
		public PostingsListElement(Integer id, Long number) {
			this.id = id;
			this.number = number;
		}

		@Override
		public int compareTo(PostingsListElement element) {
			return this.id.compareTo(element.id);
		}
	}
	
	private final static String INDEX_NAME = "SCHEMA";
	private List<ServiceDescriptionSummary> summaries = new ArrayList<ServiceDescriptionIndexer.ServiceDescriptionSummary>();
	private SimpleMultiIndex<String, PostingsListElement> index = new SimpleMultiIndex<String, PostingsListElement>();
	
	public ServiceDescriptionIndexer() {
	}
	
	private Map<String, Set<PostingsListElement>> getSchemaIndex() {
		return index.getIndex(INDEX_NAME);
	}
	
	public Set<String> getKeys() {
		return getSchemaIndex().keySet();
	}
	
	public Set<PostingsListElement> getPostingsList(String key) {
		return index.getPostingsList(INDEX_NAME, key);
	}
	
	public List<ServiceDescriptionSummary> getSummaries() {
		return summaries;
	}
	
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

	public static Map<String, Long> getNumberOfClassTriples(OntModel model) {
		String sparql = "SELECT * WHERE { \r\n"
				+ "	?s <http://rdfs.org/ns/void#class> ?class . \r\n"
				+ "	?s <http://rdfs.org/ns/void#triples> ?ntriples . \r\n"
				+ "}";
		return getNumberOfTriples(model, sparql, "class");
	}
	
	public static Map<String, Long> getNumberOfPropertyTriples(OntModel model) {
		String sparql = "SELECT * WHERE { \r\n"
				+ "	?s <http://rdfs.org/ns/void#property> ?property . \r\n"
				+ "	?s <http://rdfs.org/ns/void#triples> ?ntriples . \r\n"
				+ "}";
		return getNumberOfTriples(model, sparql, "property");
	}
	
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
	
	public static String queryServiceDescription(String endpointUrl) {
		String accept = "application/rdf+xml";
		HttpGet request = Http.get(endpointUrl, accept);
		return Http.execute(request);
	}
	
	private void add(String path) {
		LOGGER.debug("Adding service description=" + path);
		
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
	}
	
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
	
	public synchronized ServiceDescriptionSummary createSummary() {
		int id = summaries.size();
		ServiceDescriptionSummary summary = new ServiceDescriptionSummary(id);
		summaries.add(summary);
		return summary;
	}
	
	public List<ServiceDescriptionSummary> conjunctiveQuery(List<String> keys) {
		
		List<ServiceDescriptionSummary> result = new ArrayList<ServiceDescriptionSummary>();
		
		// for each service descriptions, calculate the number of supported given keys
		Map<Integer, Integer> mapId2Count = new HashMap<Integer, Integer>();
		for (String key : keys) {
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
			if (count == keys.size()) {
				ServiceDescriptionSummary summary = getSummaries().get(id);
				result.add(summary);
			}
		}
		
		return result;
	}
	
	public List<String> getEndpointUrls() {
		List<String> urls = new ArrayList<String>();
		for (ServiceDescriptionSummary summary : getSummaries()) {
			urls.add(summary.endpointURL);
		}
		return urls;
	}
}
