package uk.ac.cam.cares.jps.ontomatch;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.apache.commons.validator.routines.UrlValidator;
import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.sparql.resultset.RDFOutput;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher.MATCHING_TYPE;
import uk.ac.cam.cares.jps.ontomatch.alignment.AlignmentIOHelper;
import uk.ac.cam.cares.jps.ontomatch.properties.OntomatchProperties;

/**
 * Coordination agent that runs each component agent to run a process of
 * ontology matching, two types provided: terminology and individual Outout to
 * KG: alignment ontology of this matching process
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
@WebServlet(urlPatterns = { "/coordinate" })
public class CoordinationAgent extends JPSAgent {

	private static final long serialVersionUID = 1129478305476925061L;
	private String kb_path = OntomatchProperties.getInstance().getProperty(OntomatchProperties.OM_KB_PATH);
	private String owl_ext = OntomatchProperties.getInstance().getProperty(OntomatchProperties.OWL_EXT);

	List<String> onto1TargetClasses = new ArrayList<String>();
	List<String> onto2TargetClasses = new ArrayList<String>();

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject jo = requestParams;
		String alignmentIRI = null, sIRI = null, tIRI = null, type = null;
		String modelPath = null, dictPath = null;
		double threshold = 0;
		Double[] weights = { 0.1, 0.2, 0.3 };
		String[] choices = null;
		String[] matchers = new String[3];
		Double sameClassThrehold = null, pFactor=null;
		String classAlign = null;

		/** get caller parameters and set component agent parameter *******/
		try {
			alignmentIRI = jo.getString("aIRI");
			sIRI = jo.getString("sourceIRI");
			tIRI = jo.getString("targetIRI");
			type = jo.getString("matchingType");
			threshold = jo.getDouble("threshold");

			/** get weights **/
			JSONArray jweight = jo.getJSONArray("weights");
			List<Double> lweight = new ArrayList<Double>();
			for (int i = 0; i < jweight.length(); i++) {
				lweight.add(jweight.getDouble(i));
			}
			weights = new Double[lweight.size()];
			lweight.toArray(weights);

			/** get choices if any **/
			JSONArray jcho = jo.getJSONArray("choices");
			List<String> lcho = new ArrayList<String>();
			for (int i = 0; i < weights.length; i++) {
				lcho.add(jcho.getString(i));
			}
			choices = new String[lcho.size()];
			lcho.toArray(choices);

		} catch (JSONException e1) {
			e1.printStackTrace();
		}

		try {/** get extra parameter if exists ***/
			modelPath = jo.getString("modelAddress");
			dictPath = jo.getString("dictAddress");
		} catch (Exception e) {
			// do nothing
		}

		try {/** get extra parameter if exists ***/
			classAlign = jo.getString("classAlignment");
			pFactor = jo.getDouble("pFactor");
			sameClassThrehold = jo.getDouble("sameClassThrehold");

		} catch (Exception e) {
			// do nothing
		}
		/** set matchers to call - matching type specific ***/
		if (MATCHING_TYPE.valueOf(type) == MATCHING_TYPE.TERM) {
			/*********************
			 * CASE: TERMINOLOGY MATCHING
			 ********************************/
			matchers[0] = "STRING";
			matchers[1] = "WORD";
			matchers[2] = "DOMAIN";
		} else {
			/*********************
			 * CASE: INDIVIDUAL MATCHING
			 ********************************/
			String sourceTBOX = jo.getString("sourceTBOX");
			String targetTBOX = jo.getString("targetTBOX");

			matchers[0] = "I_STRING";
			matchers[1] = "I_WORD";
			matchers[2] = "VALUE";

			/** retrieve map of ****/
			// String[] targetClassIRIs =
			// {"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant"
			// };
			URI uri;
			try {
				String localDBAddr = AlignmentIOHelper.IRI2local(classAlign);
				List[] targetClasses = retrieveClassAlignmentMap(localDBAddr);
				queryPotentialInstanceAndSave(targetClasses[0], sIRI, sIRI, true, getTempPath(sIRI));
				System.out.println("save to: " + getTempPath(tIRI));
				String StmpPath  =getTempPath(sIRI);
				loadTBOX(StmpPath,sourceTBOX, StmpPath);
			     sIRI = StmpPath;
			   String TtmpPath =   getTempPath(tIRI);
				loadTBOX(tIRI,targetTBOX, TtmpPath);
				tIRI = TtmpPath;
			} catch (Exception e) {
				e.printStackTrace();
			}

		}

		JSONObject result = new JSONObject();
		try {
			// TODO: err handling
			/** call lexical processor ***/
			callLexicalProcessor(sIRI);
			callLexicalProcessor(tIRI);
			/** call matching agents */
			List tmpAlignments = callMatchingAgents(sIRI, tIRI, type, matchers, modelPath, dictPath);
			/** call aggregation agent, which saves the alignment result to KG **/
			callAggregationAgents(weights, tmpAlignments, choices, threshold, sIRI, tIRI, alignmentIRI,classAlign,pFactor, sameClassThrehold);
			result.put("success", true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

	public List[] retrieveClassAlignmentMap(String aIRI) throws org.apache.jena.sparql.lang.sparql_11.ParseException, FileNotFoundException {
		List<String[]> alignment = AlignmentIOHelper.readAlignmentFileAsList(aIRI);
		Set<String> onto1TargetClasses = new HashSet<String>();
		Set<String> onto2TargetClasses = new HashSet<String>();

		for (String[] m : alignment) {
			System.out.println(m[0]);
			onto1TargetClasses.add(m[0]);
			onto2TargetClasses.add(m[1]);
		}
		List[] targetClasses = new List[2];
		targetClasses[0] = new ArrayList<String>(onto1TargetClasses);
		;
		targetClasses[1] = new ArrayList<String>(onto2TargetClasses);
		return targetClasses;
	}

	protected String getTempPath(String tIRI) throws URISyntaxException {
		URI uri = new URI(tIRI);
		String[] segments = uri.getPath().split("/");
		String name = segments[segments.length - 1] + "pt.owl";// TODO: proper format this
																								// name
		String nameWpath = OntomatchProperties.getInstance().getProperty(OntomatchProperties.OM_KB_URL) + "/" + name;
		return nameWpath;
	}

	/***
	 * calls the lexical processor with required params
	 * 
	 * @param IRI of the ontology to process
	 */
	protected void callLexicalProcessor(String IRI) {
		JSONObject requestParams = new JSONObject();
		String name = getShortName(IRI);
		String saveAddress = AgentLocator.getCurrentJpsAppDirectory(CoordinationAgent.class) + "/tmp/" + name + ".pkl";
		saveAddress = saveAddress.replaceAll("\\\\", "/");
		// logger.info("Call lexical processor with saveAddress:" + saveAddress + "
		// ontologyIRI: " + IRI);
		requestParams.put("saveAddress", saveAddress);
		requestParams.put("ontologyIRI", IRI);
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/ontologyProcessor",
				requestParams.toString());
	}

	/***
	 * 
	 * @param sOnto        source ontology IRI to match
	 * @param tOnto        target ontology IRI to match
	 * @param matchingType TERM or INDIVIDUAL
	 * @param matchers     String[] types of the matchers to call
	 * @return String[] names of the generated alignment ontology in KG
	 * @throws JSONException
	 * @throws ParseException
	 */
	protected List<String> callMatchingAgents(String sOnto, String tOnto, String matchingType, String[] matchers,
			String modelPath, String dictPath) throws JSONException, ParseException {
		// call each matcher
	List names = new ArrayList<String>();
		for (int i = 0; i < matchers.length; i++) {
			JSONObject requestParams = new JSONObject();
			// create names for each 2bgenerate alignment file
			String name = getAlignmentFileName(tOnto, sOnto, matchers[i]);
			names.add(name);
			requestParams.put("alignmentFileAddress", name);
			// handle special case: domain
			requestParams.put("matcherType", matchers[i]);
			requestParams.put("matchingType", matchingType);
			requestParams.put("targetOntoIRI", tOnto);
			requestParams.put("sourceOntoIRI", sOnto);
			if (matchers[i].equals("DOMAIN")) {
				requestParams.put("modelAddress", modelPath);
				requestParams.put("dictAddress", dictPath);
			}
			AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/elementMatcher", requestParams.toString());
		}
		return names;
	}

	/**
	 * auto construct alignment ontology name according to the matching info
	 * 
	 * @param tgtOnto     target ontology IRI
	 * @param srcOnto     source ontology IRI
	 * @param matcherType matcherType or matchingType of the agent that will
	 *                    generate this alignment
	 * @return String name of alignment ontology to generate
	 * @throws ParseException
	 */
	protected String getAlignmentFileName(String tgtOnto, String srcOnto, String matcherType) throws ParseException {
		final DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/ddhh:mm:ssaaa");
		Date date = new Date();
		String dateFormatted = dateFormat.format(date);
		return "file:///" + kb_path + getShortName(tgtOnto) + getShortName(srcOnto) + matcherType + owl_ext;
	}

	protected String getShortName(String IRI) {
		String secs[] = IRI.split("/");
		String fileName = secs[secs.length - 1];
		String names[] = fileName.split("\\.");
		return names[0];
	}

	/**
	 * Calls the aggregation agent with correct params
	 * 
	 * @param weights            weights to weight sum each alignment measure
	 * @param alignmentIRIs2Aggr
	 * @param choices            choices of optional steps in aggregator
	 * @param threshold          threshold of measure saving to the final alignment
	 * @param srcOnto
	 * @param tgtOnto
	 * @param addr
	 */
	protected void callAggregationAgents(Double[] weights, List alignmentIRIs2Aggr, String[] choices,
			double threshold, String srcOnto, String tgtOnto, String addr, String classAlign, Double pfactor, Double sameClassThreshold) {
		JSONObject requestParams = new JSONObject();
		requestParams.put("weights", weights);
		requestParams.put("alignments", alignmentIRIs2Aggr);
		requestParams.put("srcOnto", srcOnto);
		requestParams.put("tgtOnto", tgtOnto);
		requestParams.put("addr", addr);
		requestParams.put("threshold", threshold);

		if (choices != null) {
			requestParams.put("choices", choices);
		}
		if(classAlign!=null) {
			requestParams.put("classAlign", classAlign);
		}
		if(pfactor!=null) {
			requestParams.put("pfactor", pfactor);
		}
		if(sameClassThreshold!=null) {
			requestParams.put("sameClassThreshold", sameClassThreshold);
		}		
		AgentCaller.executeGetWithJsonParameter("/JPS_ONTOMATCH/matchAggregator", requestParams.toString());
	}

	/**
	 * load T-BOX related to an A-BOX (this extra step is only required due to the
	 * python ontology processing library we use now)
	 * 
	 * @param tboxIRI
	 * @param aboxIRI
	 * @param saveAddr
	 * @throws Exception
	 */
	public void loadTBOX(String fileAddr, String TBOXIRI, String saveAddr) throws Exception {
		String[] params = { fileAddr, TBOXIRI, saveAddr };
		JSONObject results = AsyncPythonHelper.callPython("success",
				OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_LOADTBOX), params,
				CoordinationAgent.class);
		System.out.println(results);
	}

	/***
	 * query potential instances
	 * 
	 * @param targetClassIRIs
	 * @param ep
	 * @param tboxIRI
	 * @param isRemote
	 * @param saveIRI
	 * @throws IOException
	 */
	// TODO: clean this and test
	public void queryPotentialInstanceAndSave(List<String> targetClassIRIs, String ep, String tboxIRI, Boolean isRemote,
			String saveIRI) throws IOException {
		// use targetClassIRI to execute remote query to remoteEP
		String addr = ResourcePathConverter.convertToLocalPath(saveIRI);
		Model model = ModelFactory.createDefaultModel();

		for (String targetclassIRI : targetClassIRIs) {
			String sparql = "SELECT DISTINCT * WHERE {?s a <" + targetclassIRI + ">. "
					 + "}";
			List<QuerySolution> rlist = queryOnce(ep, sparql);
			for (QuerySolution triple : rlist) {
				String sIRI = triple.getResource("s").getURI();
				if(!validateURL(sIRI)) {
					continue;
				}
				Resource s = model.createResource(sIRI);
				String sparqlAttr = "SELECT DISTINCT * WHERE {<"+sIRI+"> ?p ?o. "
						 + "}";
				List<QuerySolution> attrs = queryOnce(ep, sparqlAttr);
				for (QuerySolution attr : attrs) {
					String pIRI= attr.getResource("p").toString();
					Property p = model.createProperty(pIRI);
					RDFNode o = attr.get("o");
					Boolean validated = true;
						if(!validateURL(pIRI)) {
							validated = false;
						} else if(o.isResource()) {
							if(o.asResource().getURI()!=null &&!validateURL(o.asResource().getURI())) {
								validated = false;
							}
						}
					if(validated) {
						s.addProperty(p,o);
					}
						
					
				}
			}
		}
		Writer w = new OutputStreamWriter(new FileOutputStream(new File(addr), true), StandardCharsets.UTF_8);
		model.write(w);
	}

	protected boolean validateURL (String url)  {
		if(url.contains("%")) {
			return false;
		}
		UrlValidator validator = new UrlValidator();
		return validator.isValid(url);
	}

	public List<QuerySolution> queryOnce(String ep, String queryStr) {
	
		String resultBody = Http.execute(Http.get(ep, "application/json", queryStr));

		InputStream targetStream = new ByteArrayInputStream(resultBody.getBytes());
		ResultSet resultSet = ResultSetFactory.fromJSON(targetStream);
		List<QuerySolution> rlist = ResultSetFormatter.toList(resultSet);
		return rlist;
	}

	/***
	 * Get potential Instances to match according to class, save as a tmp ontology
	 * file
	 * 
	 * @param targetClassIRIs
	 * @param ep
	 * @param isRemote
	 * @param saveIRI
	 * @throws IOException
	 */
	public void queryPotentialInstanceAndSave2(String[] targetClassIRIs, String ep, Boolean isRemote, String saveIRI)
			throws IOException {
		// use targetClassIRI to execute remote query to remoteEP
		String resultBody;
		String addr = ResourcePathConverter.convertToLocalPath(saveIRI);
		Model model = ModelFactory.createDefaultModel();

		for (int i = 0; i < targetClassIRIs.length; i++) {
			String sparql = "SELECT DISTINCT * WHERE {?s a <" + targetClassIRIs[i] + ">. "
					+ " ?s ?p ?o. ?s <http://dbpedia.org/ontology/country> <http://dbpedia.org/resource/Germany>."
					+ "}LIMIT 50";
			/** query either remotely or locally */
			if (isRemote) {
				JSONObject params = new JSONObject();
				resultBody = Http.execute(Http.get(ep, null, sparql));
			} else {
				resultBody = KnowledgeBaseClient.query(ep, null, sparql);
			}
			/*** convert result str to list ****/
			InputStream targetStream = new ByteArrayInputStream(resultBody.getBytes());
			ResultSet result = ResultSetFactory.fromXML(targetStream);
			List<QuerySolution> rlist = ResultSetFormatter.toList(result);
			try {// loop thru solution to construct model
				for (int j = 0; j < rlist.size(); j++) {
					QuerySolution triple = rlist.get(i);
					String sIRI = triple.getResource("s").toString();
					Resource s = model.createResource(sIRI);
					Property p = model.createProperty(triple.getResource("p").toString());
					RDFNode o = triple.get("o");
					s.addProperty(p, o);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		/** write model to KG as file ****/
		BufferedWriter writer = new BufferedWriter(new FileWriter(addr));
		model.write(writer);
	}

	protected String getTripleUpdateStr(String s, String p, String o) {
		String sparql = "INSERT DATA{\r\n" + "  <" + s + ">" + "  <" + p + "<" + o + ">.}";
		return sparql;
	}

	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty() || !requestParams.has("aIRI") || !requestParams.has("threshold")
				|| !requestParams.has("sourceIRI") || !requestParams.has("targetIRI")
				|| !requestParams.has("matchingType") || !requestParams.has("weights")) {
			throw new BadRequestException();
		}
		return true;
	}
}
