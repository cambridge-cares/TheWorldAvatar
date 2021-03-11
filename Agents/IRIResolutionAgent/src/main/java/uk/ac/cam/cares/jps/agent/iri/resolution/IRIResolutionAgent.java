package uk.ac.cam.cares.jps.agent.iri.resolution;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.cmclinnovations.ontology.model.aboxes.ABoxManagement;
import com.cmclinnovations.ontology.model.aboxes.IABoxManagement;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.KGRouter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

/**
 * IRI Resolution Agent is developed to ensure the resolvability of all IRIs<br>
 * created to identify entities/resources in the JPS knowledge graph.<br>
 * 
 * The approach followed in this implementation is the detection of a repo-<br>
 * sitory/namespace that contains the resource connected to the current IRI<br>
 * to query information which are directly connected to this resource.  
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@Controller
@WebServlet(urlPatterns = { IRIResolutionAgent.SHORT_JPS_IRI, IRIResolutionAgent.MEDIUM_JPS_IRI,
		IRIResolutionAgent.LONG_JPS_IRI })
public class IRIResolutionAgent extends JPSAgent{

	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to the IRI Resolution Agent";
	
	public static final String SHORT_JPS_IRI = "/onto*/*/*";
	public static final String MEDIUM_JPS_IRI = "/onto*/*/*/*";
	public static final String LONG_JPS_IRI = "/onto*/*/*/*/*";
	
	public static final String SUBJECT = "SUBJECT";
	public static final String PREDICATE = "PREDICATE";
	public static final String OBJECT = "OBJECT";

	public static final String LT = "<";
	public static final String GT = ">";
	public static final String ONTO_PREFIX = "onto";
	
	public static final String LOCALHOST_8080 = "http://localhost:8080";
	public static final String ACTUAL_HOST = "http://www.theworldavatar.com";
	
	private IABoxManagement aBoxManager; 
	
	/**
	 * Receives a short IRI among two types of IRIs and resolves it by querying<br>
	 * the JPS knowledge graph stored in a polymorphic knowledge store. 
	 * 
	 * @param request
	 * @return
	 */
	@RequestMapping(SHORT_JPS_IRI)
	@ResponseBody
	public String resolveShortIRI(HttpServletRequest request) {
		// If any user runs the IRI resolver from a localhost, the URL will<br>
		// contain the localhost address, which will be replaced with the<br>
		// actual address indicated by the ACTUAL_HOST constant.  
		return getResourceTarget(request.getRequestURL().toString().replaceAll(LOCALHOST_8080, ACTUAL_HOST));	}
	
	/**
	 * Receives a long IRI among two types of IRIs and resolves it by querying<br>
	 * the JPS knowledge graph stored in a polymorphic knowledge store.
	 * 
	 * @param request
	 * @return
	 */
	@RequestMapping(MEDIUM_JPS_IRI)
	@ResponseBody
	public String resolveMediumIRI(HttpServletRequest request){
		// If any user runs the IRI resolver from a localhost, the URL will<br>
		// contain the localhost address, which will be replaced with the<br>
		// actual address indicated by the ACTUAL_HOST constant.  
		return getResourceTarget(request.getRequestURL().toString().replaceAll(LOCALHOST_8080, ACTUAL_HOST));
	}

	/**
	 * Receives a long IRI among two types of IRIs and resolves it by querying<br>
	 * the JPS knowledge graph stored in a polymorphic knowledge store.
	 * 
	 * @param request
	 * @return
	 */
	@RequestMapping(LONG_JPS_IRI)
	@ResponseBody
	public String resolveLongIRI(HttpServletRequest request){
		// If any user runs the IRI resolver from a localhost, the URL will<br>
		// contain the localhost address, which will be replaced with the<br>
		// actual address indicated by the ACTUAL_HOST constant.  
		return getResourceTarget(request.getRequestURL().toString().replaceAll(LOCALHOST_8080, ACTUAL_HOST));	}

	/**
	 * Extracts the name of repository or namespace from the current IRI<br>
	 * and returns it to the calling method. 
	 * 
	 * @param iri the URI of request
	 * @return
	 */
	public String retrieveRepositoryOrNamespace(String iri){
		String[] tokens = iri.split("/");
		if(tokens.length>1 && tokens[0].length()<5){
			return tokens[2];
		}else{
			return null;
		}
	}
	
	/**
	 * Retrieves the query IRI of the target repository/namespace. 
	 * 
	 * @param kgrouterEndpoint
	 * @param targetResourceName
	 * @return
	 * @throws Exception
	 */
	private String getResourceTarget(String IRI) {
		KnowledgeBaseClient kbClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX.concat(getKBName(IRI)), true, false);
		String json = kbClient.execute(formIRIResolutionQuery(IRI));
		System.out.println(json);
		JSONArray jsonArray = new JSONArray(json);
		try {
			return createABox(IRI, jsonArray);
		} catch (OWLOntologyCreationException | OWLOntologyStorageException | ABoxManagementException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	
	
	/**
	 * For a given IRI forms a query to retrieve all predicates and objects connected to it. 
	 * 
	 * @param IRI
	 * @return
	 */
	private String formIRIResolutionQuery(String IRI){
		SelectBuilder builder = new SelectBuilder()
				.addVar( KGRouter.QUESTION_MARK.concat(PREDICATE) )
				.addVar( KGRouter.QUESTION_MARK.concat(OBJECT) )
				.addWhere( getWhereBuilder(IRI) );
		return builder.toString();
	}
	
	/**
	 * Created to put the WHERE part in the SPARQL query commands using the Jena WHERE Builder.
	 * 
	 * @return
	 */
	private WhereBuilder getWhereBuilder(String subject){
		return new WhereBuilder().addWhere(LT.concat(subject).concat(GT),
				KGRouter.QUESTION_MARK.concat(PREDICATE), KGRouter.QUESTION_MARK.concat(OBJECT));
	}
	
	/**
	 * Retrieves the name of knowledge base/name space from a given IRI.<br>
	 * If the IRI is http://www.theworldavatar.com/kb/ontokin/ABF.owl/ArrheniusCoefficient_1749249908529038,<br>
	 * this method will extract ontokin, which is the name of the knowledge base<br>
	 * in the context of JPS.
	 *
	 * 
	 * @param IRI
	 * @return
	 */
	private String getKBName(String IRI){
		if(IRI!=null && (IRI.trim().startsWith(KGRouter.HTTP) || IRI.trim().startsWith(KGRouter.HTTPS))){
			if(IRI.contains(KGRouter.BACKSLASH.concat(KGRouter.KB).concat(KGRouter.BACKSLASH))){
				String[] tokens = IRI.split(KGRouter.BACKSLASH);
				if(tokens.length>=7 && tokens[3].equals(KGRouter.KB) && tokens[4].startsWith(ONTO_PREFIX)){
					return tokens[4];
				}
			}
		}
		return null;
	}
	
	/**
	 * Creates an ABox when all object and data properties passed to this<br>
	 * method in the jsonArray parameter.
	 * 
	 * @param aBoxIRI
	 * @param jsonArray
	 * @return
	 * @throws OWLOntologyCreationException
	 * @throws OWLOntologyStorageException
	 * @throws ABoxManagementException
	 */
	private String createABox(String aBoxIRI, JSONArray jsonArray) throws OWLOntologyCreationException, OWLOntologyStorageException, ABoxManagementException {
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLOntology ontology = manager.createOntology(IRI.create(aBoxIRI));
		if(aBoxManager == null){
			aBoxManager = new  ABoxManagement();
		}
		// Assigns classes to the current instance.
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
			// Assigns anything found at the target of rdf:type as the class of the current instance
			// except owl:NamedIndividual.
			if(obj.getString(PREDICATE).equals(ABoxManagement.RDF_URL.concat(ABoxManagement.RDF_TYPE)) 
					&& !obj.getString(OBJECT).equals(ABoxManagement.OWL_URL.concat(ABoxManagement.OWL_NAMED_INDIVIDUAL))){
				aBoxManager.createIndividual(ontology, obj.getString(OBJECT), null, aBoxIRI, null);
			}
		}
		// Assigns both object and data properties to the current instance.
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
			// Ignores classes of the current instance.
			if(!obj.getString(PREDICATE).equals(ABoxManagement.RDF_URL.concat(ABoxManagement.RDF_TYPE)) ){
				// Presence of an IRI indicates that an object property should be created. 
				if(obj.getString(OBJECT).trim().startsWith(KGRouter.HTTP) || obj.getString(OBJECT).trim().startsWith(KGRouter.HTTPS)){
					aBoxManager.addObjectProperty(ontology, aBoxIRI, IRI.create(obj.getString(PREDICATE)), aBoxIRI, obj.getString(OBJECT));					
				}else{ // A value indicates that a data property should be created.
					aBoxManager.addDataProperty(ontology, aBoxIRI, null, IRI.create(obj.getString(PREDICATE)), obj.getString(OBJECT), "unknown");
				}
			}
		}		
		java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();    
		System.setOut(new java.io.PrintStream(out));  
		manager.saveOntology(ontology, System.out);
		System.err.println(out.toString());
		String ontologyInString = out.toString().replaceAll(" rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"", "");
		return ontologyInString;
	}
	
	/**
	 * Receives and processes HTTP requests that match with the URL patterns<br>
	 * listed in the annotations of this class.
	 * 
	 */
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		boolean isValidInput = false;
		System.out.println("A request has been received..............................");
		try {
			isValidInput = validateInput(requestParams);
		} catch (BadRequestException e) {
			return requestParams.put(BAD_REQUEST_MESSAGE_KEY, e.getMessage());
		}
		if (isValidInput) {
			return requestParams;
		} else {
			System.out.println("Unknown request");
			throw new JPSRuntimeException(UNKNOWN_REQUEST);
		}
	}
    
    /**
     * Validates input parameters specific to IRI Resolution Agent to decide whether<br>
     * a request can be served.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        return true;
    }
}
