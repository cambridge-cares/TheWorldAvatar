package uk.ac.cam.cares.jps.agent.iri.resolution;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.KGRouter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;

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
		String repositoryOrNamespace = retrieveRepositoryOrNamespace(request.getRequestURI());
		System.out.println("Requested IRI: "+request.getRequestURI());
		return repositoryOrNamespace + " " + request.getRequestURL();
	}
	
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
		String repositoryOrNamespace = retrieveRepositoryOrNamespace(request.getRequestURI());
		System.out.println("Requested IRI: "+request.getRequestURI());
		return repositoryOrNamespace + " " + request.getRequestURL().toString();
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
		String repositoryOrNamespace = retrieveRepositoryOrNamespace(request.getRequestURI());
		System.out.println("Requested IRI: "+request.getRequestURI());
		return repositoryOrNamespace + " " + request.getRequestURL().toString();
	}

	/**
	 * Extracts the name of repository or namespace from the current IRI<br>
	 * and returns it to the calling method. 
	 * 
	 * @param iri the URI of request
	 * @return
	 */
	public String retrieveRepositoryOrNamespace(String iri){
		String[] tokens = iri.split("/");
		if(tokens.length>1 && tokens[0].length()>5){
			return tokens[0];
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
	private String getResourceTarget(String IRI){
		KnowledgeBaseClient kbClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX.concat(getKBName(IRI)), true, false);
		String json = kbClient.execute(formIRIResolutionQuery(IRI));
		JSONArray jsonArray = new JSONArray(json);
		for (int i = 0; i<jsonArray.length(); i++){
			JSONObject obj = jsonArray.getJSONObject(i);
//			if(obj.getString(LABEL).equals(targetResourceName)){
//				System.out.println(obj.get(QUERY_ENDPOINT));
//				return obj.getString(QUERY_ENDPOINT);
//			}
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
    
    public static void main(String[] args){
    	new IRIResolutionAgent().getResourceTarget("http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl/ArrheniusCoefficient_182161099217501");
    }
}
