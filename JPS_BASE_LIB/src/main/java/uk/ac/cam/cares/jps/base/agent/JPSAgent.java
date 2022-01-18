package uk.ac.cam.cares.jps.base.agent;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.JPSAgentInterface;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.util.SysStreamHandler;

public class JPSAgent extends JPSHttpServlet implements JPSAgentInterface {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(JPSAgent.class);

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    static {
        // Upon static initialisation of any JPSAgent, redirect standard
        // system streams (System.out and System.err) to Log4j2 loggers.
        SysStreamHandler.redirectToLoggers();
    }
    /**
     * Initialise a new JPSAgent
     */
    public JPSAgent() {
        LOGGER.info("A new JPSAgent has been initialised.");
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        validateInput(requestParams);
        return new JSONObject();
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        validateInput(requestParams);
        return new JSONObject();
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            LOGGER.warn("Request parameters are empty, throwing BadRequestException...");
            throw new BadRequestException();
        }
        return true;
    }
    
    /**
     * Execute a {@link <a href="https://www.w3.org/TR/sparql11-query/">SPARQL Query</a>} on the target resource 
     * in the Knowledge Graph by calling the AccessAgent. 
     * @param targetResourceID	target namespace or IRI
     * 							e.g. to access the Ontokin triple store
     * 							both "ontokin" and "http://www.theworldavatar.com/kb/ontokin" are accepted.
     * @param sparqlQuery		SPARQL query string
     * @return 	the query result in the {@link <a href="https://www.w3.org/TR/sparql11-results-json/">W3C Query result JSON format</a>} 
     */
    public String query(String targetResourceID, String sparqlQuery) {	
    	return AccessAgentCaller.query(targetResourceID, sparqlQuery);
    }
    
    /**
     * Execute a {@link <a href="https://www.w3.org/TR/sparql11-update/">SPARQL Update</a>} on the target resource 
     * in the Knowledge Graph by calling the AccessAgent. 
     * @param targetResourceID	the target namespace or IRI
     * 							e.g. to access the Ontokin triple store
     * 							both "ontokin" and "http://www.theworldavatar.com/kb/ontokin" are accepted.
     * @param sparqlUpdate		SPARQL update string
     */
    public void update(String targetResourceID, String sparqlUpdate) {
    	AccessAgentCaller.update(targetResourceID, sparqlUpdate);
    }
}
