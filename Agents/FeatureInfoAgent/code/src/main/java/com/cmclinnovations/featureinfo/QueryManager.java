package com.cmclinnovations.featureinfo;

import java.io.IOException;
import java.time.Instant;
import java.util.List;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.InternalServerErrorException;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.core.ClassHandler;
import com.cmclinnovations.featureinfo.core.meta.MetaHandler;
import com.cmclinnovations.featureinfo.core.time.TimeHandler;
import com.cmclinnovations.featureinfo.objects.Request;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Manager class for the assorted qwueries that run as a part of
 * the agent's /get route.
 */
public class QueryManager {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(QueryManager.class);

    /**
     * Store of class mappings and stack endpoints.
     */
    private final ConfigStore configStore;

    /**
     * Internal KG handling.
     */
    private RemoteStoreClient kgClient;

    /**
     * Internal time series handling.
     */
    private TimeSeriesClient<Instant> tsClient;
    

    /**
     * Initialise a new QueryManager instance.
     * 
     * @param configStore Store of class mappings and stack endpoints.
     */
    public QueryManager(ConfigStore configStore) {
        this.configStore = configStore;
    }

    /**
     * Set the client instances used to connect to the KG and RDB.
     * 
     * @param kgClient Connection to the KG(s).
     */
    public void setClients(
        RemoteStoreClient kgClient,
        TimeSeriesClient<Instant> tsClient) {

        this.kgClient = kgClient;
        this.tsClient = tsClient;
    }

    /**
     * Check the incoming HTTP request for validity.
     * 
     * @param requestParams HTTP request parameters.
     * 
     * @return validity.
     */
    public boolean checkRequest(JSONObject requestParams) {
        // Check that there's an iri
        if (requestParams.isNull("iri") && requestParams.isNull("IRI")) {
            LOGGER.error("Could not find the required 'iri' field within the request's parameters.");
            return false;
        }
        return true;
    }

    /**
     * Runs the core logic of the FIA, calling concrete classes to:
     * 
     * - Determine the classes of the input A-Box IRI.
     * - Find and run any mapped meta data queries.
     * - Find and run any mapped time series queries.
     * - Get time series data from the relational database.
     * - Format and return as JSON.
     * 
     * @param request   Request object containing parameters.
     * @param response  HTTP response to write back to.
     * 
     * @return resulting JSON of discovered meta and time series data.
     * 
     * @throws IOException if response cannot be written to.
     */
    public JSONObject processRequest(Request request, HttpServletResponse response) throws IOException {

        LOGGER.info("Incoming IRI is: {}", request.getIri());
        request.getEndpoint().ifPresentOrElse(
                endpoint -> LOGGER.info("Incoming enforced endpoint is: {}", endpoint),
                () -> LOGGER.info("No incoming enforced endpoint, will attempt federation."));

        // Determine class matches
        List<ConfigEntry> classMatches = null;
        try {
            classMatches = this.determineClasses(request, response);
        } catch (IOException exception) {
            return null;
        }

        // Get meta data
        JSONObject metadata = getMeta(request, classMatches, response);

        // Get time data
        JSONArray timedata = getTime(request, classMatches, response);

        // Combine into a single JSON structure
        JSONObject result = new JSONObject();

        if(metadata != null && !metadata.isEmpty()) {
            result.put("meta", metadata);
        }
         if(timedata != null && !timedata.isEmpty()) {
            result.put("time", timedata);
        }
        return result;
    }    

    /**
     * Runs a ClassHandler instance to determine the class IRIs of the instance IRI and which
     * configuration entries match said classes.
     * 
     * @param iri feature IRI.
     * @param response HTTP response to write to.
     * 
     * @return Set of matching configuration entries.
     * 
     * @throws IOException if response cannot be written to.
     */
    private List<ConfigEntry> determineClasses(Request request, HttpServletResponse response) throws IOException {
        ClassHandler classHandler = new ClassHandler(this.configStore, this.kgClient);
        
        try {
            return classHandler.determineClassMatches(request);
        } catch(IllegalStateException exception) {
            response.setStatus(Response.Status.NO_CONTENT.getStatusCode());
            response.getWriter().write("{\"description\":\"" + exception.getMessage() + "\"}");

        } catch(InternalServerErrorException exception) {
            response.setStatus(Response.Status.BAD_REQUEST.getStatusCode());
            response.getWriter().write("{\"description\":\"" + exception.getMessage() + "\"}");
        }

        return null;
    }

    /**
     * Runs a MetaHandler instance to loop through the input class matches, query the KG for
     * meta data, the formats the result before returning.
     * 
     * @param iri feature IRI.
     * @param classMatches discovered configuration entries will class matches.
     * @param response HTTP response to write to.
     * 
     * @return formatted meta data.
     */
    private JSONObject getMeta(Request request, List<ConfigEntry> classMatches, HttpServletResponse response) {
        MetaHandler metaHandler = new MetaHandler(request.getIri(), request.getEndpoint(), this.configStore);
        metaHandler.setClient(this.kgClient);
        return metaHandler.getData(classMatches, response);
    }

    /**
     * Runs a TimeHandler instance to loop through the input class matches, query the KG for
     * measurement IRIs, then contact the RDB to get time series values.
     * 
     * @param iri feature IRI.
     * @param classMatches discovered configuration entries will class matches.
     * @param response HTTP response to write to.
     * 
     * @return formatted time series data.
     */
    private JSONArray getTime(Request request, List<ConfigEntry> classMatches, HttpServletResponse response) {
        TimeHandler timeHandler = new TimeHandler(request.getIri(), request.getEndpoint(), this.configStore);
        timeHandler.setClients(this.kgClient, this.tsClient, null);
        return timeHandler.getData(classMatches, response);
    }

}
// End of class.