package uk.ac.cam.cares.jps.bio;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.Path;

import com.jayway.jsonpath.JsonPath;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.bio.json.parser.JSonRequestParser;

import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

/**
 * ---------------------------------- Cross-domain query agent ----------------------------------
 *
 * This agent executes a cross-domain query to access/filter items based on their geo-spatial and
 * chemical engineering concepts, simultaneously, that are present in different knowledge graphs.
 * One of the ultimate aims of this query is to return the heat emission information of different
 * buildings/objects present in a given area. This way, we can automatically assign heat emission
 * data corresponding to multiple buildings within a specified area.This is of interest to the CS
 * 2.0 project.
 *
 * However, this class file demonstrates the feasibility of a cross-domain query by returning the
 * cost of equipments within a bounding box in Jurong Island. To achieve this, the query consists
 * of two parts, made up of two queries: geospatial part and chemical engineering part. First, we
 * obtain all the equipments, and their respective IRIs, from the geo-spatial part; then, we pass
 * the returned variables and check for their cost through the chemical engineering part; lastly,
 * the agent returns the filtered list that satisfies some conditions pertaining to the above two
 * parts.
 *
 * @author Vishvak Kannan
 *
 * ----------------------------------------------------------------------------------------------
 */

/**
 * Servlet implementation class CrossdomainQueryAgent; URL pattern to execute the agent: <http://
 * www.theworldavatar.com/JPS_BIODIESELPLANT3/crossdomainqueryagent>
 */



@Controller
@WebServlet(urlPatterns = {CrossdomainQueryAgent.URL_PATH})

// Agent begins
public class CrossdomainQueryAgent extends JPSAgent {


    public static final String URL_PATH         = "/performquery";
    //Display messages
    private static final String BAD_INPUT       = "Error in input parameters, please check the" +
                                                    " input file";
    private static final String UNKNOWN_REQUEST = "This request is unknown";


    /**
     * Receives inputs as JSON Objects, executes the CrossdomainQueryAgent and returns the result
     * as JSON objects as well. Here we pass the method "Crossdomain query" to execute the actual
     * query.
     *
     * @param requestParams
     * @param request
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        String path = request.getServletPath();
        System.out.println("A request has been received..............................\n");
        if (path.equals(URL_PATH)) {
            try {
                validateInput(requestParams);
            } catch (BadRequestException e) {
                return requestParams.put(BAD_INPUT, e.getMessage());
            }
            return new performcrossdomainquery(requestParams);
        }
        else {
            System.out.println("Unknown request.\n");
            throw new JPSRuntimeException(UNKNOWN_REQUEST);
        }
    }

    /**
     * Validates all the provided input parameters and checks if all the necessary parameters are
     * provided or not.
     *
     * @param requestParams
     * @return
     * @throws BadRequestException
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        String UPPER_LIMITS = JSonRequestParser.getUPPER_LIMITS(requestParams.toString());
        if(UPPER_LIMITS == null || UPPER_LIMITS.trim().isEmpty()){
            throw new BadRequestException("Upper limits for the bounding box are missing.\n");
        }
        String LOWER_LIMITS = JSonRequestParser.getLOWER_LIMITS(requestParams.toString());
        if(LOWER_LIMITS == null || LOWER_LIMITS.trim().isEmpty()){
            throw new BadRequestException("Lower limits for the bounding box are missing.\n");
        }
        String EQUIP_COST   = JSonRequestParser.getEQUIP_COST(requestParams.toString());
        if(EQUIP_COST   == null || EQUIP_COST.trim().isEmpty()){
            throw new BadRequestException("Equipment cost to filter the objects is missing.\n");
        }
        return true;
    }











}
