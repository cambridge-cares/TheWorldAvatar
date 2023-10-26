package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.io.IOException;
import java.sql.SQLException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet(urlPatterns = {"/sql", "/status"})
public class DataIntegrationAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(DataIntegrationAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    // private static boolean AGENT_IN_STACK = false;
    // private static final String INVALID_PARAMETER_ERROR_MSG = "Parameters are invalid, please check logs for more details.";
    // private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    // private static final String KEY_DATABASE = "database";
    // private static final String KEY_SOURCE_DATABASE = "srcDbName";
    // private static final String DATABASE_2D = "2d";
    // private static final String DATABASE_3D = "3d";
    // private static final String TABLE_2D = "table2d";
    // private static final String KEY_VALUES = "values";
    @Override
    public synchronized void init() {
        try {
            super.init();
            // Ensure logging are properly working
            LOGGER.debug("This is a test DEBUG message");
            LOGGER.info("This is a test INFO message");
            LOGGER.warn("This is a test WARN message");
            LOGGER.error("This is a test ERROR message");
            LOGGER.fatal("This is a test FATAL message");
        } catch (Exception exception) {
            DataIntegrationAgent.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
        }
    }

    /**
     * An overloaded method to process all the different HTTP (GET/POST/PULL..) requests.
     * Do note all requests to JPS agents are processed similarly and will only return response objects.
     * Parameter 1: function (attribute, footprint, height)
     * Parameter 2: thematic (true or false)
     * @return A response to the request called as a JSON Object.
     */

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        try {
			// JSONObject input = new JSONObject(request.getParameter("function"));	
            String input = request.getContextPath();
            String para = request.getParameter("function");
            String themPara =  request.getParameter("thematic");
			System.out.println("Input : " + input);
			getParameters(para,themPara);
			response.getWriter().write(input);
		} catch (JSONException | SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}

    /**
     * A method to call different functions based on the parameters that get from HTTP
     * SpatialLink: spatial matching of OSM data and 3D buidling, then migrate name and address of building from OSM to 3D building
     * FootPrint: extract footprint of 3D building and store in postgresql
     * Height: calculate height of 3D buidling and store in postgresql
     * @return json message 
     */
    public JSONObject getParameters(String requestParams, String thematicParams) throws SQLException {
        JSONObject jsonMessage = new JSONObject();
        Config c = new Config();
        // String[] config = c.retrieveSQLConfig();
        String[] config = c.Config();
        if(requestParams.equals("attribute")){            
            // jsonMessage = sqlRoute(config);
            SpatialLink spatialLink = new SpatialLink();
            spatialLink.SpatialLink(config);
        }else if(requestParams.equals("footprint")||requestParams.equals("roofprint")){
            FootPrint footprint = new FootPrint();
            footprint.proFootPrint(config, thematicParams, requestParams);
        }else if(requestParams.equals("height")){
            GetHeight height = new GetHeight();
            height.preCalculation(config);
        }
        return jsonMessage;
    }
    
}
