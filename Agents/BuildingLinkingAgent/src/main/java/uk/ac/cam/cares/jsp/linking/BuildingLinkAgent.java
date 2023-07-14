package uk.ac.cam.cares.jsp.linking;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

@WebServlet(urlPatterns = {"/sql", "/status"})
public class BuildingLinkAgent extends JPSAgent {
    
    private static final Logger LOGGER = LogManager.getLogger(BuildingLinkAgent.class);
    private static boolean VALID = true;
    private static boolean AGENT_IN_STACK = false;
    private static final String INVALID_PARAMETER_ERROR_MSG = "Parameters are invalid, please check logs for more details.";
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    private static final String KEY_DATABASE = "database";
    private static final String KEY_SOURCE_DATABASE = "srcDbName";
    private static final String DATABASE_KG = "kg";
    private static final String DATABASE_3D = "3d";
    private static final String TABLE_2D = "table2d";
    private static final String KEY_VALUES = "values";

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
            this.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
        }
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        try {
			// JSONObject input = new JSONObject(request.getParameter("function"));	
            String para = request.getParameter("function");
			System.out.println("Input : " + para.toString());
			getParameters(para);
			response.getWriter().write(para.toString());
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

    public JSONObject getParameters(String requestParams){
        JSONObject jsonMessage = new JSONObject();
        if(requestParams.equals("attribute")){
           Config c = new Config();
           String[] config = c.retrieveSQLConfig();
           jsonMessage = sqlRoute(config);
        }else if(requestParams.equals("footprint")){

        }
        return jsonMessage;
   }

    // public JSONObject processRequestParameters() {
    //     JSONObject jsonMessage = new JSONObject();
    //     String[] config = Config.retrieveSQLConfig();
    //     jsonMessage = sqlRoute(config);

    //     return jsonMessage;
    // }
    /**
     * Run logic for the "/sql" route to do spatial link.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject sqlRoute(String[] config) {
        LOGGER.debug("Creating the SQL connector..");
        JSONObject response = new JSONObject();
        BuildingLink buildingLink = new BuildingLink();
        buildingLink.BuildingLink(config);
//        SqlBridge connector = new SqlBridge(config);
//        LOGGER.debug("Transfer data from source to target database...");
//        JSONObject response = connector.transfer(AGENT_IN_STACK);
//        LOGGER.info("Data have been successfully transferred from " + config[0] + " to " + config[3]);
        if (response.isEmpty()) {
            response.put("Result", "Data have been successfully integrated");
        }
        return response;
    }
}
