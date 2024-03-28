package uk.ac.cam.cares.jps.agent.gfaagent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.Properties;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONObject;

@WebServlet(urlPatterns = {"/calculation", "/floors"})
public class GFAAgent extends JPSAgent{
    
    
    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;
    private String dbUser;
    private String dbPassword;
    public String floorsCsv;
    private String osmSchema;
    private String osmPoint;
    private String osmPolygon;

    public synchronized void init() {
        this.dbName = endpointConfig.getDbName();
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();       
        this.floorsCsv = endpointConfig.getFilepath();
        this.osmSchema = endpointConfig.getOSMSchema();
        this.osmPoint = endpointConfig.getOSMPoints();
        this.osmPolygon = endpointConfig.getOSMPolygons();
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        try {
            GFACalculation gfaCalculation = new GFACalculation(dbUrl, dbUser, dbPassword);
            if(requestParams.getString("requestUrl").contains("/calculation")){            
                //calculate GFA 1. query footpring 2. query height (if no height, estimate 3.2m/floor) 3. calculate 4. store
                gfaCalculation.calculationGFA();
            }else if(requestParams.getString("requestUrl").contains("/floors")){
               //integrate floors data: 1. query osm address 2. match address from HDB csv 3. store floors data
                IntegrateFloors integrateFloors = new IntegrateFloors(dbUrl, dbUser, dbPassword, osmSchema, osmPoint, osmPolygon);
                integrateFloors.matchAddress(floorsCsv);
            }


        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }
}
