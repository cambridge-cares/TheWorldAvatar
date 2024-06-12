package uk.ac.cam.cares.jps.agent.buildingflooragent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.Properties;
import java.io.FileReader;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONObject;

@WebServlet(urlPatterns = { "/floors", "/floorswithodba" })
public class BuildingFloorAgent extends JPSAgent {

    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;
    private String dbUser;
    private String dbPassword;
    public String floorsCsv;
    private String osmSchema;
    private String osmPoint;
    private String osmPolygon;
    private String ontopUrl;

    public synchronized void init() {
        this.dbName = endpointConfig.getDbName();
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();
        this.floorsCsv = endpointConfig.getFilepath();
        this.osmSchema = endpointConfig.getOSMSchema();
        this.osmPoint = endpointConfig.getOSMPoints();
        this.osmPolygon = endpointConfig.getOSMPolygons();
        this.ontopUrl = endpointConfig.getOntopUrl();
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        try {

            // integrate floors data: 1. query osm address 2. match address from HDB csv 3.
            // store floors data
            IntegrateFloors integrateFloors = new IntegrateFloors(dbUrl, dbUser, dbPassword, osmSchema, osmPoint,
                    osmPolygon, ontopUrl);
            integrateFloors.addFloorCatColumn();
            integrateFloors.matchAddress(floorsCsv);
            integrateFloors.importFloorDate();

            // Upload Ontop mapping
            if (requestParams.getString("requestUrl").contains("withodba")) {
                try {
                    String odbaFile = "/resources/buildingfloor.odba";
                    Path odbaPath = Paths.get(odbaFile);
                    OntopClient ontopClient = OntopClient.getInstance();
                    ontopClient.updateOBDA(odbaPath);
                } catch (Exception e) {
                    System.out.println("Could not retrieve buildingfloor .obda file.");
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }

}
