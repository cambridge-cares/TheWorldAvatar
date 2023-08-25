package uk.ac.cam.cares.jps.agent.osmagent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.GeometryMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageShareCalculator;

import javax.servlet.annotation.WebServlet;
import java.sql.Connection;
import java.util.*;

import org.json.JSONObject;

@WebServlet(urlPatterns = "/update")

public class OSMAgent extends JPSAgent {
    private EndpointConfig endpointConfig = new EndpointConfig();
    private String geoDatabase;
    private String osmDatabase;
    private String dbUser;
    private String dbPassword;
    public static final String POINT_TABLE = "points";
    public static final String POLYGON_TABLE = "polygons";

    private Map<String, String> configs = new HashMap<>();

    public void init() {
        readConfig();
        dbUser = endpointConfig.getDbUser();
        dbPassword = endpointConfig.getDbPassword();
        configs.put(GeometryMatcher.GEO_DB, endpointConfig.getDbUrl(geoDatabase));
        configs.put(GeometryMatcher.GEO_USER, dbUser);
        configs.put(GeometryMatcher.GEO_PASSWORD, dbPassword);
        configs.put(GeometryMatcher.OSM_DB, endpointConfig.getDbUrl(osmDatabase));
        configs.put(GeometryMatcher.OSM_USER, dbUser);
        configs.put(GeometryMatcher.OSM_PASSWORD, dbPassword);
    }

    public void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");
        geoDatabase = config.getString("geo.database");
        osmDatabase = config.getString("osm.database");
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {
            UsageMatcher.checkAndAddColumns(endpointConfig.getDbUrl(osmDatabase), dbUser, dbPassword);
            UsageMatcher.updateOntoBuilt(endpointConfig.getDbUrl(osmDatabase), dbUser, dbPassword);
            GeometryMatcher geometryMatcher = new GeometryMatcher(configs);
            geometryMatcher.matchGeometry(POINT_TABLE);
            geometryMatcher.matchGeometry(POLYGON_TABLE);
            UsageShareCalculator.updateUsageShare(endpointConfig.getDbUrl(osmDatabase), dbUser, dbPassword);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }
}