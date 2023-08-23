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
    private RemoteRDBStoreClient rdbStoreClient;

    private String geoDatabase;
    private String osmDatabase;
    private List<String> tableNames;

    private Map<String, String> configs = new HashMap<>();

    public void init() {
        readConfig();
        EndpointConfig endpointConfig = new EndpointConfig();
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(osmDatabase), endpointConfig.getDbUser(),
                endpointConfig.getDbPassword());
        configs.put(GeometryMatcher.GEO_DB, endpointConfig.getDbUrl(geoDatabase));
        configs.put(GeometryMatcher.GEO_USER, endpointConfig.getDbUser());
        configs.put(GeometryMatcher.GEO_PASSWORD, endpointConfig.getDbPassword());
        configs.put(GeometryMatcher.OSM_DB, endpointConfig.getDbUrl(osmDatabase));
        configs.put(GeometryMatcher.OSM_USER, endpointConfig.getDbUser());
        configs.put(GeometryMatcher.OSM_PASSWORD, endpointConfig.getDbPassword());
    }

    public void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");
        geoDatabase = config.getString("geo.database");
        osmDatabase = config.getString("osm.database");
        tableNames = Arrays.asList(config.getString("osm.tables").split(","));
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try (Connection conn = rdbStoreClient.getConnection()) {
            UsageMatcher.checkAndAddColumns(conn, tableNames);
            UsageMatcher.updateOntoBuilt(conn, tableNames);
            GeometryMatcher geometryMatcher = new GeometryMatcher(configs);
            geometryMatcher.matchGeometry("points");
            geometryMatcher.matchGeometry("polygons");
            UsageShareCalculator.updateUsageShare(conn, tableNames);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }
}