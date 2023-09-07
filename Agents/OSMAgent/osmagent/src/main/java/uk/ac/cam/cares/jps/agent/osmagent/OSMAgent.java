package uk.ac.cam.cares.jps.agent.osmagent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.GeometryMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageShareCalculator;

import javax.servlet.annotation.WebServlet;
import java.util.*;

import org.json.JSONObject;

@WebServlet(urlPatterns = "/update")

public class OSMAgent extends JPSAgent {
    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;
    private String osmSchema;
    private String dbUser;
    private String dbPassword;
    public String pointTable;
    public String polygonTable;
    public String landUseTable;
    public static final String usageTable = "usage.usage";

    public void init() {
        readConfig();
        this.pointTable = osmSchema + "." + "points";
        this.polygonTable = osmSchema + "." + "polygons";
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();
    }

    public void readConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");
        this.dbName = config.getString("db.name");
        this.osmSchema = config.getString("osm.schema");
        this.landUseTable = config.getString("landuse.table");
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {
            UsageMatcher usageMatcher = new UsageMatcher(dbUrl, dbUser, dbPassword);
            GeometryMatcher geometryMatcher = new GeometryMatcher(dbUrl, dbUser, dbPassword);
            UsageShareCalculator shareCalculator = new UsageShareCalculator(dbUrl, dbUser, dbPassword);

            usageMatcher.checkAndAddColumns(pointTable, polygonTable);
            usageMatcher.updateOntoBuilt(pointTable, polygonTable);

            geometryMatcher.matchGeometry(pointTable);
            geometryMatcher.matchGeometry(polygonTable);

            usageMatcher.copyFromOSM(pointTable, polygonTable, usageTable);

            shareCalculator.updateLandUse(usageTable, landUseTable);
            shareCalculator.updateUsageShare(usageTable);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }
}