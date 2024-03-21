package uk.ac.cam.cares.jps.agent.osmagent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.GeometryMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageShareCalculator;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

import javax.servlet.annotation.WebServlet;
import java.nio.file.Path;


import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import org.json.JSONObject;

@WebServlet(urlPatterns = "/update")

public class OSMAgent extends JPSAgent {
    private static final String PROPERTIES_PATH = "/resources/config.properties";
    private static final Path obdaFile = Path.of("/resources/building_usage.obda");

    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;
    private String osmSchema;
    private String dbUser;
    private String dbPassword;
    public String pointTable;
    public String polygonTable;
    public String landUseTable;
    public String landUseCsv;
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
        try (InputStream input = FileReader.getStream(PROPERTIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.dbName = prop.getProperty("db.name");
            this.osmSchema = prop.getProperty("osm.schema");
            this.landUseTable = prop.getProperty("landuse.table");
            this.landUseCsv = prop.getProperty("landuse.csv");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {
            UsageMatcher usageMatcher = new UsageMatcher(dbUrl, dbUser, dbPassword);
            GeometryMatcher geometryMatcher = new GeometryMatcher(dbUrl, dbUser, dbPassword);
            UsageShareCalculator shareCalculator = new UsageShareCalculator(dbUrl, dbUser, dbPassword);

            // match OSM usage to OntoBuiltEnv:PropertyUsage classes
            usageMatcher.checkAndAddColumns(pointTable, polygonTable);
            usageMatcher.updateOntoBuilt(pointTable, polygonTable);

            // match OSM geometries with building IRI
            geometryMatcher.matchGeometry(pointTable);
            geometryMatcher.matchGeometry(polygonTable);

            // intialise usage table and copy building IRI that has OSM usage
            usageMatcher.copyFromOSM(pointTable, polygonTable, usageTable);

            // match buildings without OSM usage with land use
            if (!landUseTable.isEmpty()) {
                shareCalculator.updateLandUse(usageTable, landUseTable, landUseCsv);
            }

            // assign OntoBuiltEnv:PropertyUsage and calculate usage share for mixed usage
            // buildings
            shareCalculator.updateUsageShare(usageTable);
            shareCalculator.addMaterializedView(usageTable);

            //Create geoserver layer
            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName= "twa";
            String schema = "public";
            geoServerClient.createWorkspace(workspaceName);
            UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
            GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
            virtualTable.setSql(buildingSQLQuery);
            virtualTable.setEscapeSql(true);
            virtualTable.setName("building_usage");
            virtualTable.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
            geoServerVectorSettings.setVirtualTable(virtualTable);
            geoServerClient.createPostGISDataStore(workspaceName,"building_usage" , dbName, schema);
            geoServerClient.createPostGISLayer(workspaceName, dbName,"building_usage" ,geoServerVectorSettings);

            //Upload Isochrone Ontop mapping
            try {
                OntopClient ontopClient = OntopClient.getInstance();
                ontopClient.updateOBDA(obdaFile);
            } catch (Exception e) {
                System.out.println("Could not retrieve building_usage.obda file.");
            }

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }


    private static final String buildingSQLQuery ="WITH \"uuid_table\" AS ( SELECT \"strval\" AS \"uuid\", \"cityobject_id\" FROM \"cityobject_genericattrib\" WHERE \"attrname\" = 'uuid' ), \"iri_table\" AS ( SELECT \"urival\" AS \"iri\", \"cityobject_id\" FROM \"cityobject_genericattrib\" WHERE \"attrname\" = 'iri' ), \"usageTable\" AS ( SELECT \"building_iri\" AS \"iri\", \"propertyusage_iri\", \"ontobuilt\", \"usageshare\" FROM usage.usage ), \"pointsTable\" AS ( SELECT \"building_iri\" AS \"iri\", \"name\" FROM public.points ), \"polygonsTable\" AS ( SELECT \"building_iri\" AS \"iri\", \"name\" FROM public.polygons ) SELECT DISTINCT \"building\".\"id\" AS \"building_id\",   CASE\n" +
            "    WHEN COALESCE(\"pointsTable\".name, \"polygonsTable\".name) IS NOT NULL\n" +
            "    THEN COALESCE(\"pointsTable\".name, \"polygonsTable\".name)\n" +
            "    ELSE CONCAT('Building ',\"uuid_table\".\"cityobject_id\") \n" +
            "  END AS name, COALESCE(\"measured_height\", 100.0) AS \"building_height\", public.ST_Transform(\"geometry\", 4326), \"uuid\", \"iri_table\".\"iri\", \"propertyusage_iri\", \"ontobuilt\", \"usageshare\" FROM \"building\" JOIN \"surface_geometry\" ON \"surface_geometry\".\"root_id\" = \"building\".\"lod0_footprint_id\" JOIN \"uuid_table\" ON \"building\".\"id\" = \"uuid_table\".\"cityobject_id\" JOIN \"iri_table\" ON \"building\".\"id\" = \"iri_table\".\"cityobject_id\" LEFT JOIN \"pointsTable\" ON \"uuid_table\".\"uuid\" = \"pointsTable\".\"iri\" LEFT JOIN \"polygonsTable\" ON \"uuid_table\".\"uuid\" = \"polygonsTable\".\"iri\" LEFT JOIN \"usageTable\" ON \"uuid_table\".\"uuid\" = \"usageTable\".\"iri\" WHERE \"surface_geometry\".\"geometry\" IS NOT NULL AND COALESCE(\"measured_height\", 100.0) != '0'";
}