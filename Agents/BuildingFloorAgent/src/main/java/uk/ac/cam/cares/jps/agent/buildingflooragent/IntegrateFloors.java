package uk.ac.cam.cares.jps.agent.buildingflooragent;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import org.json.JSONArray;
import org.springframework.core.io.ClassPathResource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import org.apache.commons.io.IOUtils;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.intuit.fuzzymatcher.component.MatchService;
import com.intuit.fuzzymatcher.domain.Document;
import com.intuit.fuzzymatcher.domain.Element;
import com.intuit.fuzzymatcher.domain.ElementType;
import com.intuit.fuzzymatcher.domain.Match;

import com.opencsv.bean.CsvToBeanBuilder;

public class IntegrateFloors {
    private static final Logger LOGGER = LogManager.getLogger(IntegrateFloors.class);
    private Map<String, OSMBuilding> iriToBuildingMap;
    private RemoteStoreClient storeClient;

    private static final String DOMESTIC_TYPE = "https://www.theworldavatar.com/kg/ontobuiltenv/Domestic";
    private static final String RESIDENTIAL_TYPE = "https://www.theworldavatar.com/kg/ontobuiltenv/Domestic";

    public IntegrateFloors(String ontopUrl) {
        iriToBuildingMap = new HashMap<>();
        storeClient = new RemoteStoreClient(ontopUrl);
    }

    // check table building has floor Cat. column
    public void addFloorCatColumn(Connection conn) {
        String buildingSQLAlter = "ALTER TABLE citydb.building ADD COLUMN IF NOT EXISTS storeys_above_ground_cat character varying(4000);";

        try (Statement stmt = conn.createStatement()) {
            stmt.executeUpdate(buildingSQLAlter);
        } catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    /**
     * initialise iriToBuildingMap
     * Keys in query results are defined in osm_address.sparql and osm_usage.sparql
     * stored in resources
     */
    void setOSMBuildings() {
        String addressQuery;
        try (InputStream is = new ClassPathResource("osm_address.sparql").getInputStream()) {
            addressQuery = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        JSONArray addressQueryResults = storeClient.executeQuery(addressQuery);

        for (int i = 0; i < addressQueryResults.length(); i++) {
            String buildingIri = addressQueryResults.getJSONObject(i).getString("building");
            String streetName = "null"; // setting null here so that fuzzy match does not match this
            String streetNumber = "null";
            if (addressQueryResults.getJSONObject(i).has("streetName")) {
                streetName = addressQueryResults.getJSONObject(i).getString("streetName");
            }
            if (addressQueryResults.getJSONObject(i).has("streetNumber")) {
                streetNumber = addressQueryResults.getJSONObject(i).getString("streetNumber");
            }

            iriToBuildingMap.computeIfAbsent(buildingIri, OSMBuilding::new);
            iriToBuildingMap.get(buildingIri).setAddress(streetNumber, streetName);
        }

        String usageQuery;
        try (InputStream is = new ClassPathResource("osm_usage.sparql").getInputStream()) {
            usageQuery = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        JSONArray usageQueryResults = storeClient.executeQuery(usageQuery);

        for (int i = 0; i < usageQueryResults.length(); i++) {
            String buildingIri = usageQueryResults.getJSONObject(i).getString("building");
            String usage = usageQueryResults.getJSONObject(i).getString("usage");
            iriToBuildingMap.computeIfAbsent(buildingIri, OSMBuilding::new);
            iriToBuildingMap.get(buildingIri).setUsage(usage);
        }
    }

    /******************************************** */
    /*
     * Fuzzy match building address from outer data source (csv) and osm agent to
     * integrate floors data to citydb.buildilng
     */
    /* INPUT: data file location */
    /* floors data store in citydb.building.storeys_above_ground */
    /**********************************************/
    void matchAddress(String floorsCsv, Connection conn) {
        MatchService matchService = new MatchService();
        List<Document> preDoc = new ArrayList<>();

        iriToBuildingMap.values().stream().filter(b -> b.hasAddress()).forEach(building -> {
            String buildingIri = building.getBuildingIri();
            String osmStreet = building.getStreet();
            String osmUnit = building.getStreetNumber();
            Document preDocument = new Document.Builder(buildingIri)
                    .addElement(new Element.Builder<String>().setValue(osmUnit + " " + osmStreet)
                            .setType(ElementType.ADDRESS).createElement())
                    .createDocument();
            preDoc.add(preDocument);
        });

        // get data from csv
        List<FloorsCsv> hdbFloors;
        try {
            hdbFloors = new CsvToBeanBuilder<FloorsCsv>(new FileReader(floorsCsv)).withType(FloorsCsv.class).build()
                    .parse();
        } catch (IllegalStateException | FileNotFoundException e) {
            throw new RuntimeException("Error reading CSV", e);
        }

        // fuzzy match
        for (int i = 0; i < hdbFloors.size(); i++) {
            double pointScore = 0.0;
            String blk = hdbFloors.get(i).getBLK();
            String address = hdbFloors.get(i).getStreet();

            Document matchDoc = new Document.Builder(String.valueOf(i))
                    .addElement(new Element.Builder<String>().setValue(blk + " " + address).setType(ElementType.ADDRESS)
                            .createElement())
                    .setThreshold(0.8).createDocument();

            Map<String, List<Match<Document>>> resultPoint = matchService.applyMatchByDocId(matchDoc, preDoc);

            String matchedBuildingIri = null;
            String matched = null;
            String matchedWith = null;
            for (Map.Entry<String, List<Match<Document>>> entry : resultPoint.entrySet()) {
                for (Match<Document> match : entry.getValue()) {
                    if (match.getScore().getResult() > pointScore && match.getScore().getResult() > 0.6) {
                        pointScore = match.getScore().getResult();
                        matchedBuildingIri = match.getMatchedWith().getKey();
                        matched = match.getData().toString();
                        matchedWith = match.getMatchedWith().toString();
                    }
                }
            }

            // store floors data based on building iri from osm agent

            if (pointScore != 0) {
                int floors = hdbFloors.get(i).getFloors();
                LOGGER.debug("Point score = {}. {} matched with {}", pointScore, matched, matchedWith);
                OSMBuilding matchedBuilding = iriToBuildingMap.get(matchedBuildingIri);
                matchedBuilding.setFloorCategory(OSMBuilding.FloorCategory.A);
                matchedBuilding.setFloors(floors);

                String buildingUuid = extractUuid(matchedBuildingIri);
                FloorPostGISClient.insertData(buildingUuid, "A", floors, conn);
            }
        }
    }

    public void importFloorData(Connection conn) {
        // building_iri here is not the full IRI, just the last bit
        String buildingIriPrefix = "https://www.theworldavatar.com/kg/Building/";
        String floorSQLQuery = "SELECT storeys_above_ground, building.id, cg.strval " +
                "FROM citydb.building, citydb.cityobject_genericattrib cg " +
                "WHERE building.id = cg.cityobject_id AND cg.attrname = 'uuid'";

        try (Statement stmt = conn.createStatement()) {
            ResultSet floorsResults = stmt.executeQuery(floorSQLQuery);
            while (floorsResults.next()) {
                int floors = floorsResults.getInt("storeys_above_ground");
                String buildingUuid = floorsResults.getString("strval");
                String buildingIri = buildingIriPrefix + buildingUuid;
                iriToBuildingMap.computeIfAbsent(buildingIri, OSMBuilding::new);
                OSMBuilding building = iriToBuildingMap.get(buildingIri);

                if (building.getFloorCategory() != null && building.getFloorCategory() == OSMBuilding.FloorCategory.A) {
                    continue;
                }

                if (floors == 0) {
                    // osm option if previously not matched
                    floors = queryOSMFloor(buildingUuid, conn);
                    if (building.getFloorCategory() == null && floors > 0) {
                        iriToBuildingMap.computeIfAbsent(buildingIri, OSMBuilding::new);
                        building.setFloorCategory(OSMBuilding.FloorCategory.B);
                        building.setFloors(floors);
                        FloorPostGISClient.insertData(buildingUuid, "B", floors, conn);
                    }
                }

                if (floors == 0) {
                    floors = estimateFloors(buildingIri, conn);
                    iriToBuildingMap.computeIfAbsent(buildingIri, OSMBuilding::new);
                    building.setFloorCategory(OSMBuilding.FloorCategory.C);
                    building.setFloors(floors);

                    FloorPostGISClient.insertData(buildingUuid, "C", floors, conn);
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException("Error while querying for existing floor data", e);
        }

        LOGGER.info("Complete import floor data");
    }

    public int queryOSMFloor(String buildingIri, Connection conn) {
        String osmFloorQuery = "SELECT building_levels FROM osm.polygons " +
                "WHERE building_iri = '" + buildingIri + "'";
        int floors = 0;
        try (Statement stmt = conn.createStatement()) {
            ResultSet floorsResults = stmt.executeQuery(osmFloorQuery);
            while (floorsResults.next()) {
                String floorString = floorsResults.getString("building_levels");
                if (isInteger(floorString)) {
                    floors = Integer.parseInt(floorString);
                }
            }

        } catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        return floors;
    }

    private boolean isInteger(String str) {
        if (str == null) {
            return false;
        }
        try {
            Integer.parseInt(str);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    /********************************************************* */
    /* To estimate calculate height: */
    /*
     * 1. query building usage: Domestic: 1st floor 3.6m, the others 2.8m/floor;
     * Non-Domestic: 3.3m/floor
     */
    /* 2. no usage information: 3.3m/floor */
    /*********************************************************** */
    public int estimateFloors(String buildingIri, Connection conn) {
        int floor = 0;
        float height = 0;

        String heightQuery = "SELECT measured_height" +
                " FROM citydb.building, citydb.cityobject_genericattrib cg " +
                "WHERE building.id = cg.cityobject_id AND cg.attrname = 'iri' AND cg.urival = '" + buildingIri
                + "'";
        try (Statement stmt = conn.createStatement()) {
            ResultSet heightResults = stmt.executeQuery(heightQuery);
            while (heightResults.next()) {
                height = heightResults.getFloat("measured_height");
            }
        } catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }

        if (iriToBuildingMap.containsKey(buildingIri)) {
            OSMBuilding building = iriToBuildingMap.get(buildingIri);

            if (building.hasUsage()) {
                String usage = building.getUsage();
                if (usage.contentEquals(DOMESTIC_TYPE) || usage.contentEquals(RESIDENTIAL_TYPE)) {
                    floor = (int) ((height - 3.6) / 2.8 + (((height - 3.6) % 2.8 == 0) ? 0 : 1)) + 1;
                }
            }
        }

        if (floor == 0 && height > 0) {
            floor = (int) (height / 3.3 + ((height % 3.3 == 0) ? 0 : 1));
        }

        return floor;
    }

    boolean isValidURL(String url) {
        try {
            new URL(url).toURI();
            return true;
        } catch (MalformedURLException | URISyntaxException e) {
            return false;
        }
    }

    String extractUuid(String buildingIri) {
        try {
            // Create a URL object
            URL url = new URL(buildingIri);

            // Get the path of the URL
            String path = url.getPath();

            // Split the path by '/'
            String[] pathSegments = path.split("/");

            // The UUID is the last segment of the path
            return pathSegments[pathSegments.length - 1];
        } catch (Exception e) {
            throw new RuntimeException("Error extracting uuid from building IRI", e);
        }
    }
}
