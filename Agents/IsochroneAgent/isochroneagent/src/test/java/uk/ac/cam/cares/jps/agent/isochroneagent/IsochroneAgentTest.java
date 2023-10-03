// package uk.ac.cam.cares.jps.agent.isochroneagent;

// import org.json.JSONArray;
// import org.json.JSONObject;
// import org.junit.jupiter.api.Test;

// import java.nio.file.Path;
// import java.util.Map;

// public class IsochroneAgentTest {

// private static final Path POI_PATH = Path.of("/inputs/15MSC/POIqueries");
// private static final Path EDGESTABLESQL_PATH =
// Path.of("/inputs/15MSC/edgesSQLTable");

// @Test
// public void uploadPOIsparqlTest() {
// FileReader.readPOIsparql(POI_PATH);
// }

// @Test
// public void uploadEdgesTableSQLTest() {
// // Assuming EdgesTableSQL is a Map<String, String>
// Map<String, String> EdgesTableSQL =
// FileReader.readEdgesTableSQL(EDGESTABLESQL_PATH);

// // Iterate through the entries and print them
// for (Map.Entry<String, String> entry : EdgesTableSQL.entrySet()) {
// String key = entry.getKey();
// String value = entry.getValue();
// System.out.println("Key: " + key + ", Value: " + value);
// }
// }
// }
