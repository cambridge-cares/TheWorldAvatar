package uk.ac.cam.cares.jps.agent.isochroneagent;

import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;


public class NearestNode {
    public static void segmentization(RemoteRDBStoreClient remoteRDBStoreClient, double segmentization_length){
                try (Connection connection = remoteRDBStoreClient.getConnection()) {
                String segmentization_create_table="DROP TABLE IF EXISTS routing_ways_segment;\n" +
                "\n" +
                "-- Create a new table with the same structure as the old table\n" +
                "CREATE TABLE routing_ways_segment AS\n" +
                "SELECT *\n" +
                "FROM routing_ways\n" +
                "WHERE 1 = 0; -- This ensures that the new table has the same structure but no data\n" +
                "\n";
                String segmentization_split="-- Insert data into the new table with a split \"the_geom\" column\n" +
                        "INSERT INTO routing_ways_segment\n" +
                        "SELECT\n" +
                        "    gid,\n" +
                        "    osm_id,\n" +
                        "    tag_id,\n" +
                        "    length,\n" +
                        "    length_m,\n" +
                        "    name,\n" +
                        "    source,\n" +
                        "    target,\n" +
                        "    source_osm,\n" +
                        "    target_osm,\n" +
                        "    cost,\n" +
                        "    reverse_cost,\n" +
                        "    cost_s,\n" +
                        "    reverse_cost_s,\n" +
                        "    rule,\n" +
                        "    one_way,\n" +
                        "    oneway,\n" +
                        "    x1,\n" +
                        "    y1,\n" +
                        "    x2,\n" +
                        "    y2,\n" +
                        "    maxspeed_forward,\n" +
                        "    maxspeed_backward,\n" +
                        "    priority,\n" +
                        "    (ST_DumpSegments(ST_Segmentize(the_geom, 0.0005))).geom AS the_geom -- Split the \"the_geom\" column\n" +
                        "FROM\n" +
                        "    routing_ways;\n";
                String segmentization_rearrange_sql=
                "-- Step 1: Create a temporary sequence\n" +
                "CREATE SEQUENCE temp_sequence;\n" +
                "\n" +
                "-- Step 2: Update the gid column with new values from the sequence\n" +
                "UPDATE routing_ways_segment\n" +
                "SET gid = nextval('temp_sequence');\n" +
                "\n" +
                "-- Step 3: Reset the sequence to the next available value\n" +
                "SELECT setval('temp_sequence', (SELECT max(gid) FROM routing_ways_segment) + 1);\n" +
                "\n" +
                "-- Step 4: Drop the temporary sequence (optional)\n" +
                "DROP SEQUENCE temp_sequence;\n" +
                "\n" +
                "SELECT pgr_createTopology('routing_ways_segment', 0.000001, 'the_geom', 'gid', 'source', 'target', clean := true);";
                remoteRDBStoreClient.executeUpdate(segmentization_create_table);
                System.out.println("Created segmentization table.");
                remoteRDBStoreClient.executeUpdate(segmentization_split);
                System.out.println("Split ways successfully.");
                }
                catch (Exception e) {
                    e.printStackTrace();
                    throw new JPSRuntimeException(e);
                }

                try (Connection connection = remoteRDBStoreClient.getConnection()) {

                String segmentization_rearrange_sql=
                "-- Step 1: Create a temporary sequence\n" +
                "CREATE SEQUENCE temp_sequence;\n" +
                "\n" +
                "-- Step 2: Update the gid column with new values from the sequence\n" +
                "UPDATE routing_ways_segment\n" +
                "SET gid = nextval('temp_sequence');\n" +
                "\n" +
                "-- Step 3: Reset the sequence to the next available value\n" +
                "SELECT setval('temp_sequence', (SELECT max(gid) FROM routing_ways_segment) + 1);\n" +
                "\n" +
                "-- Step 4: Drop the temporary sequence (optional)\n" +
                "DROP SEQUENCE temp_sequence;\n";
                remoteRDBStoreClient.executeUpdate(segmentization_rearrange_sql);
                System.out.println("Temp sequence created.");
                remoteRDBStoreClient.executeUpdate("SELECT pgr_createTopology('routing_ways_segment', 0.000001, 'the_geom', 'gid', 'source', 'target', clean := true);");
                System.out.println("Table rearranged.");
                System.out.println("Segmentization completed. Routing_ways_segment table created.");
                }
                catch (Exception e) {
                    e.printStackTrace();
                    throw new JPSRuntimeException(e);
                }
    }

    public static void insertPoiData(RemoteRDBStoreClient remoteRDBStoreClient, JSONArray jsonArray) {


        try (Connection connection = remoteRDBStoreClient.getConnection()) {

            String initialiseTable = "CREATE TABLE IF NOT EXISTS poi_nearest_node ("
            + "poi_iri VARCHAR, "
            + "poi_type VARCHAR, "
            + "nearest_node BIGINT"
            + "geometry geom, "
            + ")";
        
            remoteRDBStoreClient.execute(initialiseTable);

            String sql = "INSERT INTO your_table_name (poi_iri, poi_type, geometry) VALUES (?, ?, ?)";
            PreparedStatement preparedStatement = connection.prepareStatement(sql);

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject poi = jsonArray.getJSONObject(i);
                String poiIri = poi.getString("poi_iri");
                String poiType = poi.getString("poi_type");
                String geometry = poi.getString("geometry");

                preparedStatement.setString(1, poiIri);
                preparedStatement.setString(2, poiType);
                preparedStatement.setString(3, geometry);
                preparedStatement.addBatch();
            }
            preparedStatement.executeBatch();

            System.out.println("Table poi_nearest_node created.");
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }


    }

}
