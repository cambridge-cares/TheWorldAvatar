package uk.ac.cam.cares.jps.postgresql;

import org.json.JSONObject;
import org.json.JSONStringer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.sql.*;
import java.time.Instant;
import java.util.ArrayList;

//@todo: use query builder to build queries (there are many reasons for that)
public class RelationalDB {
    private static final String KEY_COLLECTION = "collection";
    private static final String KEY_ITEMS = "items";
    private static final String KEY_MMSI = "mmsi";
    private static final String url = "jdbc:postgresql:adms_ships";
    private static final String user = "postgres";
    private static final String password = "postgres";
    private static final long SHIP_QUERY_INTERVAL_MINUTES_BACK = 60;
    private static Logger logger = LoggerFactory.getLogger(RelationalDB.class);

    /**
     * Connect to the PostgreSQL database
     *
     * @return a Connection object
     */
    public static Connection connect() throws SQLException, ClassNotFoundException {

        Class.forName("org.postgresql.Driver");
        return DriverManager.getConnection(url, user, password);

//	       return conn;
    }


    public static int deleteCoordinates() {
        String SQL = "TRUNCATE TABLE ship_details";
        int affectedRows = 0;

        try (Connection conn = connect();
             Statement stmt = conn.createStatement()) {
            affectedRows = stmt.executeUpdate(SQL);
        } catch (SQLException ex) {
            logger.error(ex.getMessage(), ex);
            throw new JPSRuntimeException(ex.getMessage(), ex);
        } catch (ClassNotFoundException e) {
            logger.error(e.getMessage(), e);
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return affectedRows;
    }

    public static int getNumberOfEntities(int classID) {
        Connection conn = null;

        String SQL = "SELECT COUNT(*) FROM ship ";

        int count = 0;

        try {
            conn = connect();
            PreparedStatement pstmt = conn.prepareStatement(SQL);
            ResultSet rs = pstmt.executeQuery();
            rs.next();
            count = rs.getInt(1);
        } catch (SQLException ex) {
            logger.error(ex.getMessage(), ex);
            throw new JPSRuntimeException(ex.getMessage(), ex);
        } catch (ClassNotFoundException e) {
            logger.error(e.getMessage(), e);
            throw new JPSRuntimeException(e.getMessage(), e);
        } finally {
            try {
                if (conn != null && !conn.isClosed()) {
                    conn.close();
                }
            } catch (SQLException ex) {
                logger.error(ex.getMessage(), ex);
                throw new JPSRuntimeException(ex.getMessage(), ex);
            }
        }
        return count;
    }

    /**
     * Gets collection of entities within a region and returns them in form of Json array.
     *
     * @param xmin
     * @param xmax
     * @param ymin
     * @param ymax
     * @return JSONStringer results
     */
    public static JSONStringer getEntitiesWithinRegion(double xmin, double xmax, double ymin, double ymax) {
        JSONStringer results;
        Connection conn = null;

        String SQL = "SELECT * FROM ship " +
                "INNER JOIN ship_details sd ON ship.mmsi = sd.ship_mmsi " +
                "WHERE (lat BETWEEN ? AND ?) " +
                "AND (lon BETWEEN ? AND ?) " +
                //"AND (ts >= ? or tst >= ?) " +
                "ORDER BY ss DESC, al DESC, aw DESC " +
                "LIMIT 300";

        try {
            Instant instant = Instant.now();
            long seconds = instant.getEpochSecond();
            long epoch_back = seconds - SHIP_QUERY_INTERVAL_MINUTES_BACK * 60;
            conn = connect();
            PreparedStatement pstmt = conn.prepareStatement(SQL);
            pstmt.setDouble(1, ymin);
            pstmt.setDouble(2, ymax);
            pstmt.setDouble(3, xmin);
            pstmt.setDouble(4, xmax);
           // pstmt.setLong(5, epoch_back);
           // pstmt.setLong(6, epoch_back);
            results = preparedStatementResultsToJsonArray(pstmt, true, KEY_MMSI);
        } catch (SQLException ex) {
            logger.error(ex.getMessage(), ex);
            throw new JPSRuntimeException(ex.getMessage(), ex);
        } catch (ClassNotFoundException e) {
            logger.error(e.getMessage(), e);
            throw new JPSRuntimeException(e.getMessage(), e);
        } finally {
            try {
                if (conn != null && !conn.isClosed()) {
                    conn.close();
                }
            } catch (SQLException ex) {
                logger.error(ex.getMessage(), ex);
                throw new JPSRuntimeException(ex.getMessage(), ex);
            }
        }

        return results;
    }

    /**
     * Retrieves results of prepared statement and puts them, one by one, into Json array.
     *
     * @param pstmt PreparedStatement
     * @return JSONStringer results
     */
    private static JSONStringer preparedStatementResultsToJsonArray(PreparedStatement pstmt,
                                                                    Boolean removeDupes, String dupColumn) {
        JSONStringer results = new JSONStringer();

        try {
            ResultSet rs = pstmt.executeQuery();
            ResultSetMetaData rsmd = rs.getMetaData();
            results.object().key(KEY_COLLECTION).object().key(KEY_ITEMS);
            results.array();
            ArrayList dupesRef = new ArrayList<>();

            while (rs.next()) {
                int numColumns = rsmd.getColumnCount();
                JSONObject obj = new JSONObject();
                Boolean dupe = false;
                for (int i = 1; i <= numColumns; i++) {
                    String column_name = rsmd.getColumnName(i);
                    Object column_value = rs.getObject(column_name);
                    if (removeDupes && column_name.equals(dupColumn)) {
                        if (!dupesRef.contains(column_value)) {
                            obj.put(column_name, column_value);
                            dupesRef.add(column_value);
                        } else {
                            dupe = true;
                            break;
                        }
                    } else {
                        obj.put(column_name, rs.getObject(column_name));
                    }
                }
                if (!dupe) {
                    results.value(obj);
                }
            }
            results.endArray().endObject().endObject();

        } catch (SQLException ex) {
            logger.error(ex.getMessage(), ex);
            throw new JPSRuntimeException(ex.getMessage(), ex);
        }

        return results;
    }


}

