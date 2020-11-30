package uk.ac.cam.cares.jps.postgresql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

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

        Instant instant = Instant.now();
        long seconds = instant.getEpochSecond();
        long epoch_back = seconds - SHIP_QUERY_INTERVAL_MINUTES_BACK * 60;

        JSONStringer results = getEntitiesWithinRegionAndTimestamp(xmin, xmax, ymin, ymax, epoch_back, seconds);
        JSONObject resultsObj = new JSONObject(results.toString());

        //QUICK PATCH FOR THE Mid-Term-Review TO SHOW ANY SHIP DATA
        if (resultsObj.has(KEY_COLLECTION)) {
            JSONObject entities = resultsObj.getJSONObject(KEY_COLLECTION);
            if (entities.has(KEY_ITEMS)) {
                JSONArray items = entities.getJSONArray(KEY_ITEMS);
                if (items.isEmpty()) {
                    //Try querying for entities within the region 250 days and 1 hour from now
                    seconds = seconds - 21600000;
                    epoch_back = seconds - SHIP_QUERY_INTERVAL_MINUTES_BACK * 60;
                    results = getEntitiesWithinRegionAndTimestamp(xmin, xmax, ymin, ymax, epoch_back, seconds);
                }
            }
        }

        checkResults(new JSONObject(results.toString()));
        
        return results;
    }

    private static JSONStringer getEntitiesWithinRegionAndTimestamp(double xmin, double xmax, double ymin, double ymax,  long epoch_back, long epoch_to) {
        JSONStringer results;
        Connection conn = null;
        String SQL = getSQLTemplate();

        try {
            conn = connect();
            PreparedStatement pstmt = conn.prepareStatement(SQL);
            pstmt.setDouble(1, ymin);
            pstmt.setDouble(2, ymax);
            pstmt.setDouble(3, xmin);
            pstmt.setDouble(4, xmax);
            pstmt.setLong(5, epoch_back);
            pstmt.setLong(6, epoch_back);
            pstmt.setLong(7, epoch_to);
            pstmt.setLong(8, epoch_to);
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

    private static String getSQLTemplate() {

        String SQL = "SELECT * FROM ship " +
                "INNER JOIN ship_details sd ON ship.mmsi = sd.ship_mmsi " +
                "WHERE (lat BETWEEN ? AND ?) " +
                "AND (lon BETWEEN ? AND ?) " +
                "AND (ts >= ? or tst >= ?) " +
                "AND (ts <= ? or tst <= ?) " +
                "ORDER BY ss DESC, al DESC, aw DESC " +
                "LIMIT 300";

        return SQL;
    }
    
    private static void checkResults(JSONObject results) {
    	
    	System.out.println("checking the ship results set ...");
    	
    	try {
    	
	        String KEY_LAT = "lat";
	        String KEY_LON = "lon";
	        String KEY_MMSI = "mmsi";
	        
	        Map<String, Integer> mmsiCount= new HashMap<String, Integer>();
	        int maxDuplicationNumber = 1;
	        if (results.has(KEY_COLLECTION)) {
	            JSONObject entities = results.getJSONObject(KEY_COLLECTION);
	            if (entities.has(KEY_ITEMS)) {
	                JSONArray items = entities.getJSONArray(KEY_ITEMS);
	                for (Iterator<Object> i = items.iterator(); i.hasNext();) {
	                    JSONObject item = (JSONObject) i.next();
	                    if (item.has(KEY_MMSI)) {
	                    	
	                    	String mmsi = "" + item.getLong(KEY_MMSI);
	                    	Integer count = mmsiCount.get(mmsi);
	                    	if (count == null) {
	                    		mmsiCount.put(mmsi, 1);
	                    	} else {
	                    		count = count + 1;
	                    		mmsiCount.put(mmsi, count);
	                    		if (count > maxDuplicationNumber) {
	                    			maxDuplicationNumber = count;
	                    		}
	                    	}
	                    	
	                    } else {
	                    	System.out.println("WARNING: no mmsi found for result entry = " + item);
	                    }
	                }
	            }
	        }
	        
	        if (maxDuplicationNumber > 2) {
	        	System.out.println("WARNING: the ship result set contains a high number of duplicates, maxDuplicationNumber = " + maxDuplicationNumber);
	        	for (String key : mmsiCount.keySet()) {
	        		Integer count = mmsiCount.get(key);
	        		if (count > 2) {
	        			System.out.println("\t" + key + ", count=" + count);
	        		}
	        	}
	        } else {
	        	System.out.println("Ship results set is OK, maxDuplicationNumber = " + maxDuplicationNumber);
	        }

    	} catch (Exception exp) {
    		System.out.println("Oops, an exepction happened during checking the result for the ship query.");
    		System.out.println(exp);
    		exp.printStackTrace();
    	}
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

