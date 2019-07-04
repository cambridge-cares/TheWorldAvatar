package uk.ac.cam.cares.jps.postgresql;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.ResultSetMetaData;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.postgresql.copy.CopyManager;
import org.postgresql.core.BaseConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class RelationalDB {
    private static final long serialVersionUID = 1L;
    private static final String KEY_COLLECTION = "collection";
    private static final String KEY_ITEMS = "items";
    private static final String url = "jdbc:postgresql:adms_ships";
    private static final String user = "postgres";
    private static final String password = "postgres";
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

    public static long populateCoordinates() {
        long insertedRows = 0;

        try (Connection conn = connect()) {
            insertedRows = new CopyManager((BaseConnection) conn)
                    .copyIn("COPY coordinates FROM STDIN (FORMAT csv, HEADER)",
//			                new BufferedReader(new FileReader("C:\\Users\\WE\\PycharmProjects\\SPARQL\\ship\\ship-coordinates.csv"))	// in dev environment	
                            new BufferedReader(new FileReader("C:\\JPS_SHIP\\ship-coordinates.csv")) // on Claudius
                    );
            System.out.printf("%d row(s) inserted%n", insertedRows);
        } catch (SQLException ex) {
            logger.error(ex.getMessage(), ex);
            throw new JPSRuntimeException(ex.getMessage(), ex);
        } catch (ClassNotFoundException e) {
            logger.error(e.getMessage(), e);
            throw new JPSRuntimeException(e.getMessage(), e);
        } catch (FileNotFoundException e) {
            logger.error(e.getMessage(), e);
            throw new JPSRuntimeException(e.getMessage(), e);
        } catch (IOException e) {
            logger.error(e.getMessage(), e);
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return insertedRows;
    }

    public static int deleteCoordinates() {
        //@todo change query
        String SQL = "TRUNCATE TABLE coordinates";
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
        //@todo change query
        String SQL = "SELECT COUNT(*) FROM coordinates "
                + "WHERE classid = ?";
        int count = 0;

        try {
            conn = connect();
            PreparedStatement pstmt = conn.prepareStatement(SQL);
            pstmt.setInt(1, classID);
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
                "AND (lon BETWEEN ? AND ?) ";
        //@todo add timestamp to the query and potentialy other restrictions like size, weigh of the ship, etc

        try {
            conn = connect();
            PreparedStatement pstmt = conn.prepareStatement(SQL);
            pstmt.setDouble(1, ymin);
            pstmt.setDouble(2, ymax);
            pstmt.setDouble(3, xmin);
            pstmt.setDouble(4, xmax);
            results = preparedStatementResultsToJsonArray(pstmt);
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
    private static JSONStringer preparedStatementResultsToJsonArray(PreparedStatement pstmt) {
        JSONStringer results = new JSONStringer();

        try {
            ResultSet rs = pstmt.executeQuery();
            ResultSetMetaData rsmd = rs.getMetaData();
            results.object().key(KEY_COLLECTION).object().key(KEY_ITEMS);
            results.array();

            while (rs.next()) {
                int numColumns = rsmd.getColumnCount();
                JSONObject obj = new JSONObject();
                for (int i = 1; i <= numColumns; i++) {
                    String column_name = rsmd.getColumnName(i);
                    obj.put(column_name, rs.getObject(column_name));
                }
                results.value(obj);
            }
            results.endArray().endObject().endObject();

        } catch (SQLException ex) {
            logger.error(ex.getMessage(), ex);
            throw new JPSRuntimeException(ex.getMessage(), ex);
        }

        return results;
    }
}

