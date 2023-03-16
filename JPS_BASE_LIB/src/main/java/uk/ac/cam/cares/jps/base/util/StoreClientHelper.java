package uk.ac.cam.cares.jps.base.util;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.sql.SQLException;

/**
 * Helper class designed to contain utility functions required by store clients
 * (namely, RemoteRDBStoreClient and RemoteStoreClient)
 *
 * @author Mehal Agarwal (ma988@cam.ac.uk)
 */

public final class StoreClientHelper {

    // All methods should be "static".
    // This private constructor added to prevent instantiation.
    private StoreClientHelper() {
    }

    /**
     * Converts query results from java.sql.ResultSet into JSONArray
     * 
     * @param rs query results in java.sql.ResultSet format
     * @return query results in JSONArray format
     * @throws SQLException
     * @throws JSONException
     */
    public static JSONArray convert(java.sql.ResultSet rs) throws SQLException, JSONException {
        JSONArray json = new JSONArray();
        java.sql.ResultSetMetaData rsmd = rs.getMetaData();
        while (rs.next()) {
            int numColumns = rsmd.getColumnCount();
            JSONObject obj = new JSONObject();
            for (int i = 1; i <= numColumns; i++) {
                final Object value;
                String columnName = rsmd.getColumnName(i);
                switch (rsmd.getColumnType(i)) {
                    case java.sql.Types.ARRAY:
                        value = rs.getArray(columnName);
                        break;
                    case java.sql.Types.BOOLEAN:
                        value = rs.getBoolean(columnName);
                        break;
                    case java.sql.Types.BLOB:
                        value = rs.getBlob(columnName);
                        break;
                    case java.sql.Types.DOUBLE:
                        value = rs.getDouble(columnName);
                        break;
                    case java.sql.Types.FLOAT:
                        value = rs.getFloat(columnName);
                        break;
                    case java.sql.Types.INTEGER:
                    case java.sql.Types.TINYINT:
                    case java.sql.Types.SMALLINT:
                    case java.sql.Types.BIGINT:
                        value = rs.getInt(columnName);
                        break;
                    case java.sql.Types.NVARCHAR:
                        value = rs.getNString(columnName);
                        break;
                    case java.sql.Types.VARCHAR:
                        value = rs.getString(columnName);
                        break;
                    case java.sql.Types.DATE:
                        value = rs.getDate(columnName);
                        break;
                    case java.sql.Types.TIMESTAMP:
                        value = rs.getTimestamp(columnName);
                        break;
                    default:
                        value = rs.getObject(columnName);
                        break;
                }
                obj.put(columnName, value);
            }
            json.put(obj);
        }
        return json;
    }
}
