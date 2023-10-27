package uk.ac.cam.cares.jps.base.util;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import javax.sql.rowset.serial.SerialException;
import java.sql.Blob;
import java.sql.SQLException;
import java.util.Date;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


/**
 * This class contains unit tests for StoreClientHelper.
 *
 * @author Karthik
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class StoreClientHelperTest {

    /**
     * Check that convert method returns the expected JSONArray consisting of the right JSONObjects.
     */
    private java.sql.ResultSet rs;
    private java.sql.ResultSetMetaData rsmd ;
    private Blob rbl ;

    private double epsilon = 0.000001d;

    private java.sql.Array marr;

    @Before
    public void setUp() throws SerialException,SQLException {
        rs = mock(java.sql.ResultSet.class);
        rsmd = mock(java.sql.ResultSetMetaData.class);
        marr = mock(java.sql.Array.class);
        byte[] bytes = "A byte array".getBytes();
        rbl = new javax.sql.rowset.serial.SerialBlob(bytes);
    }

    @Test
    public void testconvert() throws SQLException, JSONException {

        when(rs.getMetaData()).thenReturn(rsmd);
        when(rsmd.getColumnCount()).thenReturn(13);
        when(rs.next()).thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(true)
                       .thenReturn(false);


        when(rsmd.getColumnName(1)).thenReturn("col1");
        when(rsmd.getColumnType(1)).thenReturn(java.sql.Types.BIGINT);
        when(rs.getInt("col1")).thenReturn(16789);

        when(rsmd.getColumnName(2)).thenReturn("col2");
        when(rsmd.getColumnType(2)).thenReturn(java.sql.Types.BOOLEAN);
        when(rs.getBoolean("col2")).thenReturn(true);

        when(rsmd.getColumnName(3)).thenReturn("col3");
        when(rsmd.getColumnType(3)).thenReturn(java.sql.Types.DOUBLE);
        when(rs.getDouble("col3")).thenReturn(2.5);

        when(rsmd.getColumnName(4)).thenReturn("col4");
        when(rsmd.getColumnType(4)).thenReturn(java.sql.Types.FLOAT);
        float x = 2.5f;
        when(rs.getFloat("col4")).thenReturn(x);

        when(rsmd.getColumnName(5)).thenReturn("col5");
        when(rsmd.getColumnType(5)).thenReturn(java.sql.Types.INTEGER);
        when(rs.getInt("col5")).thenReturn(16789);

        when(rsmd.getColumnName(6)).thenReturn("col6");
        when(rsmd.getColumnType(6)).thenReturn(java.sql.Types.TINYINT);
        when(rs.getInt("col6")).thenReturn(16789);

        when(rsmd.getColumnName(7)).thenReturn("col7");
        when(rsmd.getColumnType(7)).thenReturn(java.sql.Types.SMALLINT);
        when(rs.getInt("col7")).thenReturn(16789);

        when(rsmd.getColumnName(8)).thenReturn("col8");
        when(rsmd.getColumnType(8)).thenReturn(java.sql.Types.VARCHAR);
        when(rs.getString("col8")).thenReturn("test");


        when(rsmd.getColumnName(9)).thenReturn("col9");
        when(rsmd.getColumnType(9)).thenReturn(java.sql.Types.NVARCHAR);
        when(rs.getNString("col9")).thenReturn("test");

        when(rsmd.getColumnName(10)).thenReturn("col10");
        when(rsmd.getColumnType(10)).thenReturn(java.sql.Types.DATE);
        Date date = new Date();
        java.sql.Date sqlDate = new java.sql.Date(date.getTime());
        when(rs.getDate("col10")).thenReturn(sqlDate);

        when(rsmd.getColumnName(11)).thenReturn("col11");
        when(rsmd.getColumnType(11)).thenReturn(java.sql.Types.TIMESTAMP);
        java.sql.Timestamp sqlTime = new java.sql.Timestamp(date.getTime());
        when(rs.getTimestamp("col11")).thenReturn(sqlTime);

        when(rsmd.getColumnName(12)).thenReturn("col12");
        when(rsmd.getColumnType(12)).thenReturn(java.sql.Types.BLOB);
        when(rs.getBlob("col12")).thenReturn(rbl);

        when(rsmd.getColumnName(13)).thenReturn("col13");
        when(rsmd.getColumnType(13)).thenReturn(java.sql.Types.ARRAY);
        when(rs.getArray("col13")).thenReturn(marr);



        JSONArray json = StoreClientHelper.convert(rs);

        for (int i = 0; i < json.length(); i++) {
            JSONObject obj = json.getJSONObject(i);
            assertTrue(16789 == obj.getInt("col1"));
            assertTrue(true == obj.getBoolean("col2"));
            assertTrue(Math.abs(2.5 - obj.getDouble("col3")) < epsilon);
            assertTrue(Math.abs(2.5 - obj.getFloat("col4")) < epsilon);
            assertTrue(16789 == obj.getInt("col5"));
            assertTrue(16789 == obj.getInt("col6"));
            assertTrue(16789 == obj.getInt("col7"));
            assertTrue("test" == obj.getString("col8"));
            assertTrue("test" == obj.get("col9"));
            assertTrue(sqlDate == obj.get("col10"));
            assertTrue(sqlTime == obj.get("col11"));
            assertTrue(rbl == obj.get("col12"));
            assertTrue(marr == obj.get("col13"));

        }

    }

}