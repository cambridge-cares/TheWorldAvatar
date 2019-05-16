package uk.ac.cam.cares.jps;

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
import java.util.ArrayList;
import java.util.List;

import org.json.JSONStringer;
import org.postgresql.copy.CopyManager;
import org.postgresql.core.BaseConnection;

public class RelationalDB {
	private static final long serialVersionUID = 1L;   
	   private static final String url = "jdbc:postgresql:adms_ships";
	   private static final String user = "postgres";
	   private static final String password = "password";
	   
	   /**
	    * Connect to the PostgreSQL database
	    *
	    * @return a Connection object
	    */
	   public static Connection connect() throws SQLException, ClassNotFoundException{
	    
	   	   Class.forName("org.postgresql.Driver");
	       return DriverManager.getConnection(url, user, password);

//	       return conn;
	   }
	   
	protected static long populateCoordinates() {
		   long insertedRows = 0;
		   
		   try (Connection conn = connect()) {
			   insertedRows = new CopyManager((BaseConnection) conn)
			            .copyIn(
			                "COPY coordinates FROM STDIN (FORMAT csv, HEADER)", 
//			                new BufferedReader(new FileReader("C:\\Users\\WE\\PycharmProjects\\SPARQL\\ship\\ship-coordinates.csv"))	// in dev environment	
			                new BufferedReader(new FileReader("C:\\JPS_SHIP\\ship-coordinates.csv"))	// on Claudius
			                );
			    System.out.printf("%d row(s) inserted%n", insertedRows);		   
		   } catch (SQLException ex) {
			   System.out.println(ex.getMessage());
		   } catch (ClassNotFoundException e) {
			   e.printStackTrace();
		   } catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		   return insertedRows;
	   }
	   
	   protected static int deleteCoordinates() {
		   String SQL = "TRUNCATE TABLE coordinates";
		   int affectedRows = 0;
		   
		   try (Connection conn = connect();
				   Statement stmt = conn.createStatement()) {
			   affectedRows = stmt.executeUpdate(SQL);		   
		   } catch (SQLException ex) {
			   System.out.println(ex.getMessage());
		   } catch (ClassNotFoundException e) {
			   e.printStackTrace();
		   }
		   return affectedRows;
	   }
	   
	   protected static int getNumberOfEntities(int classID) {
		   Connection conn = null;
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
	           System.out.println(ex.getMessage());
	       } catch (ClassNotFoundException e) {
	    	   e.printStackTrace();
	       } finally {
	    	   try {
	    		   if (conn != null && !conn.isClosed()) {
	    			   conn.close();
	    		   }
	    	   } catch (SQLException ex) {
	    		   ex.printStackTrace();
	    	   }
	       }
	       return count;
	   }
	   
	   protected static JSONStringer getIRIsOfEntitiesWithinRegion(double xmin, double xmax, double ymin, double ymax) {
		   JSONStringer results = new JSONStringer();
		   results.object()
	   			.key("shipIRIs").array();
		   
		   Connection conn = null;
		   String SQL = "SELECT instanceiri FROM coordinates " + 
				   "WHERE (latlong[0] BETWEEN ? AND ?) " + 
				   "AND (latlong[1] BETWEEN ? AND ?) " +
				   "AND classid=1";

	       try {
	    	   conn = connect();
	           PreparedStatement pstmt = conn.prepareStatement(SQL);
	           pstmt.setDouble(1, ymin);
	           pstmt.setDouble(2, ymax);
	           pstmt.setDouble(3, xmin);
	           pstmt.setDouble(4, xmax);
	           ResultSet rs = pstmt.executeQuery();
	           
	           while (rs.next()) {
	        	   results.value(rs.getString("instanceiri"));
	           }
	       } catch (SQLException ex) {
	           System.out.println(ex.getMessage());
	       } catch (ClassNotFoundException e) {
	    	   e.printStackTrace();
	       } finally {
	    	   try {
	    		   if (conn != null && !conn.isClosed()) {
	    			   conn.close();
	    		   }
	    	   } catch (SQLException ex) {
	    		   ex.printStackTrace();
	    	   }
	       }
	       
	       results.endArray().endObject();
	       
		   return results;
	   }
}
