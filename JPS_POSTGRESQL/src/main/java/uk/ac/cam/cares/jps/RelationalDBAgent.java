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

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.json.JSONStringer;
import org.postgresql.copy.CopyManager;
import org.postgresql.core.BaseConnection;

import uk.ac.cam.cares.crs.CRSTransformer;
/**
 * Servlet implementation class SimpleServlet
 */
@WebServlet(urlPatterns = {
		"/populateDB",
		"/getEntities",
		"/getEntitiesWithinRegion",
		"/emptyDB"
		})
public class RelationalDBAgent extends HttpServlet {
   
   
   @Override
   protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
	   String path = req.getServletPath();
//      resp.setContentType("text/plain");
//      resp.getWriter().write("Hello World! Maven Web Project Example.");
	   if ("/populateDB".equals(path)) {
		   System.out.println(RelationalDB.populateCoordinates());
	   } else if ("/getEntities".equals(path)) {
		   System.out.println(RelationalDB.getNumberOfEntities(1));
	   } else if ("/getEntitiesWithinRegion".equals(path)) {
		   
		   resp.setContentType("application/json");
		   JSONObject input = new JSONObject(req.getParameter("query"));
		   JSONObject region = input.getJSONObject("region");
		   int entitiesLimit = 25;
		   System.out.println(region.getJSONObject("lowercorner").get("lowerx"));
		   double xmin = Double.parseDouble(""+region.getJSONObject("lowercorner").get("lowerx"));
		   double xmax = Double.parseDouble(""+region.getJSONObject("uppercorner").get("upperx"));
		   double ymin = Double.parseDouble(""+region.getJSONObject("lowercorner").get("lowery"));
		   double ymax = Double.parseDouble(""+region.getJSONObject("uppercorner").get("uppery"));
		   double[] pmin = CRSTransformer.transform("EPSG:3857", "EPSG:4326", new double[] {xmin, ymin});
		   double[] pmax = CRSTransformer.transform("EPSG:3857", "EPSG:4326", new double[] {xmax, ymax});
		   xmin = pmin[0]; ymin = pmin[1]; xmax = pmax[0]; ymax = pmax[1];
		   System.out.println(pmin[0] + " " + pmin[1]);
		   System.out.println(pmax[0] + " " + pmax[1]);
		   
//		   JSONStringer result = RelationalDB.getIRIsOfEntitiesWithinRegion(103.8, 104, 1.28, 2.01);
		   JSONStringer result = RelationalDB.getIRIsOfEntitiesWithinRegion(xmin, xmax, ymin, ymax);
		   
		   resp.getWriter().write(result.toString());
	   } else if ("/emptyDB".equals(path)) {
		   System.out.println(RelationalDB.deleteCoordinates());
	   }
   }
   
   
}
