package uk.ac.cam.cares.jps.postgresql;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.json.JSONStringer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.crs.CRSTransformer;


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
	private Logger logger = LoggerFactory.getLogger(RelationalDBAgent.class);
   
   
   @Override
   protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
	   String path = req.getServletPath();
//      resp.setContentType("text/plain");
//      resp.getWriter().write("Hello World! Maven Web Project Example.");
	   if ("/populateDB".equals(path)) {
		   logger.info("The database is populated--- "+RelationalDB.populateCoordinates());
	   } else if ("/getEntities".equals(path)) {
		   logger.info("the number of entities exist= "+RelationalDB.getNumberOfEntities(1));
	   } else if ("/getEntitiesWithinRegion".equals(path)) {
		   logger.info("querying the entities iri");
		   
		   resp.setContentType("application/json");
		   JSONObject input = new JSONObject(req.getParameter("query"));
		   JSONObject region = input.getJSONObject("region");
		   System.out.println(region.getJSONObject("lowercorner").get("lowerx"));
		   double xmin = Double.parseDouble(""+region.getJSONObject("lowercorner").get("lowerx"));
		   double xmax = Double.parseDouble(""+region.getJSONObject("uppercorner").get("upperx"));
		   double ymin = Double.parseDouble(""+region.getJSONObject("lowercorner").get("lowery"));
		   double ymax = Double.parseDouble(""+region.getJSONObject("uppercorner").get("uppery"));
		   double[] pmin = CRSTransformer.transform("EPSG:3857", "EPSG:4326", new double[] {xmin, ymin});
		   double[] pmax = CRSTransformer.transform("EPSG:3857", "EPSG:4326", new double[] {xmax, ymax});
		   xmin = pmin[0]; ymin = pmin[1]; xmax = pmax[0]; ymax = pmax[1];
		   logger.info("minimum point= "+pmin[0] + " " + pmin[1]);
		   logger.info("maximum point= "+pmax[0] + " " + pmax[1]);
		   
//		   JSONStringer result = RelationalDB.getIRIsOfEntitiesWithinRegion(103.8, 104, 1.28, 2.01);
		   JSONStringer result = RelationalDB.getIRIsOfEntitiesWithinRegion(xmin, xmax, ymin, ymax);
		   
		   resp.getWriter().write(result.toString());
	   } else if ("/emptyDB".equals(path)) {
		   logger.info("it is emptied--- "+RelationalDB.deleteCoordinates());
	   }
   }
   
   
}

