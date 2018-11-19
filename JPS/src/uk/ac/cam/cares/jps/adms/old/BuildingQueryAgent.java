package uk.ac.cam.cares.jps.adms.old;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

@WebServlet(urlPatterns = {"/buildings/fromregion", "/buildings/simpleshape"})
public class BuildingQueryAgent extends HttpServlet {

	private static final long serialVersionUID = 1755249235044182867L;
	Logger logger = LoggerFactory.getLogger(BuildingQueryAgent.class);
	
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String path = req.getServletPath();
		logger.info("BuildingQueryAgent is called, path = " + path);

		BuildingQueryPerformer performer = new BuildingQueryPerformer();
		
		if ("/buildings/fromregion".equals(path)) {
			
			String cityIRI = req.getParameter("cityiri");
 
			int buildingLimit = Integer.valueOf(req.getParameter("buildinglimit"));
			double lowerx = Double.valueOf(req.getParameter("lowerx"));
			double lowery = Double.valueOf(req.getParameter("lowery"));
			double upperx = Double.valueOf(req.getParameter("upperx"));
			double uppery = Double.valueOf(req.getParameter("uppery"));
			
			double plantx = 79831;
			double planty = 454766;
			
			if(cityIRI.equalsIgnoreCase(BuildingQueryPerformer.THE_HAGUE_IRI)) {
				plantx = 79831;
				planty = 454766;
			}
			else {
				String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
				double[] sourceCenter = new double[]{392825, 5819122};
				String targetCRS = CRSTransformer.EPSG_28992; // The Hague
				double[] targetCenter = CRSTransformer.transform(sourceCRS, targetCRS, sourceCenter);
				plantx = targetCenter[0];
				planty = targetCenter[1];
			}
			
//			System.out.println("==============================");
//			System.out.println("city: " + cityIRI);
//			System.out.println(plantx + "| "+ planty + "| " + lowerx + "| "+ lowery + "| "+ upperx + "|" + uppery);
//			System.out.println("==============================");

			//List<String> buildingIRIs = performer.performQueryBuildingsFromRegion(cityIRI, buildingLimit, lowerx, lowery, upperx, uppery);
			List<String> buildingIRIs = performer.performQueryClosestBuildingsFromRegion(cityIRI, plantx, planty, 25, lowerx, lowery, upperx, uppery);
			System.out.println("=============== building List selected ===============");
			System.out.println(buildingIRIs.size());
			String message = new Gson().toJson(buildingIRIs);
			System.out.println("Direct printout: " + buildingIRIs);
			System.out.println("Direct printout to string: " + buildingIRIs.toString());
			for(String iri : buildingIRIs)
			{
				System.out.println(iri);
			}
			System.out.println("The message : " + message);
			System.out.println("======================================================");
			print(resp, message);
			
		} else if ("/buildings/simpleshape".equals(path)) {

			String cityIRI = req.getParameter("cityiri");
			String buildingIRIsAsString = req.getParameter("buildingiris");
			List<String> buildingIRIs = new Gson().fromJson(buildingIRIsAsString, List.class);
			SimpleBuildingData data = performer.performQuerySimpleBuildingData(cityIRI, buildingIRIs);
			String message = new Gson().toJson(data);
			print(resp, message);
		}
	}
	
	private void print(HttpServletResponse resp, String message) throws IOException {
		//TODO-AE just AgentCaller.printToResponse
		resp.setContentType("text/plain");
		resp.setCharacterEncoding("UTF-8");
		logger.debug(message);
		resp.getWriter().print(message);
	}
}
