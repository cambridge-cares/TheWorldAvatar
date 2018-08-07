package uk.ac.cam.cares.jps.building;

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
			List<String> buildingIRIs = performer.performQueryBuildingsFromRegion(cityIRI, buildingLimit, lowerx, lowery, upperx, uppery);
			String message = new Gson().toJson(buildingIRIs);
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
		resp.setContentType("text/plain");
		resp.setCharacterEncoding("UTF-8");
		logger.debug(message);
		resp.getWriter().print(message);
	}
}
