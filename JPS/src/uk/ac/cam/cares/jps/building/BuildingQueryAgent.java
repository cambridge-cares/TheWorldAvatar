package uk.ac.cam.cares.jps.building;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@WebServlet(urlPatterns = {"/buildings/fromregion", "/buildings/simpleshape"})
public class BuildingQueryAgent extends HttpServlet {
	
	Logger logger = LoggerFactory.getLogger(BuildingQueryAgent.class);
	
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String path = req.getServletPath();
		logger.info("BuildingQueryAgent is called, path = " + path);

		BuildingQueryPerformer performer = new BuildingQueryPerformer();
		
		if ("/building/fromregion".equals(path)) {
			
			//performer.performQueryBuildingsFromRegion(cityIRI, buildingLimit, lowerx, lowery, upperx, uppery);

		} else if ("/building/simpleshape".equals(path)) {

		}
	}
}
