package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

/**
 * Servlet implementation class CoordinatesAndCityToADMSOutput
 */
@WebServlet("/CoordinatesAndCityToADMSOutput")
public class CoordinatesAndCityToADMSOutput extends HttpServlet {
	private static final long serialVersionUID = 1L;

	
    /**
     * @see HttpServlet#HttpServlet()
     */
    public CoordinatesAndCityToADMSOutput() {
        super();
     }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 

		// 1. Get city IRI and coordinates 
		// 2. Produces GST file and Buildings List
		String cityIRI = request.getParameter("cityIRI");
		double lowerx = Double.valueOf(request.getParameter("lowerx"));
		double lowery = Double.valueOf(request.getParameter("lowery"));
		double upperx = Double.valueOf(request.getParameter("upperx"));
		double uppery = Double.valueOf(request.getParameter("uppery"));
		
		
		String plantIRI = "";
		String sourceCRS = "";
		String targetCRS = "";
		
		
		
		double[] sourceCenter = new double[2];
		double[] targetCenter = new double[2];
 
		int buildingLimit = 25;
		if(cityIRI.contentEquals(BuildingQueryPerformer.BERLIN_IRI)) {			
			plantIRI = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
			sourceCRS = CRSTransformer.EPSG_25833; // Berlin
			sourceCenter = new double[]{392825, 5819122};
			targetCRS = CRSTransformer.EPSG_28992; // The Hague
			targetCenter = CRSTransformer.transform(sourceCRS, targetCRS, sourceCenter);
			double plantx = targetCenter[0];
			double planty = targetCenter[1];
			
			System.out.println("==================== Berlin ====================");
			System.out.println(cityIRI + "|" +  plantIRI + "|" + plantx + "|" + planty + "|" + buildingLimit + "|" + lowerx + "|" + lowery + "|" + upperx + "|" + uppery);
			
			try {
				response.getWriter().write(startIntegrationWithPython(cityIRI, plantIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery));
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		else
		{
			String city = BuildingQueryPerformer.THE_HAGUE_IRI;
			String plant = "http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl";
			double plantx = 79831;
			double planty = 454766;
			System.out.println("==================== THE HAGUE ====================");
			System.out.println(cityIRI + "|" +  plantIRI + "|" + plantx + "|" + planty + "|" + buildingLimit + "|" + lowerx + "|" + lowery + "|" + upperx + "|" + uppery);

			
			try {
				response.getWriter().write(startIntegrationWithPython(city, plant, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery));
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	
	private String startIntegrationWithPython(String cityIRI, String plantIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) throws InterruptedException {
		
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsMainNew.py"); 
		args.add(plantIRI);
		String coordintates = getCoordinatesForPython(lowerx, lowery, upperx, uppery);
		args.add(coordintates);
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		args.add(fullPath); // this extra parameter tells the python script where to put the input files
		String buildingData = retrieveBuildingDataInJSON(cityIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		buildingData = buildingData.replace('\"', '\'');
		args.add(buildingData);
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		String result = CommandHelper.executeCommands(targetFolder, args);
		System.out.println("fullpath= "+fullPath);
		System.out.println("cityiri= "+cityIRI);
		System.out.println("plantiri= "+plantIRI);
		return fullPath;
		
	}
	
	private String getCoordinatesForPython(double lowerx, double lowery, double upperx, double uppery) {
		String template = "{'xmin':%f, 'xmax':%f, 'ymin':%f, 'ymax':%f}";
		return String.format(template, lowerx, upperx, lowery, uppery);
	}
	
	private String retrieveBuildingDataInJSON(String cityIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(cityIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(cityIRI, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
}
