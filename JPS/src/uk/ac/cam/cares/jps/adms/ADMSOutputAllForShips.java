package uk.ac.cam.cares.jps.adms;


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

/**
 * Servlet implementation class ADMSOutput
 */
@WebServlet("/ADMSOutputAllForShips")
public class ADMSOutputAllForShips extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static Logger logger = LoggerFactory.getLogger(ADMSOutputAllForShips.class);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 * get all adms output in one go
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		
		JSONObject joforEN = AgentCaller.readJsonParameter(request);
		String folder = joforEN.getString("folder");
		
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		//String outputFile = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS/test.levels.gst";
		String outputFile = folder +"/test.levels.gst";
		String csv = new QueryBroker().readFile(outputFile);
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		

		
		
		int startcontentindex=7;
		int sizeofpol = findHowManyPol(simulationResult, startcontentindex);  //number of polluttant (e.g:CO2,CO,NO2,..etc) with ozone and so2
		logger.info("number of pollutant= "+sizeofpol);
		
		int heightamount=(simulationResult.get(0).length-startcontentindex)/sizeofpol;//height variation level amount (e.g:0m,10m,20m,30m) currently 4
		
		
//		TODO IS IT NEEDED IN THE FUTURE TO CHANGE THE GST PHYSICALLY????
//		ArrayList<String[]> copier=new ArrayList<String[]>();
//		String[]headerold=simulationResult.get(0);
//		int size=headerold.length;
//		String[]header=new String[size];
//		for(int w=0;w<size;w++) {
//			header[w]=headerold[w];
//		}
//		copier.add(simulationResult.get(0));
		
		
		
		
		ArrayList<String> args = new ArrayList<String>();
		System.out.println("================ output file ===============");
		System.out.println(outputFile);
		System.out.println("============================================");
		args.add("python");
		args.add("gstReader.py"); 
		args.add(outputFile);
		args.add(""+heightamount); 
		args.add(""+sizeofpol);
		args.add("");
		
		String result = CommandHelper.executeCommands(targetFolder, args);
		 
		logger.debug("=== Result === :" + result);
		response.setContentType("application/json");
		response.getWriter().write(result);
	}

	public int findHowManyPol(List<String[]> simulationResult, int startcontentindex) {
		int sizeofpol2=startcontentindex;
		ArrayList<String> listofpol = new ArrayList<String>();	
			
		while (Double.valueOf(simulationResult.get(0)[sizeofpol2].split("Z=")[1].split("m")[0])
				- Double.valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {	
			sizeofpol2++;
			listofpol.add(simulationResult.get(0)[sizeofpol2].split("\\|")[2]);
		}
	
		int sizeofpol=(sizeofpol2-startcontentindex);
		return sizeofpol;
	}
}
