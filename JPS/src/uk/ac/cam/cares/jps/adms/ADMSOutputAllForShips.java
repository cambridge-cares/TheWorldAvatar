package uk.ac.cam.cares.jps.adms;


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
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
		int numpol = findHowManyPol(simulationResult, startcontentindex);  //number of polluttant (e.g:CO2,CO,NO2,..etc) with ozone and so2
		logger.info("number of pollutant= "+numpol);
		
		int heightamount=(simulationResult.get(0).length-startcontentindex)/numpol;//height variation level amount (e.g:0m,10m,20m,30m) currently 4
		
		
//		TODO IS IT NEEDED IN THE FUTURE TO CHANGE THE GST PHYSICALLY????
		ArrayList<String[]> copier=new ArrayList<String[]>();
		int totalline=simulationResult.size();
		//hardcoded = index 14,15,16 (numpol=10 then merged to 9), pollostmerging=1



		int pollostmerging = 1;
		int newarrsize = simulationResult.get(0).length - (heightamount * pollostmerging);// 43
		
		//the header of a modified file
		String[] newheader = new String[newarrsize];
		//column index 0-6 is the same between old and modified file
		for (int line = 0; line < startcontentindex; line++) {
			newheader[line] = simulationResult.get(0)[line];
		}
		int counter1 = 0;
		for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level

			for (int line1 = startcontentindex; line1 < 14; line1++) { // (col 7-13 which is normal)
				newheader[line1 + numpol * heightindex] = simulationResult.get(0)[line1 + numpol * heightindex
						+ counter1];
				int a = (line1 + numpol * heightindex) + counter1;
				System.out.println("index= " + a);
				System.out.println("a" + simulationResult.get(0)[line1 + numpol * heightindex + counter1]);
			}

			String headernamepm25 = simulationResult.get(0)[14 + numpol * heightindex];
			newheader[14 + numpol * heightindex] = headernamepm25.replace(headernamepm25.split("\\|")[2], "PM2.5");
			int b = 14 + numpol * heightindex;
			System.out.println("indexb= " + b);
			System.out.println("b" + headernamepm25.replace(headernamepm25.split("\\|")[2], "PM2.5"));

			String headernamepm10 = simulationResult.get(0)[16 + numpol * heightindex];
			newheader[15 + numpol * heightindex] = headernamepm10.replace(headernamepm10.split("\\|")[2], "PM10");
			int c = 15 + numpol * heightindex;
			System.out.println("indexc= " + c);
			System.out.println("c" + headernamepm10.replace(headernamepm10.split("\\|")[2], "PM10"));
			System.out.println("-----------------------------");

			counter1 += pollostmerging; // how many pol lost after merging (shifted in index array)

		}
		copier.add(newheader);
		//the content of a modified file
		for (int t = 1; t < totalline; t++) { // row
			String[] newcontent = new String[newarrsize];
			//column index 0-6 is the same between old and modified file
			for (int line = 0; line < startcontentindex; line++) {
				newcontent[line] = simulationResult.get(t)[line];
			}
			int counter = 0;
			for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level

				double pm25 = Double.valueOf(simulationResult.get(t)[14 + numpol * heightindex + counter])
						+ Double.valueOf(simulationResult.get(t)[15 + numpol * heightindex + counter]);//+index.....
				newcontent[14 + numpol * heightindex] = "" + pm25;

				double pm10 = Double.valueOf(simulationResult.get(t)[16 + numpol * heightindex + counter]) + pm25; //+index.....
				newcontent[15 + numpol * heightindex] = "" + pm10;

				for (int line1 = startcontentindex; line1 < 14; line1++) { // (col 7-13 which is normal)
					newcontent[line1 + numpol * heightindex] = simulationResult.get(t)[line1 + numpol * heightindex
							+ counter];
				}

				counter += pollostmerging; // how many pol lost after merging

			}
			copier.add(newcontent);
		}
		
		//make the json array to replace the functionality of gstreader.py
		JSONArray a = new JSONArray();
		for (int z = 0; z < heightamount; z++) {
			JSONArray h = new JSONArray();
			for (int y = 7; y < 16; y++) { // index0-index 9 for 1 pollutant
				JSONArray pol = new JSONArray();
				for (int x = 1; x < copier.size(); x++) { // 1-the last line
					pol.put(copier.get(x)[y + 9*z]);
				}
				h.put(pol);
			}
			a.put(h);
		}

		
		
		
		new QueryBroker().put(folder +"/testmod.levels.gst", MatrixConverter.fromArraytoCsv(copier));
//============================================================================================		
		
		
		
		/*ArrayList<String> args = new ArrayList<String>();
		System.out.println("================ output file ===============");
		System.out.println(outputFile);
		System.out.println("============================================");
		args.add("python");
		args.add("gstReader.py"); 
		args.add(outputFile);
		args.add(""+heightamount); 
		args.add(""+numpol);
		args.add("");
		
		String result = CommandHelper.executeCommands(targetFolder, args);*/
		//============================================================================================		
		 
		//logger.debug("=== Result === :" + result);
		response.setContentType("application/json");
		//response.getWriter().write(result);
		response.getWriter().write(a.toString());
	}

	public int findHowManyPol(List<String[]> simulationResult, int startcontentindex) {
		int sizeofpol2=startcontentindex;
		ArrayList<String> listofpol = new ArrayList<String>();	
			
		while (Double.valueOf(simulationResult.get(0)[sizeofpol2].split("Z=")[1].split("m")[0])
				- Double.valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {	
			
			String name=simulationResult.get(0)[sizeofpol2].split("\\|")[2];
			//System.out.println(name);
			sizeofpol2++;
			if (name.contains("PM10")) {
				listofpol.add("PM10");
			}
			else if (name.contains("PM2.5")) {
				listofpol.add("PM2.5");
			}
			else {
				listofpol.add(name);
			}
		}
	       List<String> newList = listofpol.stream().distinct().collect(Collectors.toList()); //remove duplication
		
	
		//int sizeofpol=(sizeofpol2-startcontentindex);
		int sizeofpol=newList.size();
		return sizeofpol;
	}
}
