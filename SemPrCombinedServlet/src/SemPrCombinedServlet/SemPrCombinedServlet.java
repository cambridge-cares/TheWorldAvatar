/*
 * this PWServlet listens (doPost method) to the httpRequest sent from JParkSim (delivered by TomCat), 
 * and then implements the associated model, gives a result, and then visualizes to the user interface (through ArcGIS database at the moment)
 * and send back httpRespond as well.
 */

package SemPrCombinedServlet;


import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import SemPrCombinedServlet.runpower;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class SemPrCombinedServlet extends HttpServlet {
		
	private static final long serialVersionUID = 1L;
	public static long start_time;
	public static long end_time;
	public static long interval;
	
	public static ArrayList<String[]> editStack;
	

	// reverse mapping
 	public static String httpReqCSV = new String("Y:/httpReq.CSV"); // (mjk, 151115) differentiating function calls "Run PowerWorld" and "Run parameterised PW"
 	
	
	/**this is the main method of this servlet which is used to listen to HttpRequest, excute the corresponding model and send back HttpRsponse */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		/**reconstructing editStack from request string so that the information can be passed on to the relevant methods*/
		System.out.println("hello all");
		ArrayList<String[]> editStack = new ArrayList<String[]>(); 
		String[] layers = request.getParameter("layers").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(",");     // OBJECTID indicating parameter of which unit in chemical process has been changed
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // (mjk, 151115) adding flag indicating which  function has been called: PowerWorld,  parameterised  PW, AspenPlus, parameterised AP
		
		ArrayList<String> layer = new ArrayList<String>();

		for (int i = 0; i < layers.length; i++) {
			editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i]});
		}
		
		for(String[]thing : editStack)
		{
			layer.add(thing[0]);
		}
		
 
		/**check whether the httpRequst has been correctly recieved */
		FileWriter flag1 = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
		flag1 = new FileWriter(httpReqCSV);
		flag1.append("layers=" + layers[0]);
		flag1.append(", OBJECTIDs=" + OBJECTIDs[0]);
		flag1.append(", appCallFlag=" + appCallFlag[0]);
		flag1.flush();
		flag1.close(); // (mjk, 151115) writing this file works fine.
		
		System.out.println("good in here");
		System.out.println("number of layer editted=" + layer.size());
		
		/**the following part of code distinguishes which functionality of the JParkSimulator has been called, and then provides the corresponding service by evaluating the associated model */
		
		if (appCallFlag[0].equals("PW"))
		{
			System.out.println(appCallFlag[0] + " button was pressed! (doPOST)");
		start_time = System.currentTimeMillis();
		runpower run = new runpower();
		run.runSemPW(editStack);  // run Power World model when run PowerWorld Button was pressed                                             
		end_time = System.currentTimeMillis();
		System.out.println("runPowerWorld takes: "+(end_time-start_time));
		}
		if (appCallFlag[0].equals("PWPr"))
		{
				
		for (int x=0 ; x<layer.size() ;x++)
		{
					
		switch (layers[x]) {		
		
		case "EnergyStorage":
			System.out.println(layers[x] + " layer was editted! (doPOST)");
			start_time = System.currentTimeMillis();
			runpower run = new runpower();
			run.runPrPowerWorld1(editStack); 						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;
	
		case "Loadpoint":
			System.out.println(layers[x] + " layer was editted!! (doPOST)");
			start_time = System.currentTimeMillis();
			runpower run2 = new runpower();
			run2.runPrPowerWorld2(editStack); 						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;
			
		case "WindTurbine":
			System.out.println(layers[x] + " layer was editted!!! (doPOST)");
			start_time = System.currentTimeMillis();
			runpower run3 = new runpower();
			run3.runPrPowerWorld3(editStack); 						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;
		case "PVfarm":
			System.out.println(layers[x] + " layer was editted!!!! (doPOST)");
			start_time = System.currentTimeMillis();
			runpower run4 = new runpower();
			run4.runPrPowerWorld4(editStack);						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;
		case "Marinefarm":
			System.out.println(layers[x] + " layer was editted!!!!! (doPOST)");
			start_time = System.currentTimeMillis();
			runpower run5 = new runpower();
			run5.runPrPowerWorld5(editStack);						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;
			
		default:
			break;
		} 
		}
		}
	} // of doPost()

	//allows manual updating using a browser e.g. entering  http://localhost:8080/PWServlet/?layers=Load_Points&FIDs=103
/*	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ArrayList<String[]> editStack = new ArrayList<String[]>();
		String[] layers = request.getParameter("layers").split(",");
		// String[] FIDs = request.getParameter("OBJECTIDs").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(","); // ZL-151209 OBJECTID indicating parameter of which unit in chemical process has been changed
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // (mjk, 151115) adding flag indicating which function has been called: PowerWorld, parameterised PW, AspenPlus, parameterised AP

		for (int i = 0; i < layers.length; i++) {editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i] }); // Here, "editStack" for only one layer modification looks like this: [Load_Points,103,PW]
		}
		switch (appCallFlag[0]) {
		
		case "PWPr":
			System.out.println(appCallFlag[0] + " button was pressed! (doGET)");
			start_time = System.currentTimeMillis();
			runPrPowerWorld(editStack); 						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;					    
		} 
	}

	
	
	
	
	/**This method is wrote to collect the input state variable for the RT-lab sim model*/
	
	
	

	
	

	}	
		





