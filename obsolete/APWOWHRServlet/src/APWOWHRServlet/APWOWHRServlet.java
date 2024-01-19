
/*
 * what is the whole thing about? What is its structure? What is the point?
 * 
 */

package APWOWHRServlet;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.cmclinnovations.modsapi.MoDSAPI;
import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;

public class APWOWHRServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;
	public static long start_time1;
	public static long end_time1;
	public static long start_time2;
	public static long end_time2;
	public static ArrayList<String[]> editStack;	 //global variable for receiving and storing the httpRequest information
	
	public static Map<Integer, String> OBJECTIDtoHXNum = new HashMap<>();                      // ZL-160114 Maps ArcGIS OBJECTID to the heat exchanger in chemical plant
	public static Map<Integer, String> OBJECTIDtoHXB1 = new HashMap<>();                      // Maps ArcGIS OBJECTID to the heat exchanger in Biodiesel plant 1
	public static Map<Integer, String> OBJECTIDtoRadF = new HashMap<>(); 	                   //Maps ArcGIS OBJECTID to the RadFrac
	public static Map<Integer, String> OBJECTIDtoMXNum = new HashMap<>(); 	                   //Maps ArcGIS OBJECTID to the Mixer
	public static Map<Integer, String> OBJECTIDtogaslinenum = new HashMap<>(); 	                   //Maps ArcGIS OBJECTID to the GasLine
	
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReqTestAP.CSV");   //address to write the .csv file where to checke the httpRequest message
	public static String PrAPWOWHROUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPWOWHRoutCSV.CSV");
	public static String APWOWHRINCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPWOWHRinCSV.CSV");
	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/sample_static_running.py"); // ensure that python environment variable is set to python34
		
	public static String BD_WOWHR_Sim = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/BD_WOWHR_Sim");
	
	public APWOWHRServlet() {
		super();	
		OBJECTIDtoHXB1.put(3, "10E01B1");   //Biodiesel1
		
		OBJECTIDtoHXNum.put(1, "Boiler1"); //Biodiesel1
		OBJECTIDtoHXNum.put(2, "10E02B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(3, "10E01B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(4, "HRSG1B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(5, "10E03B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(6, "10E04B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(7, "10E05B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(8, "10E02B2");   //Biodiesel2
		OBJECTIDtoHXNum.put(9, "10E03B2");   //Biodiesel2
		OBJECTIDtoHXNum.put(10, "10E04B2");  //Biodiesel2
		OBJECTIDtoHXNum.put(11, "10E05B2");  //Biodiesel2
		OBJECTIDtoHXNum.put(12, "C001");   //Zeon
		OBJECTIDtoHXNum.put(13, "C002");   //Zeon
		OBJECTIDtoHXNum.put(14, "C002");   //Evonik
		OBJECTIDtoHXNum.put(15, "C002");   //Evonik
		OBJECTIDtoHXNum.put(16, "C002");   //Evonik
		OBJECTIDtoHXNum.put(17, "C002");   //Evonik
		OBJECTIDtoHXNum.put(18, "C002");   //Evonik
		OBJECTIDtoHXNum.put(19, "B3");     //hydrocracking
		OBJECTIDtoHXNum.put(20, "Cool1");  //lanxess
		OBJECTIDtoHXNum.put(21, "Eva");    //zeon
		OBJECTIDtoHXNum.put(22, "Cond");   //zeon
		OBJECTIDtoHXNum.put(23, "heat1");  //lanxess
		OBJECTIDtoHXNum.put(24, "Heat2");  //lanxess
		OBJECTIDtoHXNum.put(25, "COol2");  //lanxess
		OBJECTIDtoHXNum.put(26, "SOLVCOND");//lanxess
		OBJECTIDtoHXNum.put(27, "B2");     //zeon
		OBJECTIDtoHXNum.put(28, "B3");     //lanxess
		OBJECTIDtoHXNum.put(29, "B1");     //lanxess
		OBJECTIDtoHXNum.put(30, "B5");     //lanxess
		OBJECTIDtoHXNum.put(31, "B3");     //hydrocracking
		OBJECTIDtoHXNum.put(32, "B6");     //hydrocracking
		OBJECTIDtoHXNum.put(33, "Eva");    //cogen
		OBJECTIDtoHXNum.put(34, "Cond");   //cogen
		OBJECTIDtoHXNum.put(35, "HRSG2B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(36, "Boiler2B1"); //Biodiesel1
		OBJECTIDtoHXNum.put(37, "Boiler3B1"); //Biodiesel1
		OBJECTIDtoHXNum.put(38, "Boiler4B1"); //Biodiesel1
		OBJECTIDtoHXNum.put(39, "HRSG3B1");   //Biodiesel1
		OBJECTIDtoHXNum.put(40, "HRSG1B2");   //Biodiesel2
		OBJECTIDtoHXNum.put(41, "10E01B2");   //Biodiesel2
		OBJECTIDtoHXNum.put(42, "Boiler4B2"); //Biodiesel2
		OBJECTIDtoHXNum.put(43, "Boiler3B2"); //Biodiesel2
		OBJECTIDtoHXNum.put(44, "Boiler1B2"); //Biodiesel2
		OBJECTIDtoHXNum.put(45, "Boiler2B2"); //Biodiesel2
		OBJECTIDtoHXNum.put(46, "HRSG2B2");   //Biodiesel2
		OBJECTIDtoHXNum.put(47, "HRSG3B2");   //Biodiesel2
		OBJECTIDtoHXNum.put(48, "PREHEATB2"); //Biodiesel2
		OBJECTIDtoHXNum.put(49, "WHR-EXB2");  //Biodiesel2
		OBJECTIDtoHXNum.put(50, "10E04B3");   //Biodiesel3
		OBJECTIDtoHXNum.put(51, "10E03B3");   //Biodiesel3
		OBJECTIDtoHXNum.put(52, "10E05B3");   //Biodiesel3
		OBJECTIDtoHXNum.put(53, "10E02B3");   //Biodiesel3
		OBJECTIDtoHXNum.put(54, "HRSG1B3");   //Biodiesel3
		OBJECTIDtoHXNum.put(55, "10E01B3");   //Biodiesel3
		OBJECTIDtoHXNum.put(56, "Boiler1B3"); //Biodiesel3
		
		OBJECTIDtoRadF.put(1, "10D06B1"); //Biodiesel1
		OBJECTIDtoRadF.put(2, "10D08B1"); //Biodiesel1
		OBJECTIDtoRadF.put(3, "C1");
		OBJECTIDtoRadF.put(4, "B7");  
		OBJECTIDtoRadF.put(5, "10D08B2"); //Biodiesel2
		OBJECTIDtoRadF.put(6, "10D06B2"); //Biodiesel2
		
		OBJECTIDtoMXNum.put(1, "mx01B1");   //Biodiesel 1
		OBJECTIDtoMXNum.put(2, "mx02B1");   //Biodiesel 1
		OBJECTIDtoMXNum.put(3, "mx03B1");   //Biodiesel 1
		OBJECTIDtoMXNum.put(4, "S6");     //air liquide
		OBJECTIDtoMXNum.put(5, "S7");     //air liquide
		OBJECTIDtoMXNum.put(6, "S3");     //air liquide
		OBJECTIDtoMXNum.put(7, "S5");     //air liquide
		OBJECTIDtoMXNum.put(8, "B5");     //hydrocracking
		OBJECTIDtoMXNum.put(9, "mx01B2");   //Biodiesel 2
		OBJECTIDtoMXNum.put(10, "mx03B2");  //Biodiesel 2
		OBJECTIDtoMXNum.put(11, "mx02B2");  //Biodiesel 2
		OBJECTIDtoMXNum.put(12, "mx-whr");//Biodiesel 2
		OBJECTIDtoMXNum.put(13, "mx01ZN");  //Zeon
		OBJECTIDtoMXNum.put(14, "B8");    //Hydrocracking
		OBJECTIDtoMXNum.put(15, "MIX");   //lanxess
		OBJECTIDtoMXNum.put(16, "SOLVMIX");  //lanxess
		OBJECTIDtoMXNum.put(17, "MXEX");    //Biodiesel1
		OBJECTIDtoMXNum.put(18, "MXEX-WHR");  //Biodiesel2
		OBJECTIDtoMXNum.put(19, "mx01B3");    //Biodiesel3
		OBJECTIDtoMXNum.put(20, "mx02B3");    //Biodiesel3
		OBJECTIDtoMXNum.put(21, "mx03B3");    //Biodiesel3
		
		OBJECTIDtogaslinenum.put(7, "FUELSUPPLY"); //Biodiesel2
		OBJECTIDtogaslinenum.put(8, "FUEL1B1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(9, "FUEL2B1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(10, "FUEL3B1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(11, "FUEL1B2"); //Biodiesel2
		OBJECTIDtogaslinenum.put(12, "LPGAS1B2"); //Biodiesel2
		OBJECTIDtogaslinenum.put(13, "COMBGAS1B1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(14, "FUEL2B2"); //Biodiesel2		
		OBJECTIDtogaslinenum.put(15, "FUEL3B2"); //Biodiesel2
		
		OBJECTIDtogaslinenum.put(20, "FUELSUPPLYWHR"); //Biodiesel2
		
		OBJECTIDtogaslinenum.put(24, "FLUEGASB1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(28, "FLUEGASB2"); //Biodiesel2
		OBJECTIDtogaslinenum.put(32, "FLUEGASB3"); //Biodiesel3
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {    //doPost method to handle the httpRequest and httpResponse
		ArrayList<String[]> editStack = new ArrayList<String[]>(); // variable for receiving/storing the httpRequest information, and passing the message to relevant methods
		String[] layers = request.getParameter("layers").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(","); //OBJECTID indicate which particular entity being modified, not fully used at the moment, might be very useful in the future(can be used to improve the efficiency of JPS) 
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // appCallFlage indicate which function of the JPS being called from the applet side
		String[] QueryT = request.getParameter("QueryT").split(",");  //new parameter for the query function

		for (int i = 0; i < layers.length; i++) {
			editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i], QueryT[i]});
		}

		FileWriter flag1 = null;                                                      //filewriter to check whether the httpRequest have been correctly received
		flag1 = new FileWriter(httpReqCSV);
		flag1.append("layers=" + layers[0]);
		flag1.append(", OBJECTIDs=" + OBJECTIDs[0]);
		flag1.append(", appCallFlag=" + appCallFlag[0]);
		flag1.append(", QueryT=" + QueryT[0]);
		flag1.flush();
		flag1.close();  		
		
		switch (appCallFlag[0]) {
		case "PrAP":                                                                     // if PrAP button was pressed, then the following action will be taken
			System.out.println(appCallFlag[0] + " button was pressed! (APWOWHR)");
//			start_time = System.currentTimeMillis();
			runPrAspenPlusWOWHR(editStack);
//			System.out.println("runPrAspenPlusWOWHR takes: "+( System.currentTimeMillis()-start_time));
			System.out.println("Thread testing"+Thread.currentThread().isAlive()); 
			if(Thread.currentThread().isAlive()){
//				stopTread();
				Thread.currentThread().interrupt();
				return;
			}
			System.out.println("Thread testing"+Thread.currentThread().isAlive());
			break;
		}
	}
	
	/*public class UsingInterruptToShutdownThread extends Thread {
		  public void run() {
		    while (true) {
		      System.out.print(".");
		      System.out.flush();
		      try {
		        Thread.sleep(1000);
		      } catch (InterruptedException ex) {
		        Thread.currentThread().interrupt(); // very important
		        break;
		      }
		    }
		    System.out.println("Shutting down thread");
		  }
		  public void stopTread(String[] args)  throws InterruptedException {
		    Thread t = new UsingInterruptToShutdownThread();
		    t.start();
		    Thread.sleep(5000);
		    t.interrupt();
		  }
		}*/
	
	public void runPrAspenPlusWOWHR(ArrayList<String[]> editStack) {
//		String appCallFlag = null;
//		appCallFlag = editStack.get(0)[2];                                               // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)
		List<Double> xRow = new ArrayList<>();                                            // extra arraylist to collect the x-value required as input to the pr aspen plus model
		List<List<Double>> xData = new ArrayList<>(1);                                    // arraylist to
		List<List<Double>> yData;                                                         // output of the pr aspenplus model
		start_time1 = System.currentTimeMillis();
		xRow=getAPWOWHRInput(editStack);
		end_time1 = System.currentTimeMillis();
		xData.add(xRow);  
		
		String simDir = BD_WOWHR_Sim;
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {
	
			fileWriter = new FileWriter(PrAPWOWHROUTCSV);                                                 // filewriter for the output of pr aspenplus model
			System.load("C:/apache-tomcat-8.0.24/webapps/APWOWHRServlet/WEB-INF/DLL/MoDS_Java_API.dll");  //the MoDS API at use is version 0.1
			
			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);		
			System.out.println("xNames= " + xNames);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);
			System.out.println("yNames= " + yNames);
			for (int j = 0; j < yNames.size(); j++) {
				fileWriter.append(yNames.get(j));                                               // write the yNames to the output CSV file
				fileWriter.append(",");
			}									
		} catch (Error e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData);                       // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam"  and  the input xData that was collected before
		System.out.println("xRow=" + xRow);
		System.out.println("yData=" + yData);                                              // print out the output yData to console

		for (int j = 0; j < yData.size(); j++) {
			try {
				fileWriter.append("\n");
				for (int k = 0; k < yData.get(j).size(); k++) {
					fileWriter.append(Double.toString(yData.get(j).get(k)));                        // write the yData to the output CSV file
					fileWriter.append(",");
				}
			} catch (IOException e) {

				e.printStackTrace();
			} finally {
				try {
					fileWriter.flush();
					fileWriter.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
// end of evaluating the surrogate model
		start_time2 = System.currentTimeMillis();
		readPrAPCSV();
		end_time2 = System.currentTimeMillis();
		System.out.println("getAPWOWHRInput takes: "+( end_time1-start_time1));
		System.out.println("readPrAPCSV takes: "+( end_time2-start_time2));
	}
	
	public ArrayList<Double> getAPWOWHRInput(ArrayList<String[]> editStack){ 
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		
		System.out.println("keyset="+ OBJECTIDtoHXB1.keySet());				
		for (Integer key : OBJECTIDtoHXB1.keySet()) {
			try {
				QueryParameters qParameter_HX = new QueryParameters();                       // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
				qParameter_HX.setWhere("OBJECTID='" + key + "'");                            // define FID address of an ArcGIS element
				qParameter_HX.setOutFields(new String[] { "*" });                            // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_HX = null;                                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_HX = null;                                                   // create an instance of Feature to store an ArcGIS element

				qTask_HX = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_HX = qTask_HX.execute(qParameter_HX);                   // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
				graphic_HX = (Feature) fResult_HX.iterator().next();                          // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_HX.add(graphic_HX.getAttributes());                            // append information about the  element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace();                                                            // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}
		
		ArrayList<Double> xRow = new ArrayList<Double>();                                      // extra arraylist to collect the x-value required as input to the pr aspen plus model
	 		 
		 FileWriter filewriterAPIN = null;

		try {			
			filewriterAPIN = new FileWriter(APWOWHRINCSV); // to put the input values for the AspenPlus subset model
			filewriterAPIN.append("FOIL, TOILin, TOILout");
			filewriterAPIN.append("\n");

			/*for (Integer key : OBJECTIDtoHXB1.keySet()) {
				int OBJECTID = key; //3
				System.out.println("key = "+OBJECTID);
				
				if(OBJECTIDtoHXB1.get(OBJECTID).equals("10E01B1")) {
					filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
					filewriterAPIN.append(",");
					filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")));
					filewriterAPIN.append(",");
					filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")));
					filewriterAPIN.append(",");
					xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
					xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")))); // add the temperature of oil to xRow
					xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")))); // add the temperature of oil to xRow
				}
			}*/
					
			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) {                                       // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {						
						if (OBJECTIDtoHXB1.get(i + 3).equals("10E01B1")) {
//						if (OBJECTIDtoHXB1.get(i + 1).equals("10E01B1")) {                                         // "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")))); // add the temperature of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")))); // add the temperature of oil to xRow
						break;
						}
					}
				}
			}

			System.out.println("xRow=" + xRow);                                                                    // print out all the x-data that has been collected to console
			
			filewriterAPIN.flush();
			filewriterAPIN.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return xRow;
	} 

	public void readPrAPCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrAPWOWHROUTCSV));
			fileReader.readLine();     // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable RadFracTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/RadFrac/FeatureServer", user, 0);
			System.out.println("1");
			RadFracTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			System.out.println("2");
			RadFracTable.initialize();
			System.out.println(RadFracTable.getStatus());
			RadFracTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable HeaterCoolerTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer", user, 0);
			HeaterCoolerTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			HeaterCoolerTable.initialize();
			System.out.println(HeaterCoolerTable.getStatus());
			HeaterCoolerTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable MixerTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer", user, 0);
			MixerTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MixerTable.initialize();
			System.out.println(MixerTable.getStatus());
			MixerTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable GasLineTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Gas_line/FeatureServer", user, 0);
			GasLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			GasLineTable.initialize();
			System.out.println(GasLineTable.getStatus());
			GasLineTable.getInitializationError();

			final CountDownLatch latch = new CountDownLatch(4);                                                                             // ZL-151207 handles one asynchronous processes, only continues  Thread when it reaches 0
			RadFracTable.populateFromService(loadAllFeatures, false, 
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {                                                                            // Asynchronous callback: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown();                                                                                          // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			
			HeaterCoolerTable.populateFromService(loadAllFeatures, false, 
					new CallbackListener<Boolean>() {
				        @Override
				        public void onCallback(Boolean status) {                                                                            // Asynchronous callback: code must wait for populate from service to finish loading features
					        if (status == true) {
						        latch.countDown();                                                                                          // latch decrement if feature service table is ready
					        }
				        }

				        @Override
				        public void onError(Throwable e) {
					        e.printStackTrace();
				        }
			       });
			
			MixerTable.populateFromService(loadAllFeatures, false, 
					new CallbackListener<Boolean>() {
				        @Override
				        public void onCallback(Boolean status) {                                                                            // Asynchronous callback: code must wait for populate from service to finish loading features
					        if (status == true) {
						        latch.countDown();                                                                                          // latch decrement if feature service table is ready
					        }
				        }

				        @Override
				        public void onError(Throwable e) {
					        e.printStackTrace();
				        }
			       });
			
			GasLineTable.populateFromService(loadAllFeatures, false, 
					new CallbackListener<Boolean>() {
				        @Override
				        public void onCallback(Boolean status) {                                                                            // Asynchronous callback: code must wait for populate from service to finish loading features
					        if (status == true) {
						        latch.countDown();                                                                                          // latch decrement if feature service table is ready
					        }
				        }

				        @Override
				        public void onError(Throwable e) {
					        e.printStackTrace();
				        }
			       });
			
			       latch.await();                                                                                                              // wait until all feature service tables are ready then continue
			             
			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				System.out.println("data= " + data);
//				String[] ArcGISOBJECTID = null;
//				ArcGISOBJECTID = new String[100];

				//the following code is used for updating the flowrate of the FINALPRD to ArcGIS database
				for (int j = 1; j < 2; j++) {
					String[] ArcGISOBJECTID = null;
					ArcGISOBJECTID = new String[3];
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoRadF.get(j + 1).equals("10D08B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> RadFracAttributes = RadFracTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[0].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3Qnt",Float.parseFloat(data[0].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("F="+data[0]);
						
						RadFracTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),RadFracAttributes);                          // update feature table locally
						break;
					}
				}
				//the following code is used for updating the heat duty of the heater-coolers to ArcGIS database
				for (int j = 1; j < 6; j++) {
					String[] ArcGISOBJECTID = null;
					ArcGISOBJECTID = new String[6];
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoHXNum.get(j + 1).equals("10E01B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> HeaterCoolerAttributes = HeaterCoolerTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[4].trim().isEmpty()) {
							HeaterCoolerAttributes.put("Heat_Loads",Float.parseFloat(data[4].trim())*4.1868e-3);                               // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("10E01Duty="+data[4]);
						
						HeaterCoolerTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),HeaterCoolerAttributes);                          // update feature table locally
						
					}
					if (OBJECTIDtoHXNum.get(j + 1).equals("10E02B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> HeaterCoolerAttributes = HeaterCoolerTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[5].trim().isEmpty()) {
							HeaterCoolerAttributes.put("Heat_Loads",Float.parseFloat(data[5].trim())*4.1868e-3);                               // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("10E02Duty="+data[5]);
						
						HeaterCoolerTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),HeaterCoolerAttributes);                          // update feature table locally
						
					}
					if (OBJECTIDtoHXNum.get(j + 1).equals("10E03B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> HeaterCoolerAttributes = HeaterCoolerTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[6].trim().isEmpty()) {
							HeaterCoolerAttributes.put("Heat_Loads",Float.parseFloat(data[6].trim())*4.1868e-3);                               // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("10E03Duty="+data[6]);
						
						HeaterCoolerTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),HeaterCoolerAttributes);                          // update feature table locally
						
					}
					if (OBJECTIDtoHXNum.get(j + 1).equals("10E04B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> HeaterCoolerAttributes = HeaterCoolerTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[7].trim().isEmpty()) {
							HeaterCoolerAttributes.put("Heat_Loads",Float.parseFloat(data[7].trim())*4.1868e-3);                               // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("10E04Duty="+data[7]);
						
						HeaterCoolerTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),HeaterCoolerAttributes);                          // update feature table locally
						
					}
				}
				//the following code is used for updating the CO2 emission amount of the heater-coolers to ArcGIS database
				for (int j = 23; j < 24; j++) {
					String[] ArcGISOBJECTID = null;
					ArcGISOBJECTID = new String[25];
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);
					
					if (OBJECTIDtogaslinenum.get(j + 1).equals("FLUEGASB1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> GasLineAttributes = GasLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[18].trim().isEmpty()) {
							GasLineAttributes.put("Mat_1_qnt",Float.parseFloat(data[18].trim()));                                // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("CO2="+data[18]);
						
						GasLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),GasLineAttributes);                          // update feature table locally
						break;
					}
				}
				//the following code is used for updating the flowrate and cost of the fuel gas to ArcGIS database
				for (int j = 6; j < 10; j++) {
					String[] ArcGISOBJECTID = null;
					ArcGISOBJECTID = new String[11];
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtogaslinenum.get(j + 1).equals("FUEL1B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> GasLineAttributes = GasLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[11].trim().isEmpty()) {
							GasLineAttributes.put("Mat_1_qnt",Float.parseFloat(data[11].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("FFuel1="+data[11]);
						
						if (!data[8].trim().isEmpty()) {
							GasLineAttributes.put("Cost",Float.parseFloat(data[8].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("Fuel1 Cost="+data[8]);
						
						GasLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),GasLineAttributes);                          // update feature table locally
						
					}
					
					if (OBJECTIDtogaslinenum.get(j + 1).equals("FUEL2B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> GasLineAttributes = GasLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[12].trim().isEmpty()) {
							GasLineAttributes.put("Mat_1_qnt",Float.parseFloat(data[12].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("FFuel2="+data[12]);
						
						if (!data[9].trim().isEmpty()) {
							GasLineAttributes.put("Cost",Float.parseFloat(data[9].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("Fuel2 Cost="+data[9]);
						
						GasLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),GasLineAttributes);                          // update feature table locally
						
					}
					
					if (OBJECTIDtogaslinenum.get(j + 1).equals("FUEL3B1")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> GasLineAttributes = GasLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[13].trim().isEmpty()) {
							GasLineAttributes.put("Mat_1_qnt",Float.parseFloat(data[13].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("FFuel3="+data[13]);
						
						if (!data[10].trim().isEmpty()) {
							GasLineAttributes.put("Cost",Float.parseFloat(data[10].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("Fuel3 Cost="+data[10]);
						
						GasLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),GasLineAttributes);                          // update feature table locally
						
					}
					
					if (OBJECTIDtogaslinenum.get(j + 1).equals("FUELSUPPLY")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> GasLineAttributes = GasLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
												
						if (!data[10].trim().isEmpty()) {
							Float FuelCost1=Float.parseFloat(data[8].trim());
							Float FuelCost2=Float.parseFloat(data[9].trim());
							Float FuelCost3=Float.parseFloat(data[10].trim());
							GasLineAttributes.put("Cost",(FuelCost1+FuelCost2+FuelCost3));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
							
							System.out.println("TotalFuel Cost="+(FuelCost1+FuelCost2+FuelCost3));
						}
												
						GasLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),GasLineAttributes);                          // update feature table locally
						
					}
					
				}
			}
			RadFracTable.applyEdits(null);                                                                                        // commit local updates onto Server
			MixerTable.applyEdits(null); 
			HeaterCoolerTable.applyEdits(null);
			GasLineTable.applyEdits(null);
			
			RadFracTable.dispose();
			MixerTable.dispose(); 
			HeaterCoolerTable.dispose();
			GasLineTable.dispose();
			
			
			
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms");                     // tells how long it took to update
		} catch (Exception e) {
			e.printStackTrace();
		}finally {
			try {
				fileReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
				
	}

}



