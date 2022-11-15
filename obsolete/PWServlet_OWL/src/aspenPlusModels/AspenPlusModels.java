package aspenPlusModels;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;

public class AspenPlusModels {
	
	public static Map<Integer, String> OBJECTIDtoHXNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the heat exchanger in chemical plant
	public static Map<Integer, String> OBJECTIDtoCRNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the reactor in chemical plant
	public static Map<Integer, String> OBJECTIDtoRadF = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the RadFrac
	public static Map<Integer, String> OBJECTIDtoMXNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the mixer in chemical plant 
	public static Map<Integer, String> OBJECTIDtogaslinenum = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the Reactor
	
	public static String APINCSV = new String( "C:/apache-tomcat-8.0.24/webapps/ROOT/APIN.CSV"); // ZL-151124 input CSV for aspen plus
	public static String APOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APOUT.CSV"); // ZL-151124 output CSV from Aspen plus
	public static String APWHRINCSV = new String( "C:/apache-tomcat-8.0.24/webapps/ROOT/APWHRIN.CSV"); // ZL-151124 input CSV for aspen plus with heat recovery
	public static String APWHROUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APWHROUT.CSV"); // ZL-151124 output CSV from Aspen plus with heat recovery
	
	public static String runPythonCommandAP = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APrun_modified.pyw"); // ZL-151124  python script calling Aspen Plus model
	public static String runPythonCommandAPWHR = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APrunWHR.pyw"); // ZL-151124  python script calling Aspen Plus With Heat Recovery model
	
	public AspenPlusModels(){
		super();
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
		
		OBJECTIDtoCRNum.put(1, "10D01B1"); //Biodiesel1
		OBJECTIDtoCRNum.put(2, "10D03B1"); //Biodiesel1
		OBJECTIDtoCRNum.put(3, "Combust1B1"); //Biodiesel1
		OBJECTIDtoCRNum.put(4, "R101"); //EVONIK
		OBJECTIDtoCRNum.put(5, "R102"); //EVONIK
		OBJECTIDtoCRNum.put(6, "R201"); //EVONIK
		OBJECTIDtoCRNum.put(7, "B20"); //HYDROCRACKING
		OBJECTIDtoCRNum.put(8, "10D01B2"); //Biodiesel2
		OBJECTIDtoCRNum.put(9, "10D03B2"); //Biodiesel2
		OBJECTIDtoCRNum.put(10, "POLYBRT1"); //ZEON
		OBJECTIDtoCRNum.put(11, "B1"); //HYDROCRACKING
		OBJECTIDtoCRNum.put(12, "Reactor"); //LANXESS
		OBJECTIDtoCRNum.put(13, "COMBUST2B1"); //Biodiesel1
		OBJECTIDtoCRNum.put(14, "COMBUST3B1"); //Biodiesel1
		OBJECTIDtoCRNum.put(15, "COMBUST1B2"); //Biodiesel2
		OBJECTIDtoCRNum.put(16, "COMBUST2B2"); //Biodiesel2
		OBJECTIDtoCRNum.put(17, "COMBUST3B2"); //Biodiesel2
		OBJECTIDtoCRNum.put(18, "10D03B3"); //Biodiesel3
		OBJECTIDtoCRNum.put(19, "10D01B3"); //Biodiesel3
		OBJECTIDtoCRNum.put(20, "COMBUST1B3"); //Biodiesel3
		
		OBJECTIDtoRadF.put(1, "10D06B1"); //Biodiesel1
		OBJECTIDtoRadF.put(2, "10D08B1"); //Biodiesel1
		OBJECTIDtoRadF.put(3, "C1");
		OBJECTIDtoRadF.put(4, "B7");  
		OBJECTIDtoRadF.put(5, "10D08B2"); //Biodiesel2
		OBJECTIDtoRadF.put(6, "10D06B2"); //Biodiesel2		
		OBJECTIDtoRadF.put(11, "10D08B3"); //Biodiesel3
		OBJECTIDtoRadF.put(12, "10D06B3"); //Biodiesel3
		
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
		
		OBJECTIDtogaslinenum.put(7, "FUEL SUPPLYB1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(20, "FUEL SUPPLYB2"); //Biodiesel2
	}
	
		
	public static void runAspenPlus(ArrayList<String[]> editStack) {
		getAPInput(editStack);
		runPyScript(editStack);                                                                  // call python script to run aspen plus model
		readAPCSV();
	}
	
	public static void runAspenPlusWithWasteHeatRecovery(ArrayList<String[]> editStack) {
		getAPWHRInput(editStack);
		runPyScript(editStack);                                                                  // PC call python script to run aspen plus model with waste heat recovery
		readAPWHRCSV();		
	}
	
	public static ArrayList<Double> getAPInput(ArrayList<String[]> editStack){  
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		ArrayList<Map<String, Object>> attributeslist_Reactor = new ArrayList<Map<String, Object>>(); // additional ArrayList for Reactor
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		
		System.out.println("keyset="+ OBJECTIDtoHXNum.keySet());
		for (Integer key : OBJECTIDtoHXNum.keySet()) {
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
		
		for (int key : OBJECTIDtoCRNum.keySet()) {
		System.out.println(key);
		try {
			QueryParameters qParameter_Reactor = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
			qParameter_Reactor.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
			qParameter_Reactor.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
			QueryTask qTask_Reactor = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
			Feature graphic_Reactor = null; // create an instance of Feature to store an ArcGIS element

			qTask_Reactor = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Reactor/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

			FeatureResult fResult_Reactor = qTask_Reactor.execute(qParameter_Reactor); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
			graphic_Reactor = (Feature) fResult_Reactor.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
			attributeslist_Reactor.add(graphic_Reactor.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

		} catch (Exception e) {
			e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
		}
	  }
		
		
		
		
		ArrayList<Double> xRow = new ArrayList<Double>();                                      // extra arraylist to collect the x-value required as input to the pr aspen plus model
		 		 
		 FileWriter filewriterAPIN = null;

		try {
			
			filewriterAPIN = new FileWriter(APINCSV); // to put the input values for the AspenPlus subset model

			filewriterAPIN.append("FOIL, TOIL, DSTEMP"); //, FMEOH, TMEOH, FREWATER, PBOILER");
			filewriterAPIN.append("\n");

			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoHXNum.get(i + 1).equals("10E01B1")) { // "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")))); // add the temperature of oil to xRow
						}
					}
				}
			}
			
			for (int i = 0; i < attributeslist_Reactor.size(); i++) {
				for (String key : attributeslist_Reactor.get(i).keySet()) { // go through all  the Reactors in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoCRNum.get(i + 1).equals("10D01B1")) { // "10D01" is the Reactor 
							filewriterAPIN.append(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1_T")))); // add the temperature of the feeding methanol flow to xRow							
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
	/**get input for aspen plus model with waste heat recovery, maybe there's a way to merge this method with the getAPInput method*/
	public static ArrayList<Double> getAPWHRInput(ArrayList<String[]> editStack){ 
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		ArrayList<Map<String, Object>> attributeslist_Reactor = new ArrayList<Map<String, Object>>(); // additional ArrayList for Reactor
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		
		System.out.println("keyset="+ OBJECTIDtoHXNum.keySet());
		for (Integer key : OBJECTIDtoHXNum.keySet()) {
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
		
		for (int key : OBJECTIDtoCRNum.keySet()) {
		System.out.println(key);
		try {
			QueryParameters qParameter_Reactor = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
			qParameter_Reactor.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
			qParameter_Reactor.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
			QueryTask qTask_Reactor = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
			Feature graphic_Reactor = null; // create an instance of Feature to store an ArcGIS element

			qTask_Reactor = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Reactor/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

			FeatureResult fResult_Reactor = qTask_Reactor.execute(qParameter_Reactor); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
			graphic_Reactor = (Feature) fResult_Reactor.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
			attributeslist_Reactor.add(graphic_Reactor.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

		} catch (Exception e) {
			e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
		}
	  } 
		
		ArrayList<Double> xRow = new ArrayList<Double>();                                      // extra arraylist to collect the x-value required as input to the pr aspen plus model
				 		 
		 FileWriter filewriterAPIN = null;

		try {
			
			filewriterAPIN = new FileWriter(APWHRINCSV); // to put the input values for the AspenPlusWHR model

			filewriterAPIN.append("FOIL, TOIL, DSTEMP"); //, FMEOH, TMEOH, FREWATER, PBOILER");
			filewriterAPIN.append("\n");
			System.out.println("Run for loop till" + attributeslist_HX.size());

			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoHXNum.get(i + 1).equals("PREHEATB2")) { // "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")))); // add the temperature of oil to xRow
						}
					}
				}
			}
			
			for (int i = 0; i < attributeslist_Reactor.size(); i++) {
				for (String key : attributeslist_Reactor.get(i).keySet()) { // go through all  the Reactors in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoCRNum.get(i + 1).equals("10D01B2")) { // "10D01" is the Reactor 
							filewriterAPIN.append(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1_T")))); // add the temperature of the feeding methanol flow to xRow							
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
	
	public static void runPyScript(ArrayList<String[]> editStack) {
		String appCallFlag = null;
		appCallFlag = editStack.get(0)[2];           // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)

		try {
			System.out.println(appCallFlag);
			switch (appCallFlag) {

			case ("AP"):               // when appCallFlag=AP indicating that the run Aspenplus button has been pressed, then the following actions are going to be taken
				System.out.println(appCallFlag + " Button was pressed! (runPyScript)");   // for double checking
				Process p = Runtime.getRuntime().exec(runPythonCommandAP);         // call python script to run aspenplus
				p.waitFor();
				System.out.println("Exit Value (0 means success): " + p.exitValue()); // if console prints 0 it means success
				BufferedReader br = new BufferedReader(new InputStreamReader( p.getInputStream()));
				String line;                                                     // retrieves console from python script
				System.out.println("Python input:");
				while ((line = br.readLine()) != null) {
					System.out.println(line);                                       // print input array from Python (see python code for more details)
				}
				line = br.readLine();
				break;
				
			case ("APHR"):             // PC when appCallFlag=APHR indicating that the run Aspenplus with heat recovery button has been pressed, then the following actions are going to be taken
				System.out.println(appCallFlag + " Button was pressed! (runPyScript)");     // for double checking
				Process p3 = Runtime.getRuntime().exec(runPythonCommandAPWHR);    // call python script to run aspenplus
				p3.waitFor();
				System.out.println("Exit Value (0 means success): " + p3.exitValue()); // if console prints 0 it means success
				BufferedReader br3 = new BufferedReader(new InputStreamReader( p3.getInputStream()));
				String line3;                                 // retrieves console from python script
				System.out.println("Python input:");
				while ((line3 = br3.readLine()) != null) {
					System.out.println(line3);        // print input array from Python (see python code for more details)
				}
				line = br3.readLine();
				break;	
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void readAPCSV() {       // PC (Update outputs obtained by running Aspen Plus model to ArcGIS database)
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(APOUTCSV));
			fileReader.readLine(); // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			GeodatabaseFeatureServiceTable RadFracTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/RadFrac/FeatureServer",user, 0);
			RadFracTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			RadFracTable.initialize();
			System.out.println(RadFracTable.getStatus());
			RadFracTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable MXTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer",user, 0);
			MXTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MXTable.initialize();
			System.out.println(MXTable.getStatus());
			MXTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable ReactorTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Reactor/FeatureServer",user, 0);
			ReactorTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			ReactorTable.initialize();
			System.out.println(ReactorTable.getStatus());
			ReactorTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable HXTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer",user, 0);
			HXTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			HXTable.initialize();
			System.out.println(HXTable.getStatus());
			HXTable.getInitializationError();
			
			GeodatabaseFeatureServiceTable GLTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Gas_line/FeatureServer",user, 0);
			GLTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			GLTable.initialize();
			System.out.println(GLTable.getStatus());
			GLTable.getInitializationError();

			final CountDownLatch latch = new CountDownLatch(5); // ZL-151207 handles one asynchronous processes, only continuesThread when it reaches 0
			RadFracTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 1: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			MXTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 2: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			ReactorTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 3: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			HXTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 4: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			GLTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 5: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			latch.await(); // wait until all feature service tables are ready then continue

			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				System.out.println("data= " + data);
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[7];

				for (int j = 0; j < 2; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoRadF.get(j + 1).equals("10D08B1")) {            // RadF 10D08B1 represents the distillation column that separates the final product from remaining reactants and byproducts
						Map<String, Object> RadFracAttributes = RadFracTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[1].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3Qnt",Float.parseFloat(data[1].trim()));   // update the Molar Flowrate of ester3 (Biodiesel) (or) Final Product from APOUT csv to ArcGIS
						}
						System.out.println("F="+data[0]);
						if (!data[2].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3_T",Float.parseFloat(data[2].trim()));   // update the Temperature of Ester3 (Biodiesel) from APOUT csv to ArcGIS
						}
						RadFracTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),RadFracAttributes);  // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[20];
				for (int k = 0; k < 19; k++) {
					ArcGISOBJECTID[k] = String.valueOf(k+1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoMXNum.get(k+1).equals("MXEXB1")) {    // MXEXB1 represents the mixer that combines the exhaust gases from all the boilers
						Map<String, Object> MXAttributes = MXTable.getFeature(Long.parseLong(ArcGISOBJECTID[k])).getAttributes();
						if (!data[3].trim().isEmpty()) {
							MXAttributes.put("MatIn1Qnt",Float.parseFloat(data[3].trim()));   //Update the CO2 flow rate from APOUT to ArcGIS
						}
						System.out.println("F="+data[3]);
						MXTable.updateFeature(Long.parseLong(ArcGISOBJECTID[k]),MXAttributes);   // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[7];
				for (int l = 0; l < 4; l++) {
					ArcGISOBJECTID[l] = String.valueOf(l+1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoCRNum.get(l+1).equals("Combust1B1")) {    // COMBUSTB1 represnets the Combustion Reactor
						Map<String, Object> ReactorAttributes = ReactorTable.getFeature(Long.parseLong(ArcGISOBJECTID[l])).getAttributes();
						if (!data[4].trim().isEmpty()) {
							ReactorAttributes.put("MatIn1Qnt",Float.parseFloat(data[4].trim()));   // Update the Amount of Fuel being supplied
						}
						ReactorTable.updateFeature(Long.parseLong(ArcGISOBJECTID[l]),ReactorAttributes);  // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[7];
				for (int m = 0; m < 4; m++) {
					ArcGISOBJECTID[m] = String.valueOf(m+1);
					System.out.println(ArcGISOBJECTID);
					
					if (OBJECTIDtoHXNum.get(m+1).equals("Boiler1B1")) {      // BoilerB1 represents the Boiler which is use to produce LP steam
						Map<String, Object> HXAttributes = HXTable.getFeature(Long.parseLong(ArcGISOBJECTID[m])).getAttributes();
						if (!data[5].trim().isEmpty()) {
							HXAttributes.put("MatIn1Qnt",Float.parseFloat(data[5].trim()));   // Update the amount of steam being supplied 
						}
						System.out.println("F="+data[5]);
						HXTable.updateFeature(Long.parseLong(ArcGISOBJECTID[m]),HXAttributes);   // update feature table locally
						break;
					}
					
				}
				
				ArcGISOBJECTID = new String[7];
				for (int m = 0; m < 4; m++) {
					ArcGISOBJECTID[m] = String.valueOf(m+1);
					System.out.println(ArcGISOBJECTID);
					if (OBJECTIDtoHXNum.get(m+1).equals("10E01B1")) {    // 10E01 represents the heat exchanger (heater)  
						Map<String, Object> HXAttributes = HXTable.getFeature(Long.parseLong(ArcGISOBJECTID[m])).getAttributes();
						if (!data[6].trim().isEmpty()) {
							HXAttributes.put("Heat_Loads",Float.parseFloat(data[6].trim()));   // Update the heat load on the heat exchanger
						}	
						HXTable.updateFeature(Long.parseLong(ArcGISOBJECTID[m]),HXAttributes);   // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[10];
				for (int m = 0; m < 10; m++) {
					ArcGISOBJECTID[m] = String.valueOf(m+1);
					System.out.println(ArcGISOBJECTID);
					if (OBJECTIDtogaslinenum.get(m+1) == null) {
						continue;
					}
					System.out.println(OBJECTIDtogaslinenum.get(m+1));
					if (OBJECTIDtogaslinenum.get(m+1).equals("FUEL SUPPLYB1")) {   // FUEL SUPPLYB1 represents Fuel supplying Gas Line
						Map<String, Object> GLAttributes = GLTable.getFeature(Long.parseLong(ArcGISOBJECTID[m])).getAttributes();
						if (!data[7].trim().isEmpty()) {
							GLAttributes.put("Cost",Float.parseFloat(data[7].trim()));   // update the cost of the fuel based upon the amount of fuel supplied
						}	
						GLTable.updateFeature(Long.parseLong(ArcGISOBJECTID[m]),GLAttributes);  // update feature table locally
						break;
					}
				}
			}
			RadFracTable.applyEdits(null); // commit local updates onto server
		    MXTable.applyEdits(null);
	        ReactorTable.applyEdits(null);
		    HXTable.applyEdits(null);
		    GLTable.applyEdits(null);
		    
//		    RadFracTable.dispose(); // commit local updates onto server
//		    MXTable.dispose();
//	        ReactorTable.dispose();
//		    HXTable.dispose();
//		    GLTable.dispose();
		    
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms"); // tells how long it took to update
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	public static void readAPWHRCSV() {   // PC (Update outputs obtained by running Aspen Plus model with waste heat recovery to ArcGIS database)
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(APWHROUTCSV));
			fileReader.readLine(); // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			GeodatabaseFeatureServiceTable RadFracTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/RadFrac/FeatureServer",user, 0);
			RadFracTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			RadFracTable.initialize();
			
			GeodatabaseFeatureServiceTable MXTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer",user, 0);
			MXTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MXTable.initialize();
			
			GeodatabaseFeatureServiceTable ReactorTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Reactor/FeatureServer",user, 0);
			ReactorTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			ReactorTable.initialize();
			
			GeodatabaseFeatureServiceTable HXTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer",user, 0);
			HXTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			HXTable.initialize();
			
			GeodatabaseFeatureServiceTable GLTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Gas_line/FeatureServer",user, 0);
			GLTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			GLTable.initialize();

			final CountDownLatch latch = new CountDownLatch(5); // ZL-151207 handles one asynchronous processes, only continuesThread when it reaches 0
			RadFracTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 1: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			MXTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 2: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			ReactorTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 3: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			HXTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 4: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			GLTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback 5: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown(); // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			latch.await(); // wait until all feature service tables are ready then continue

			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				System.out.println("data= " + data);
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[7];

				for (int j = 0; j < 7; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);
					
					System.out.println(OBJECTIDtoRadF.get(j + 1));
					if (OBJECTIDtoRadF.get(j + 1) == null){
						continue;}
					if (OBJECTIDtoRadF.get(j + 1).equals("10D08B2")) {            // RadF 10D08B1 represents the distillation column that separates the final product from remaining reactants and byproducts
						Map<String, Object> RadFracAttributes = RadFracTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[1].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3Qnt",Float.parseFloat(data[1].trim()));   // update the Molar Flowrate of ester3 (Biodiesel) (or) Final Product from APOUT csv to ArcGIS
						}
						System.out.println("F="+data[0]);
						if (!data[2].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3_T",Float.parseFloat(data[2].trim()));   // update the Temperature of Ester3 (Biodiesel) from APOUT csv to ArcGIS
						}
						RadFracTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),RadFracAttributes);  // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[20];
				for (int k = 0; k < 20; k++) {
					ArcGISOBJECTID[k] = String.valueOf(k+1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoMXNum.get(k+1).equals("MXEX-WHRB2")) {       // MXEXB1 represents the mixer that combines the exhaust gases from all the boilers
						Map<String, Object> MXAttributes = MXTable.getFeature(Long.parseLong(ArcGISOBJECTID[k])).getAttributes();
						if (!data[3].trim().isEmpty()) {
							MXAttributes.put("MatIn1Qnt",Float.parseFloat(data[3].trim()));   //Update the CO2 flow rate from APOUT to ArcGIS
						}
						System.out.println("F="+data[3]);
						MXTable.updateFeature(Long.parseLong(ArcGISOBJECTID[k]),MXAttributes);   // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[20];
				for (int l = 0; l < 20; l++) {
					ArcGISOBJECTID[l] = String.valueOf(l+1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoCRNum.get(l+1).equals("COMBUST1B2")) {      // COMBUSTB1 represnets the Combustion Reactor
						Map<String, Object> ReactorAttributes = ReactorTable.getFeature(Long.parseLong(ArcGISOBJECTID[l])).getAttributes();
						if (!data[4].trim().isEmpty()) {
							ReactorAttributes.put("MatIn1Qnt",Float.parseFloat(data[4].trim()));  // Update the Amount of Fuel being supplied
						}
						ReactorTable.updateFeature(Long.parseLong(ArcGISOBJECTID[l]),ReactorAttributes);  // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[45];
				for (int m = 0; m < 45; m++) {
					ArcGISOBJECTID[m] = String.valueOf(m+1);
					System.out.println(ArcGISOBJECTID);
					
					if (OBJECTIDtoHXNum.get(m+1).equals("Boiler1B2")) {     // BoilerB1 represents the Boiler which is use to produce LP steam
						Map<String, Object> HXAttributes = HXTable.getFeature(Long.parseLong(ArcGISOBJECTID[m])).getAttributes();
						if (!data[5].trim().isEmpty()) {
							HXAttributes.put("MatIn1Qnt",Float.parseFloat(data[5].trim()));   // Update the amount of steam being supplied
						}
						System.out.println("F="+data[5]);
						System.out.println("F="+data[5]);
						HXTable.updateFeature(Long.parseLong(ArcGISOBJECTID[m]),HXAttributes);   // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[42];
				for (int m = 0; m < 42; m++) {
					ArcGISOBJECTID[m] = String.valueOf(m+1);
					System.out.println(ArcGISOBJECTID);
					if (OBJECTIDtoHXNum.get(m+1).equals("10E01B2")) {    // 10E01B2 represents the heat exchanger (heater) 
						System.out.println(HXTable);
						Map<String, Object> HXAttributes = HXTable.getFeature(Long.parseLong(ArcGISOBJECTID[m])).getAttributes();
						if (!data[6].trim().isEmpty()) {
							HXAttributes.put("Heat_Loads",Float.parseFloat(data[6].trim()));   // Update the heat load on the heat exchanger
						}	
						HXTable.updateFeature(Long.parseLong(ArcGISOBJECTID[m]),HXAttributes);    // update feature table locally
						break;
					}
				}
				
				ArcGISOBJECTID = new String[25];
				for (int n = 0; n < 21; n++) {
					ArcGISOBJECTID[n] = String.valueOf(n + 1);
					System.out.println(ArcGISOBJECTID);
					
					System.out.println(OBJECTIDtogaslinenum.get(n + 1));
					if (OBJECTIDtogaslinenum.get(n + 1) == null){
						continue;
						}
					if (OBJECTIDtogaslinenum.get(n + 1).equals("FUEL SUPPLYB2")) {     // FUEL SUPPLYB1 represents Fuel supplying Gas Line
						System.out.println("F="+data[7]);
						System.out.println(GLTable);
						System.out.println(ArcGISOBJECTID[n]);
						Map<String, Object> GLAttributes = GLTable.getFeature(Long.parseLong(ArcGISOBJECTID[n])).getAttributes();
						if (!data[7].trim().isEmpty()) {
							GLAttributes.put("Cost",Float.parseFloat(data[7].trim()));   // update the cost of the fuel based upon the amount of fuel supplied
						}
						System.out.println("F="+data[7]);
						GLTable.updateFeature(Long.parseLong(ArcGISOBJECTID[n]),GLAttributes);    // update feature table locally
						break;
					}
				}
			}
			RadFracTable.applyEdits(null); // commit local updates onto server
		    MXTable.applyEdits(null);
	        ReactorTable.applyEdits(null);
		    HXTable.applyEdits(null);
		    GLTable.applyEdits(null);
//		    RadFracTable.dispose(); // commit local updates onto server
//		    MXTable.dispose();
//	        ReactorTable.dispose();
//		    HXTable.dispose();
//		    GLTable.dispose();
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms"); // tells how long it took to update
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		 }
	   }
}
