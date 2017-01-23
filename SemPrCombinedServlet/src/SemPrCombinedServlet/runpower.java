package SemPrCombinedServlet;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import com.cmclinnovations.modsapi.MoDSAPI;
import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.EsriSecurityException;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;


public class runpower {
	public static long start_time;
	public static long end_time;
	public static long interval;
	
	public static ArrayList<String[]> editStack;
	
	public static Map<String, String> ArcGISFIDloadtoPWBusNum = new HashMap<>();                                           // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFIDload = new HashMap<>();                                           // reverse mapping BusNum to ArcGIS FID
	public static Map<String, String> ArcGISFIDgenwindtoPWBusNum = new HashMap<>();                                          // Maps Substation number to HV and LV bus numbers
	public static Map<String, String> ArcGISFIDgenmarinetoPWBusNum = new HashMap<>();
	public static Map<String, String> ArcGISFIDgendieseltoPWBusNum = new HashMap<>();
	public static Map<String, String> ArcGISFIDgenpvtoPWBusNum = new HashMap<>();
	public static Map<String, String> PWBusNumtoArcGISFIDgenmarine = new HashMap<>();
	public static Map<String, String> PWBusNumtoArcGISFIDgendiesel = new HashMap<>();
	public static Map<String, String> PWBusNumtoArcGISFIDgenpv = new HashMap<>();
	public static Map<String, String> PWBusNumtoArcGISFIDgenwind = new HashMap<>();
	public static Map<String, String> ArcGISFIDbattoPWBusNum = new HashMap<>();                                          // Maps Substation number to HV and LV bus numbers
	public static Map<String, String> PWBusNumtoArcGISFIDbat = new HashMap<>(); 
	public static Map<String, Integer> BusNumtoXPointLP = new HashMap<>(); // Maps BusNum load point to X point for the parametrised model
	public static Map<Integer, String> XPointtoBusNumLP = new HashMap<>(); // reverse mapping
	public static Map<String, Integer> BusNumtoXPointWT = new HashMap<>(); // Maps BusNum load point to X point for the parametrised model
	public static Map<Integer, String> XPointtoBusNumWT = new HashMap<>(); // reverse mapping
	public static Map<String, Integer> BusNumtoXPointPV = new HashMap<>(); // Maps BusNum load point to X point for the parametrised model
	public static Map<Integer, String> XPointtoBusNumPV = new HashMap<>(); // reverse mapping
	public static Map<String, Integer> BusNumtoXPointMG = new HashMap<>(); // Maps BusNum load point to X point for the parametrised model
	public static Map<Integer, String> XPointtoBusNumMG = new HashMap<>(); // reverse mapping
	public static Map<String, Integer> BusNumtoXPoint = new HashMap<>(); // Maps BusNum load point to X point for the parametrised model
	public static Map<Integer, String> XPointtoBusNum = new HashMap<>(); // reverse mapping
	
	// reverse mapping
 	public static String httpReqCSV = new String("Y:/httpReq.CSV"); // (mjk, 151115) differentiating function calls "Run PowerWorld" and "Run parameterised PW"
 	
 	
	public static String XVALUE = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUEBat.CSV");
	public static String BatPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/BatPIN.CSV"); 
	public static String BatVIN = new String("C:/apache-tomcat-8.0.24/webapps/input/BAtVIN.CSV");
	public static String BatQIN = new String("C:/apache-tomcat-8.0.24/webapps/input/BAtQIN.CSV");
		public static String PrPWOUTBatCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUTBat.CSV");
	public static String Sim1 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim_battery1");  
	
	public static String XVALUE5 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUEMGen.CSV");
	public static String MGenPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/MGenPIN.CSV"); 
	public static String MGenVIN = new String("C:/apache-tomcat-8.0.24/webapps/input/MGenVIN.CSV");
	public static String PrPWOUTMGenCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUTMGen.CSV");
	public static String Sim5 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim_marine1"); 
	
	public static String XVALUE4 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUEPV.CSV");
	public static String PVPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/PVPIN.CSV"); 
	public static String PVVIN = new String("C:/apache-tomcat-8.0.24/webapps/input/PVVIN.CSV");
	public static String PrPWOUTPVCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUTPV.CSV");
	public static String Sim4 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim_PV1"); 
	
	public static String XVALUE3 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUE.CSV");
	public static String WPGPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/WPGPIN.CSV");
	public static String WPGVIN = new String("C:/apache-tomcat-8.0.24/webapps/input/WPGVIN.CSV");
	public static String PrPWOUT2CSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUT2.CSV");
	public static String Sim3 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim_wt2"); 
	
	public static String XVALUE2 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUELP.CSV");
	public static String LPPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/LPPIN.CSV"); 
	public static String LPQIN = new String("C:/apache-tomcat-8.0.24/webapps/input/LPQIN.CSV");
	public static String PrPWOUTLPCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUTLP.CSV");
	public static String Sim2 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim_loadpoints1");
	
	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/semakausimulator/SemakauPWrun.pyw");
	public static String SMGINCSV = new String("C:/apache-tomcat-8.0.24/webapps/semakausimulator/SMGIN.CSV");                   // specifies where the SMGIN.CSV file is written to and read from.
	public static String SMGBUSCSV = new String("C:/apache-tomcat-8.0.24/webapps/semakausimulator/SMGBUS.CSV");                 // specifies where the SMGBUS.CSV file is written to and read from.
	
	public runpower() {
		super();
		// Hard Coded ArcGISFIDtoPWBusNum
		// Hard Coded ArcGISFIDloadtoPWBusNum (from the load points layer)
        
				ArcGISFIDloadtoPWBusNum.put( "1","1");    // NEA transfer hall/ Hub         AC Load
				ArcGISFIDloadtoPWBusNum.put( "2","24");    // Phase1 (admin buildings1) AC Load
				ArcGISFIDloadtoPWBusNum.put( "3","25");    // Phase1 (admin buildings2) AC Load
				ArcGISFIDloadtoPWBusNum.put( "4","26");    // Phase1 (admin buildings3) AC Load
				ArcGISFIDloadtoPWBusNum.put( "6","16");   // Desalination Plant 1     AC Load 
				ArcGISFIDloadtoPWBusNum.put( "7","15");   // Desalination Plant 2     AC Load 
				ArcGISFIDloadtoPWBusNum.put( "8","13");   // Desalination Plant 3     AC Load 
				ArcGISFIDloadtoPWBusNum.put( "9","14");   // Fish Hatchery            AC Load 
				ArcGISFIDloadtoPWBusNum.put("10","17");   // AC  loads (Big Windfarm) 
				ArcGISFIDloadtoPWBusNum.put("11","18");   // AC  loads1 (microgrid 1 Windfarm)
				ArcGISFIDloadtoPWBusNum.put("12","20");   // AC loads2 (microgrid 2 Windfarm )
				ArcGISFIDloadtoPWBusNum.put("13","22");   // AC loads3 (microgrid 3 Windfarm )
				
				
				//Hard Coded ArcGISFIDgentoPWBusNum (from the wind turbine)
				ArcGISFIDgenwindtoPWBusNum.put( "1","29");    // Large Windturbine  AC generator 
				ArcGISFIDgenwindtoPWBusNum.put( "2","30");    // Microgrid 1 Windturbine  AC generator 
				ArcGISFIDgenwindtoPWBusNum.put( "3","33");    // Microgrid 2 Windturbine  AC generator 
				ArcGISFIDgenwindtoPWBusNum.put( "4","35");    // Microgrid 3 Windturbine  AC generator 
				
				//Hard Coded ArcGISFIDgentoPWBusNum (from the PVfarm)
				ArcGISFIDgenpvtoPWBusNum.put( "1","6");    // Microgrid 1 PVfarm  DC generator  
				ArcGISFIDgenpvtoPWBusNum.put( "2","8");   // Microgrid 2 PVfarm  DC generator 
				ArcGISFIDgenpvtoPWBusNum.put( "3","10");  // Microgrid 3 PVfarm  DC generator 
				
				//Hard Coded ArcGISFIDgentoPWBusNum (from the dieselgen)
				ArcGISFIDgendieseltoPWBusNum.put( "1","11");  //  Diesel generator active 
				ArcGISFIDgendieseltoPWBusNum.put( "2","12");  //  Diesel generator standby 
				
				//Hard Coded ArcGISFIDgentoPWBusNum (from the marinefarm)
				ArcGISFIDgenmarinetoPWBusNum.put( "1","27");  //  Marinefarm AC generator 
				
				
				//Hard Coded ArcGISFIDbattoPWBusNum (from the energy storage layer)
						ArcGISFIDbattoPWBusNum.put( "1","32");    // Microgrid 2 energy storage 
						ArcGISFIDbattoPWBusNum.put( "2","34");    // Microgrid 3 energy storage 
						ArcGISFIDbattoPWBusNum.put( "3","31");    // Microgrid 1 energy storage 
						ArcGISFIDbattoPWBusNum.put( "4","28");    // Large energy storage 
						
				for (Map.Entry<String, String> entry : ArcGISFIDloadtoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFIDload.put(entry.getValue(), entry.getKey());
				}

				for (Map.Entry<String, String> entry : ArcGISFIDgenmarinetoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFIDgenmarine.put(entry.getValue(), entry.getKey());
				}
				for (Map.Entry<String, String> entry : ArcGISFIDgendieseltoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFIDgendiesel.put(entry.getValue(), entry.getKey());
				}
				for (Map.Entry<String, String> entry : ArcGISFIDgenpvtoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFIDgenpv.put(entry.getValue(), entry.getKey());
				}
				for (Map.Entry<String, String> entry : ArcGISFIDgenwindtoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFIDgenwind.put(entry.getValue(), entry.getKey());
				}
				for (Map.Entry<String, String> entry : ArcGISFIDbattoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFIDbat.put(entry.getValue(), entry.getKey());
				}
				
				BusNumtoXPoint.put("28", 0);
				BusNumtoXPoint.put("31", 1);
				BusNumtoXPoint.put("32", 2);
				BusNumtoXPoint.put("34", 3);
				//marine
				BusNumtoXPointMG.put("27", 0);
				//pv
				BusNumtoXPointPV.put("6", 0);
				BusNumtoXPointPV.put("8", 1);
				BusNumtoXPointPV.put("10", 2);
				//LP
				BusNumtoXPointLP.put("1", 0);
				BusNumtoXPointLP.put("13", 1);
				BusNumtoXPointLP.put("14", 2);
				BusNumtoXPointLP.put("15", 3);
				BusNumtoXPointLP.put("16", 4);
				BusNumtoXPointLP.put("17", 5);
				BusNumtoXPointLP.put("18", 6);
				BusNumtoXPointLP.put("20", 7);
				BusNumtoXPointLP.put("22", 8);
				BusNumtoXPointLP.put("24", 9);
				BusNumtoXPointLP.put("25", 10);
				BusNumtoXPointLP.put("26", 11);
				//wt
				BusNumtoXPointWT.put("29", 0);
				BusNumtoXPointWT.put("30", 1);
				BusNumtoXPointWT.put("33", 2);
				BusNumtoXPointWT.put("35", 3);
				
			for (Map.Entry<String, Integer> entry : BusNumtoXPoint.entrySet()) { // reverse mapping
		 XPointtoBusNum.put(entry.getValue(), entry.getKey());
		           }
			
			for (Map.Entry<String, Integer> entry : BusNumtoXPointPV.entrySet()) { // reverse mapping
				 XPointtoBusNumPV.put(entry.getValue(), entry.getKey());
				           }
			for (Map.Entry<String, Integer> entry : BusNumtoXPointMG.entrySet()) { // reverse mapping
				 XPointtoBusNumMG.put(entry.getValue(), entry.getKey());
				           }
			for (Map.Entry<String, Integer> entry : BusNumtoXPointWT.entrySet()) { // reverse mapping
				 XPointtoBusNumWT.put(entry.getValue(), entry.getKey());
				           }
			for (Map.Entry<String, Integer> entry : BusNumtoXPointLP.entrySet()) { // reverse mapping
				 XPointtoBusNumLP.put(entry.getValue(), entry.getKey());
				           }

		    }
	public void runSemPW(ArrayList<String[]> editStack) {
		ArrayList<String[]> skeleton = new ArrayList<String[]>(); // array of strings containing PW fields, ArcGIS fields corresponding to PW fields and PW object type
		ArrayList<Map<String, Object>> attributeslist = new ArrayList<Map<String, Object>>();
		ArrayList<String> layers = new ArrayList<String>();
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau"); // Access secure feature layer service using login username  and password

		for (int i = 0; i < editStack.size(); i++) { // for each feature in editStack, append something to skeleton, attributeslist and layers
			String layer = (String) editStack.get(i)[0];
			String graphicFID = (String) editStack.get(i)[1];
			// String appCallFlag = (String) editStack.get(i)[3];

			QueryParameters qParameter = new QueryParameters();
			qParameter.setWhere("OBJECTID='" + graphicFID + "'"); // find graphic using FID
			qParameter.setOutFields(new String[] { "*" }); // fetch all attributes using *
			QueryTask qTask = null;
			Feature graphic = null;

			if (layer.equals("WindTurbine") || layer.equals("Loadpoint") || layer.equals("Marinefarm") || layer.equals("PVfarm") // check if feature in editStack is part of power grid
					|| layer.equals("EnergyStorage") || layer.equals("DieselGen")) { // PW variable names can be found in Case Object Fields.xslx

				if (layer.equals("Loadpoint")) {                                                                                           // variable names specific to load points (e.g. LoadMW=pwr_P,  LoadMVR=pwr_Q)
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,V_nom_kV", "Bus" });
					skeleton.add(new String[] { "BusNum,LoadID,LoadMW,LoadMVR", "OBJECTID,LoadID,P_set_MW,Q_set_MVar", "Load" });                    // can  only  modify MW and MVR at load, not bus
					try {
						qTask = new QueryTask( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint2/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				} else if (layer.equals("WindTurbine")) {
					skeleton.add(new String[] { "BusNum,GenID,GenMW,BusNomVolt", "OBJECTID,GenID,P_set_MW,V_set_kV", "Gen" });
					skeleton.add(new String[] { "BusNum,GenID,GenMW,BusNomVolt", "OBJECTID,GenID,P_set_MW,V_set_kV", "Gen" });                                               // low voltage bus
					try {
						qTask = new QueryTask( "http://services3.arcgis.com/785KAqvbaBANxwtT/ArcGIS/rest/services/windturbine2/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				} else if (layer.equals("Marinefarm")) {
					skeleton.add(new String[] { "BusNum,GenID,GenMW,BusNomVolt", "OBJECTID,GenID,P_set_MW,V_set_kV", "Gen" });
					skeleton.add(new String[] { "BusNum,GenID,GenMW,BusNomVolt", "OBJECTID,GenID,P_set_MW,V_set_kV", "Gen" });
					try {
						qTask = new QueryTask( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Marinefarm2/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				} else if (layer.equals("PVfarm")) { 
					skeleton.add(new String[] { "BusNum,GenID,GenMW,BusNomVolt", "OBJECTID,GenID,P_set_MW,V_set_kV", "Gen" });
					skeleton.add(new String[] { "BusNum,GenID,GenMW,BusNomVolt", "OBJECTID,GenID,P_set_MW,V_set_kV", "Gen" });                  // duplicate because generator only needs to update one  bus
					try {
						qTask = new QueryTask( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/PVfarm2/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				}else if (layer.equals("EnergyStorage")) {
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,V_nom_kV", "Bus" });
					skeleton.add(new String[] { "BusNum,LoadID,LoadMW,LoadMVR", "OBJECTID,LoadID,P_set_MW,Q_set_MVar", "Load" });
						try {
							qTask = new QueryTask( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/EnergyStorage2/FeatureServer/0", user);
						} catch (EsriSecurityException e) {
							e.printStackTrace();
						}
				}else if (layer.equals("DieselGen")) {
					skeleton.add(new String[] { "BusNum,GenID,BusNomVolt,GenMVR", "OBJECTID,GenID,theta_set,V_set_kV", "Gen" });
					skeleton.add(new String[] { "BusNum,GenID,BusNomVolt,GenMVR", "OBJECTID,GenID,theta_set,V_set_kV", "Gen" });
							try {
								qTask = new QueryTask( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/DieselGen2/FeatureServer/0", user);
							} catch (EsriSecurityException e) {
								e.printStackTrace();
							}
				}
				//
				// EXPAND CODE HERE
				//
				try {
					FeatureResult fResult = qTask.execute(qParameter);
					graphic = (Feature) fResult.iterator().next(); // queryResult.iterator() should only return one feature
				} catch (Exception e) {
					e.printStackTrace();
				}

				attributeslist.add(graphic.getAttributes()); // map of attributes
				attributeslist.add(graphic.getAttributes()); // each feature has to update two elements in  PowerWorld, hence duplicate
				layers.add(layer); // layer reference
				layers.add(layer);

			}
		} // of for()

		if (!skeleton.isEmpty()) {                                     // run PowerWorld only if editStack contains power grid features
			writeCSV(skeleton, attributeslist, layers);                      // this was changed to also pass attributeslist_LP ArrayList
			for (int i = 0; i < editStack.size(); i++) {                // for each feature in editStack, append something to skeleton, attributeslist and layers
				runPyScript(editStack); // ZL-151216
				readCSV(); // ZL-151216
				break;
			}
		}
	} // of runPowerWorld()
	
	public void runPyScript(ArrayList<String[]> editStack) {
		String appCallFlag = null;
		appCallFlag = editStack.get(0)[2];                                               // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)

		try {
			System.out.println(appCallFlag);
			switch (appCallFlag) {

			
			case ("PW"):                                                           // when appCallFlag=pw indicating that the run powerworld button has been pressed, then the following actions are going to be taken
				System.out.println(appCallFlag + " Button was pressed! (runPyScript)");
				Process p1 = Runtime.getRuntime().exec(runPythonCommand);
				p1.waitFor();
				System.out.println("Exit Value (0 means success): " + p1.exitValue()); // if console prints 0 it means success
				BufferedReader br1 = new BufferedReader(new InputStreamReader( p1.getInputStream()));
				String line1;                                                         // retrieves console from python script
				System.out.println("Python input:");
				while ((line1 = br1.readLine()) != null) {
					System.out.println(line1);                                         // print input array from Python (see python code for more details)
				}
				String line;
				line = br1.readLine();
				break;
						
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}//end of run pyscript
	
	public void writeCSV(ArrayList<String[]> skeleton, ArrayList<Map<String, Object>> attributeslist, 
			ArrayList<String> layers) {                                                         //write input file to python
		
		FileWriter fileWriter = null;
		try {
			fileWriter = new FileWriter(SMGINCSV);
			for (int i=0; i<skeleton.size(); i++) {                                             // for each entry in editStack
				fileWriter.append(skeleton.get(i)[0]);                                          // write headers (PowerWorld field names)
				fileWriter.append("\n");                                                        // new line
				String[] ArcGISfields = skeleton.get(i)[1].split(",");                          // produce iterable list from comma separated string (e.g. ["FID", "volt_nom"])
				Map<String, Object> attributes = attributeslist.get(i);
				String layer = layers.get(i);

				for (int j=0; j<ArcGISfields.length; j++) {                                     // for each ArcGIS field, append corresponding values
//  1
					if (layer.equals("Loadpoint")) {                                     // specific to "Loadpoint"
						if (ArcGISfields[j].equals("OBJECTID")) {                                    // ArcGIS element is FID
							String ArcGISOBJECTID = String.valueOf(attributes.get("OBJECTID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDloadtoPWBusNum.get(ArcGISOBJECTID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("LoadID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
//  2
					} else if (layer.equals("EnergyStorage")) {                                 // specific to "EnergyStorage"
						if (ArcGISfields[j].equals("OBJECTID")) {                                    // ArcGIS element is FID
							String ArcGISOBJECTID = String.valueOf(attributes.get("OBJECTID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDbattoPWBusNum.get(ArcGISOBJECTID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("LoadID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
						
						
//3a	
						} else if (layer.equals("DieselGen")) {                                 // specific to "dieselgen"
						if (ArcGISfields[j].equals("OBJECTID")) {                                    // ArcGIS element is FID
							String ArcGISOBJECTID = String.valueOf(attributes.get("OBJECTID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDgendieseltoPWBusNum.get(ArcGISOBJECTID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
						
//3b
						
					} else if (layer.equals("PVfarm")) {                                 // specific to "pvfarm"
						if (ArcGISfields[j].equals("OBJECTID")) {                                    // ArcGIS element is FID
							String ArcGISOBJECTID = String.valueOf(attributes.get("OBJECTID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDgenpvtoPWBusNum.get(ArcGISOBJECTID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
						
//3c						
					} else if (layer.equals("WindTurbine")) {                                 // specific to "EnergyStorage"
						if (ArcGISfields[j].equals("OBJECTID")) {                                    // ArcGIS element is FID
							String ArcGISOBJECTID = String.valueOf(attributes.get("OBJECTID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDgenwindtoPWBusNum.get(ArcGISOBJECTID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
						
						

//  3
					} else if (layer.equals("Marinefarm")) {                                 // specific to "EnergyStorage"
						if (ArcGISfields[j].equals("OBJECTID")) {                                    // ArcGIS element is FID
							String ArcGISOBJECTID = String.valueOf(attributes.get("OBJECTID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDgenmarinetoPWBusNum.get(ArcGISOBJECTID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}


					}  
					
					fileWriter.append(",");                                                     // separate values with a comma
				}
				fileWriter.append("\n");
				fileWriter.append(skeleton.get(i)[2]);                                          // PW object type (e.g. BUS, GEN, LOAD)
				fileWriter.append("\n");
				// each item is given three rows: headers (name of attributes), corresponding values and type of object (e.g. BUS, GEN, etc)
			} // of for (int i=0; i<skeleton.size(); i++)
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileWriter.flush();
				fileWriter.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		} // of finally
	} // of public void writeCSV()/**this method reads the csv file generated by the powerworld model and update to ArcGIS database */
	public void readCSV() {
		BufferedReader fileReader = null;
	    UserCredentials user = new UserCredentials();
	    user.setUserAccount("semakausimulator", "c4tsemakau");                                      // Access secure feature layer service using login user name and password
		try {
			long start = System.currentTimeMillis();                                                // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(SMGBUSCSV));
			fileReader.readLine();                                                                  // Read the CSV file header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL"); // Load all features using SQL command

			GeodatabaseFeatureServiceTable LoadpointTable       = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint2/FeatureServer", user, 0);
			LoadpointTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadpointTable.initialize();
			
			GeodatabaseFeatureServiceTable EnergyStorageTable   = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/EnergyStorage2/FeatureServer", user, 0);
			EnergyStorageTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			EnergyStorageTable.initialize();
			/*GeodatabaseFeatureServiceTable LoadTLineTable       = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/LoadTLine2/FeatureServer", user, 0);
			LoadTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadTLineTable.initialize();*/
			GeodatabaseFeatureServiceTable MarinePowerGenTable  = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Marinefarm2/FeatureServer", user, 0);
			MarinePowerGenTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MarinePowerGenTable.initialize();
			/*GeodatabaseFeatureServiceTable MarineTLineTable     = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/MarineTLine2/FeatureServer", user, 0);
			MarineTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MarineTLineTable.initialize();*/
			/*GeodatabaseFeatureServiceTable SolarfarmTable       = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Solarfarm2/FeatureServer", user, 0);
			SolarfarmTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			SolarfarmTable.initialize();/*
			/*GeodatabaseFeatureServiceTable SolarPowerTLineTable = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/SolarPowerTLine2/FeatureServer", user, 0);
			SolarPowerTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			SolarPowerTLineTable.initialize();*/
			GeodatabaseFeatureServiceTable PVfarmTable        = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/PVfarm2/FeatureServer", user, 0);
			PVfarmTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			PVfarmTable.initialize();
			GeodatabaseFeatureServiceTable DieselGeneratorTable = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/DieselGen2/FeatureServer", user, 0);			
			DieselGeneratorTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			DieselGeneratorTable.initialize();
			GeodatabaseFeatureServiceTable WindTurbineTable     = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/windturbine2/FeatureServer", user, 0);			
			WindTurbineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			WindTurbineTable.initialize();

			//
			// EXPAND CODE HERE
			//
			
			final CountDownLatch latch = new CountDownLatch(6); // handles four asynchronous processes, only continues Thread when it reaches 0
			LoadpointTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
					if (status==true) {
						latch.countDown(); // latch decrement if feature service table is ready
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			WindTurbineTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) {
					if (status==true) {
						latch.countDown();
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			PVfarmTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
					if (status==true) {
						latch.countDown(); // latch decrement if feature service table is ready
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			EnergyStorageTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) {
					if (status==true) {
						latch.countDown();
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			DieselGeneratorTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
					if (status==true) {
						latch.countDown(); // latch decrement if feature service table is ready
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			MarinePowerGenTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) {
					if (status==true) {
						latch.countDown();
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			//
			// EXPAND CODE HERE
			//

			// update the listOfTables below to ensure that all the tables are initialized properly
//	EnergyStorageTable,LoadTLineTable,MarinePowerGenTable,MarineTLineTable,SolarfarmTable,SolarPowerTLineTable,WindfarmTable,WindPowerTLineTable,WindTransformerTable,
//			GeodatabaseFeatureServiceTable[] listOfTables = {
//				LoadpointTable, WindTurbineTable
//			};

//			loadTables(listOfTables, latch);                                                  // helper function

			latch.await();                                                                    // wait until all feature service tables are ready then continue
			while ((line=fileReader.readLine())!=null) {                                      // Continue reading lines until none left
				String[] data = line.split(",");                                              // split string by comma

				// data[0]=BUSNUM
				// data[1]=BUSNAME
				// data[2]=BUSLOADMW
				// data[3]=BUSLOADMVR
				// data[4]=BUSNOMVOLT
				// data[5]=BUSKVVOLT
				// data[6]=BUSANGLE
				// data[7]=BUSGENMW
				// data[8]=BUSGENMVR

				int PWBusNum = Integer.valueOf(data[0].trim());                              // data[0] is the bus number
				String ArcGISOBJECTID = PWBusNumtoArcGISFIDload.get(data[0].trim());                  // .trim() removes trailing white spaces
				String ArcGISbatOBJECTID = PWBusNumtoArcGISFIDbat.get(data[0].trim());
				String ArcGISgenwindOBJECTID = PWBusNumtoArcGISFIDgenwind.get(data[0].trim());
				String ArcGISgenpvOBJECTID = PWBusNumtoArcGISFIDgenpv.get(data[0].trim());
				//String ArcGISgendieselOBJECTID = PWBusNumtoArcGISFIDgendiesel.get(data[0].trim());
				String ArcGISgenmarineOBJECTID = PWBusNumtoArcGISFIDgenmarine.get(data[0].trim());
				
				
				if (ArcGISOBJECTID != null) {                                                     // if PowerWorld bus number can map to an ArcGIS LoadPoint FID
					Map<String, Object> LoadpointAttributes = LoadpointTable.getFeature(Long.parseLong(ArcGISOBJECTID)).getAttributes();
					if (!data[2].trim().isEmpty()) { // if not an empty string
						// ArcGIS layer specifies that pwr_P is of data type Float
						// overwrite values using attributes.put()
/*						LoadpointAttributes.put("pwr_P", Float.parseFloat(data[2].trim())); // BUSLOADMW
					}
					if (!data[3].trim().isEmpty()) {
						LoadpointAttributes.put("pwr_Q", Float.parseFloat(data[3].trim())); // BUSLOADMVR
					}	
					if (!data[4].trim().isEmpty()) {
						LoadpointAttributes.put("volt_nom", Float.parseFloat(data[4].trim())); // BUSNOMVOLT						
					}
					if (!data[5].trim().isEmpty()) {
						LoadpointAttributes.put("volt_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT						
					}
					if (!data[6].trim().isEmpty()) {
						LoadpointAttributes.put("theta_act", Float.parseFloat(data[6].trim())); // BUSANGLE
					}
*/					
					// function below writes
					// data[2] to pwr_P, 
					// data[3] to pwr_Q, 
					// data[4] to volt_nom,
					// data[5] to volt_act,
					// data[6] to theta_act,
					writeDesiredOutputs("Pwr_P_MW,Pwr_Q_MVar,V_nom_kV,V_act_kV,theta_act", new int[]{2,3,4,5,6}, data, LoadpointAttributes);
					}
					LoadpointTable.updateFeature(Long.parseLong(ArcGISOBJECTID), LoadpointAttributes);   // update feature table locally
				}

				
				if (ArcGISgenwindOBJECTID != null) {
					Map<String, Object> WindTurbineAttributes = WindTurbineTable.getFeature(Long.parseLong(ArcGISgenwindOBJECTID)).getAttributes();
				
					if (PWBusNum==29 || PWBusNum==30 || PWBusNum==33 || PWBusNum==35 ) {                                                                 // WindTurbine generator buses
					 // subtract 4 from PWBusNum to get FID
					writeDesiredOutputs("V_act_kV,theta_act,P_out_MW,Q_out_MVar", new int[] {5,6,7,8}, data, WindTurbineAttributes);
				}
					WindTurbineTable.updateFeature(Long.parseLong(ArcGISgenwindOBJECTID), WindTurbineAttributes);
				}

				
				if (ArcGISgenpvOBJECTID != null) {
					Map<String, Object> PVAttributes = PVfarmTable.getFeature(Long.parseLong(ArcGISgenpvOBJECTID)).getAttributes();
				
				if (PWBusNum==6 || PWBusNum==8 || PWBusNum==10 ) {                                                             // alternative for WindTurbine generator buses
					writeDesiredOutputs("V_act_kV,theta_act,P_out_MW,Q_out_Mvar", new int[] {5,6,7,8}, data, PVAttributes);
				}
				PVfarmTable.updateFeature(Long.parseLong(ArcGISgenpvOBJECTID), PVAttributes);
				}			


				if (ArcGISgenmarineOBJECTID != null) {
					Map<String, Object> MarineGenAttributes = MarinePowerGenTable.getFeature(Long.parseLong(ArcGISgenmarineOBJECTID)).getAttributes();
				
				if (PWBusNum==27 ) {                                                             // alternative for WindTurbine generator buses
					writeDesiredOutputs("V_act_kV,theta_act,P_out_MW,Q_out_Mvar", new int[] {5,6,7,8}, data, MarineGenAttributes);
				}
				MarinePowerGenTable.updateFeature(Long.parseLong(ArcGISgenmarineOBJECTID), MarineGenAttributes);
				}		
				
				
				if (ArcGISbatOBJECTID != null) {
					Map<String, Object> batteryAttributes = EnergyStorageTable.getFeature(Long.parseLong(ArcGISbatOBJECTID)).getAttributes();
				
				if (PWBusNum==28 || PWBusNum==31 || PWBusNum==32 || PWBusNum==34 ) {                                                             // alternative for WindTurbine generator buses
					writeDesiredOutputs("V_act_kV,theta_act,P_out_MW,Q_out_Mvar", new int[] {5,6,7,8}, data, batteryAttributes);
				}
				EnergyStorageTable.updateFeature(Long.parseLong(ArcGISbatOBJECTID), batteryAttributes);
				}


				
			} // of while()
				LoadpointTable.applyEdits(null); // commit local updates onto server
				WindTurbineTable.applyEdits(null);
				EnergyStorageTable.applyEdits(null); // commit local updates onto server
				MarinePowerGenTable.applyEdits(null);
				DieselGeneratorTable.applyEdits(null); // commit local updates onto server
				PVfarmTable.applyEdits(null);
				
//				for (GeodatabaseFeatureServiceTable table : listOfTables) {                                 // commit local updates onto server
//				table.applyEdits(null);
//				}

			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis()-start) + "ms"); // tells you how long it took to update
			System.out.println("PowerWorld has finished running!");

		} // of try
		catch (Exception e) {e.printStackTrace();}
		finally {
			try {fileReader.close();}
			catch (IOException e) {e.printStackTrace();}
		}
	} // of readCSV()
	
	public void writeDesiredOutputs(String headers, int[] dataIndex, String[] data, Map<String,Object> attributes) {		
		String[] headersList = headers.split(",");                                             // number of headers in headersList must be equal to number of integers in dataIndex
		for (int i=0; i<headersList.length; i++) {
			String value = data[dataIndex[i]].trim();                                          // remove whitespaces
			if (!value.isEmpty()) {                                                            // value is not empty
				attributes.put(headersList[i], Float.parseFloat(value));                       // value must be of type float
			}
		}
	}
	
	
	
public void runPrPowerWorld1(ArrayList<String[]> editStack) {
		
		end_time = System.currentTimeMillis();
		ArrayList<Map<String, Object>> attributeslist_BG = new ArrayList<Map<String, Object>>(); // additional ArrayList for batterypowergen
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		int counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
		
		
		for (String key : ArcGISFIDbattoPWBusNum.keySet()) {
			try {

				QueryParameters qParameter_BG = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for  predefined data
				qParameter_BG.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
				qParameter_BG.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_BG = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_BG = null; // create an instance of Feature to store an ArcGIS element

				qTask_BG = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/EnergyStorage2/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_BG = qTask_BG.execute(qParameter_BG); // FeatureResult is used to store information from ArcGIS database requested using qParameter_PG and qTask_PG
				graphic_BG = (Feature) fResult_BG.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_PG and stores it in graphic_PG; qParameter_PG requests information about a single element only
				attributeslist_BG.add(graphic_BG.getAttributes()); // append information about the element in graphic_PG to ArrayList attributeslist_LP

				counter++;
				if (counter == 4) {
					break;
				}
			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err.
			}
		}
		System.out.println("success step 1");
		
		writePrPWCSV(attributeslist_BG);/* attributeslist_LP,attributeslist_PVG,attributeslist_MG, attributeslist_BG,attributeslist_DG*/

		System.out.println("success writing csv");
		
// the following code extract the required input data set from arcgis database
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);
		List<List<Double>> xData = new ArrayList<>(1);				
		List<Double> xRow = new ArrayList<>();
		List<List<Double>> yData;

		String[] ArcGISFID = null;
		ArcGISFID = new String[4];                        // for the simplified parameterised PW model, 6 inputs from 3 of the BusNum are required

		for (int j = 0; j < 4; j++) {
			String BusNum = XPointtoBusNum.get(j);          // get the BusNum so that we can look for the ArcGIS FID, then extract the input x-value
			System.out.println(BusNum);
			ArcGISFID[j] = PWBusNumtoArcGISFIDbat.get(BusNum);  // get the ArcGIS FID for the input x-values
		}

		BufferedReader fileReader = null;
		BufferedReader fileReader1 = null;
		BufferedReader fileReader2 = null;
		FileWriter XValue = null;

		try {
			
			XValue = new FileWriter(XVALUE);
			
			for (int k = 0; k < 4; k++) {
				String line = null;
				fileReader = new BufferedReader(new FileReader(BatPIN)); // read the pwr_p data from CSV file
				fileReader.readLine();                                  // Read the CSV file header to skip it
				while ((line = fileReader.readLine()) != null) {
					String[] data = line.split(",");
					System.out.println(ArcGISFID[k] + " & " + data[1]);
					if (ArcGISFID[k].equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
						break;
					}
				}
			}
			
			for (int k = 0; k < 4; k++) {
				String line1 = null;
				fileReader1 = new BufferedReader(new FileReader(BatQIN)); // read the pwr_p data from CSV file
				fileReader1.readLine();                                  // Read the CSV file header to skip it
				while ((line1 = fileReader1.readLine()) != null) {
					String[] data = line1.split(",");
					System.out.println(ArcGISFID[k] + " & " + data[1]);
					if (ArcGISFID[k].equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k+4 + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue1" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
						break;
					}
				}
			}

			for (int k = 0; k < 4; k++) {
				String line2 = null;
				fileReader2 = new BufferedReader(new FileReader(BatVIN)); // read the  pwr_Q data from CSV file
				fileReader2.readLine(); 
				while ((line2 = fileReader2.readLine()) != null) {
					String[] data = line2.split(",");
					if (ArcGISFID[k].equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k+8 + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue2" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));                            // convert string to double and add to xRow
						break;
					}
				}
			}
			
			
			XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
			XValue.close();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
				fileReader1.close();
				fileReader2.close();
				
				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		// end of extracting the input data set from arcgis database

	xData.add(xRow);                                                                                  // pass all the collected input x-value to xData
				
		System.out.println("xData=" + xData);

		String simDir = Sim1;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {			
			// System.out.println(MoDSAPI.class);
			fileWriter = new FileWriter(PrPWOUTBatCSV); // filewriter for the
			System.load("C:/apache-tomcat-8.0.24/webapps/batlib/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line		
			
//			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);

				for (int j = 0; j < yNames.size(); j++) {
					fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
					fileWriter.append(",");
				}
				
			} catch (Error e) {
		e.printStackTrace();
				
			} catch (IOException e) {
		e.printStackTrace();			
			}

		yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
		System.out.println("Success!");														
		System.out.println("yData=" + yData); 
		
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
		
		end_time = System.currentTimeMillis();
		interval = end_time - start_time;
		System.out.println("The Process Took You: "+ interval + "ms");

		// write the output data to console
	    readPrPWCSV();

	}/**This method:
     * 1) collects all the input data for the PowerWorld model of the whole Jurong Island;
     * 2) calls python script, run the PowerWorld model, generate output csv file
     * 3) update the output to ArcGIS database (the updating function is still requires debug)*/
	
		
	
	public void writePrPWCSV(ArrayList<Map<String, Object>> attributeslist_BG)
	
	{

		FileWriter fW_BG_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
		FileWriter fW_BG_V = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.
		FileWriter fW_BG_Q = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

		
		try {
			
			fW_BG_P = new FileWriter(BatPIN);
			fW_BG_Q = new FileWriter(BatQIN);
			fW_BG_V = new FileWriter(BatVIN);

			
			fW_BG_P.append("   ,OBJECTID, P_set_MW, V_set_kV");
			fW_BG_P.append("\n");
			fW_BG_Q.append("   ,OBJECTID, Q_set_Mvar");
			fW_BG_Q.append("\n");
			fW_BG_V.append("   ,OBJECTID, V_set_kV");
			fW_BG_V.append("\n");
			
			
			
			//for loadpoint  writing
			for (int i = 0; i < attributeslist_BG.size(); i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
				
				if (i == 4) {				
						break;
					}
					for (String key : attributeslist_BG.get(i).keySet()) { // access all feature fields ("key") of the Load_Point  ("LP") layer
						if (key == "OBJECTID") {
							fW_BG_P.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
							fW_BG_P.append(","); // ZL-151218
							fW_BG_P.append(String.valueOf(attributeslist_BG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_BG_P.append(", ");
							fW_BG_P.append(String.valueOf(attributeslist_BG.get(i).get("P_set_MW"))); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
							fW_BG_P.append(", ");
							fW_BG_P.append(String.valueOf(attributeslist_BG.get(i).get("V_set_kV")));
							fW_BG_P.append("\n");

							fW_BG_V.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
							fW_BG_V.append(","); // ZL-151218
							fW_BG_V.append(String.valueOf(attributeslist_BG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_BG_V.append(", ");
							fW_BG_V.append(String.valueOf(attributeslist_BG.get(i).get("V_set_kV"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
							fW_BG_V.append("\n");
						
							fW_BG_Q.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
							fW_BG_Q.append(","); // ZL-151218
							fW_BG_Q.append(String.valueOf(attributeslist_BG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_BG_Q.append(", ");
							fW_BG_Q.append(String.valueOf(attributeslist_BG.get(i).get("Q_set_Mvar"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
							fW_BG_Q.append("\n");
						}
					}
				}

			
			
			fW_BG_P.flush(); // passes the data from fW_PG to PGIN.csv
			fW_BG_V.flush(); // passes the data from fW_PG to PGIN.csv
			fW_BG_Q.flush(); // passes the data from fW_PG to PGIN.csv
			
			
			fW_BG_P.close();
			fW_BG_Q.close();
			fW_BG_V.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	// update the ArcGIS database according to the output of the PrPW model ZL-20160111
	public void readPrPWCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrPWOUTBatCSV));
			fileReader.readLine();     // Read the CSV file header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable BGTable = new GeodatabaseFeatureServiceTable( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/EnergyStorage2/FeatureServer", user, 0);
			BGTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			BGTable.initialize();
			System.out.println("status of battery=" + BGTable.getStatus());
			BGTable.getInitializationError();								                                  

			final CountDownLatch latch = new CountDownLatch(1); // ZL-151207 handles one asynchronous processes, only continues Thread when it reaches 0
			BGTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
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
//				System.out.println("data= " + data);
				
//				String[] ArcGISFID = null;
//				ArcGISFID = new String[209]; 
				

				
				for (int j = 0; j < 4; j++) {
					System.out.println(j);
					String PWBusNum = XPointtoBusNum.get(j);
					System.out.println(PWBusNum);
					String ArcGISFID = PWBusNumtoArcGISFIDbat.get(PWBusNum);
					
//					String BusNum = XPointtoBusNum.get(j);
//					System.out.println(BusNum);
//					ArcGISFID = PWBusNumtoArcGISFID.get(BusNum);
					System.out.println(ArcGISFID);
					
					int i;
					if (j==1){
					 i=3*j;
					}
					else
					{ i=2*j;
					
					}
										
					if (ArcGISFID != null) {
						Map<String, Object> BGAttributes = BGTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
						if (!data[162 + 7 * i].trim().isEmpty()) {
							BGAttributes .put("theta_act", Float.parseFloat(data[162 + 7 * i] .trim())); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
						}
						if (!data[163 + 7 * i].trim().isEmpty()) { 
							BGAttributes.put("V_act_kV", Float.parseFloat(data[163 + 7 * i].trim()));
						}
						if (!data[166 + 7 * i].trim().isEmpty()) {
							BGAttributes.put("P_out_MW",Float.parseFloat(data[166 + 7 * i].trim()));
						}
						if (!data[167 + 7 * i].trim().isEmpty()) {
							BGAttributes.put("Q_out_Mvar",Float.parseFloat(data[167 + 7 * i].trim()));						}
						
						BGTable.updateFeature( Long.parseLong(ArcGISFID), BGAttributes); // update feature table locally
					}
				}
				
				}
				
			BGTable.applyEdits(null); // commit local updates onto server
			System.out.println("Updating process took "+ String.valueOf(System.currentTimeMillis() - start)+ "ms"); // tells how long it took to update

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
			
		
	public void runPrPowerWorld5(ArrayList<String[]> editStack) {
		
		
		end_time = System.currentTimeMillis();
		
		ArrayList<Map<String, Object>> attributeslist_MG = new ArrayList<Map<String, Object>>(); // additional ArrayList for marinepowergen
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		int counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
		
		for (String key : ArcGISFIDgenmarinetoPWBusNum.keySet()) {
			try {

				QueryParameters qParameter_MGen = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for  predefined data
				qParameter_MGen.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
				qParameter_MGen.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_MGen = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_MGen = null; // create an instance of Feature to store an ArcGIS element

				qTask_MGen = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Marinefarm2/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_MGen = qTask_MGen.execute(qParameter_MGen); // FeatureResult is used to store information from ArcGIS database requested using qParameter_PG and qTask_PG
				graphic_MGen = (Feature) fResult_MGen.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_PG and stores it in graphic_PG; qParameter_PG requests information about a single element only
				attributeslist_MG.add(graphic_MGen.getAttributes()); // append information about the element in graphic_PG to ArrayList attributeslist_LP

				counter++;
				if (counter == 1) {
					System.out.println("done do 1");
					break;
				}
			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err.
			}
		}
		
		System.out.println("success step 1");
		
		writePrPWCSV5(attributeslist_MG);/* attributeslist_LP,attributeslist_PVG,attributeslist_MG, attributeslist_BG,attributeslist_DG*/

		System.out.println("success writing csv");
		
// the following code extract the required input data set from arcgis database
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);
		List<List<Double>> xData = new ArrayList<>(1);				
		List<Double> xRow = new ArrayList<>();
		List<List<Double>> yData;

		String[] ArcGISFID = null;
		ArcGISFID = new String[1];                        // for the simplified parameterised PW model, 6 inputs from 3 of the BusNum are required

		for (int j = 0; j < 1; j++) {
			String BusNum = XPointtoBusNumMG.get(j);          // get the BusNum so that we can look for the ArcGIS FID, then extract the input x-value
			System.out.println(BusNum);
			ArcGISFID[j] = PWBusNumtoArcGISFIDgenmarine.get(BusNum);  // get the ArcGIS FID for the input x-values
		}

		BufferedReader fileReader = null;
		BufferedReader fileReader1 = null;
		
		FileWriter XValue = null;

		try {
			
			XValue = new FileWriter(XVALUE5);
			
			for (int k = 0; k < 1; k++) {
				String line = null;
				fileReader = new BufferedReader(new FileReader(MGenPIN)); // read the pwr_p data from CSV file
				fileReader.readLine();                                  // Read the CSV file header to skip it
				while ((line = fileReader.readLine()) != null) {
					String[] data = line.split(",");
					System.out.println(ArcGISFID[k] + " & " + data[1]);
					if (ArcGISFID[k].equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
						break;
					}
				}
			}

			for (int k = 0; k < 1; k++) {
				String line1 = null;
				fileReader1 = new BufferedReader(new FileReader(MGenVIN)); // read the  pwr_Q data from CSV file
				while ((line1 = fileReader1.readLine()) != null) {
					String[] data = line1.split(",");
					if (ArcGISFID[k].equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k+3 + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue1" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));                            // convert string to double and add to xRow
						break;
					}
				}
			}
			
			
			XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
			XValue.close();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
				fileReader1.close();
				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		// end of extracting the input data set from arcgis database

	xData.add(xRow);                                                                                  // pass all the collected input x-value to xData
				
		System.out.println("xData=" + xData);

		String simDir = Sim5;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {			
			// System.out.println(MoDSAPI.class);
			fileWriter = new FileWriter(PrPWOUTMGenCSV); // filewriter for the output
			
				
			
			System.load("C:/apache-tomcat-8.0.24/lib/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line		
		
//			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);

				for (int j = 0; j < yNames.size(); j++) {
					fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
					fileWriter.append(",");
				}
				
			} catch (Error e) {
		e.printStackTrace();
				
			} catch (IOException e) {
		e.printStackTrace();			
			}

		yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
		System.out.println("Success!");														
		System.out.println("yData=" + yData); 
		
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
		
		end_time = System.currentTimeMillis();
		interval = end_time - start_time;
		System.out.println("The Process Took You: "+ interval + "ms");

		// write the output data to console
	    readPrPWCSV5();
	    
	    

	}/**This method:
     * 1) collects all the input data for the PowerWorld model of the whole Jurong Island;
     * 2) calls python script, run the PowerWorld model, generate output csv file
     * 3) update the output to ArcGIS database (the updating function is still requires debug)*/
	
		
	
	public void writePrPWCSV5(ArrayList<Map<String, Object>> attributeslist_MG)
	
	{

		FileWriter fW_MG_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
		FileWriter fW_MG_V = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

		try {
			
			fW_MG_P = new FileWriter(MGenPIN);
			fW_MG_V = new FileWriter(MGenVIN);

			
			fW_MG_P.append("   ,OBJECTID, P_set_MW, V_set_kV");
			fW_MG_P.append("\n");
			fW_MG_V.append("   ,OBJECTID, V_set_kV");
			fW_MG_V.append("\n");
			
			
			
			//for loadpoint  writing
			for (int i = 0; i < attributeslist_MG.size(); i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
				
				if (i == 1) {				
						break;
					}
					for (String key : attributeslist_MG.get(i).keySet()) { // access all feature fields ("key") of the Load_Point  ("LP") layer
						if (key == "OBJECTID") {
							fW_MG_P.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
							fW_MG_P.append(","); // ZL-151218
							fW_MG_P.append(String.valueOf(attributeslist_MG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_MG_P.append(", ");
							fW_MG_P.append(String.valueOf(attributeslist_MG.get(i).get("P_set_MW"))); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
							fW_MG_P.append(", ");
							fW_MG_P.append(String.valueOf(attributeslist_MG.get(i).get("V_set_kV")));
							fW_MG_P.append("\n");

							fW_MG_V.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
							fW_MG_V.append(","); // ZL-151218
							fW_MG_V.append(String.valueOf(attributeslist_MG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_MG_V.append(", ");
							fW_MG_V.append(String.valueOf(attributeslist_MG.get(i).get("V_set_kV"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
							fW_MG_V.append("\n");
						}
					}
				}

			
			
			fW_MG_P.flush(); // passes the data from fW_PG to PGIN.csv
			fW_MG_V.flush(); // passes the data from fW_PG to PGIN.csv
			
			
			fW_MG_P.close();
			fW_MG_V.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	// update the ArcGIS database according to the output of the PrPW model ZL-20160111
	public void readPrPWCSV5() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrPWOUTMGenCSV));
			fileReader.readLine();     // Read the CSV file header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable MGTable = new GeodatabaseFeatureServiceTable( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Marinefarm2/FeatureServer", user, 0);
			MGTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MGTable.initialize();
			System.out.println("status of Mgen=" + MGTable.getStatus());
			MGTable.getInitializationError();								                                  

			final CountDownLatch latch = new CountDownLatch(1); // ZL-151207 handles one asynchronous processes, only continues Thread when it reaches 0
			MGTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
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
//				System.out.println("data= " + data);
				
//				String[] ArcGISFID = null;
//				ArcGISFID = new String[209]; 
				

				
				for (int j = 0; j < 1; j++) {
					System.out.println(j);
					String PWBusNum = XPointtoBusNumMG.get(j);
					System.out.println(PWBusNum);
					String ArcGISFID = PWBusNumtoArcGISFIDgenmarine.get(PWBusNum);
					
//					String BusNum = XPointtoBusNum.get(j);
//					System.out.println(BusNum);
//					ArcGISFID = PWBusNumtoArcGISFID.get(BusNum);
					System.out.println(ArcGISFID);
										
					if (ArcGISFID != null) {
						Map<String, Object> MGAttributes = MGTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
						if (!data[155 + 14 * j].trim().isEmpty()) {
							MGAttributes .put("theta_act", Float.parseFloat(data[155 + 14 * j] .trim())); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
						}
						if (!data[156 + 14 * j].trim().isEmpty()) { 
							MGAttributes.put("V_act_kV", Float.parseFloat(data[156 + 14 * j].trim()));
						}
						if (!data[159 + 14 * j].trim().isEmpty()) {
							MGAttributes.put("P_out_MW",Float.parseFloat(data[159 + 14 * j].trim()));
						}
						if (!data[160 + 14 * j].trim().isEmpty()) {
							MGAttributes.put("Q_out_Mvar",Float.parseFloat(data[160 + 14 * j].trim()));						}
						
						MGTable.updateFeature( Long.parseLong(ArcGISFID), MGAttributes); // update feature table locally
					}
				}
				
				}
				
			MGTable.applyEdits(null); // commit local updates onto server
			System.out.println("Updating process took "+ String.valueOf(System.currentTimeMillis() - start)+ "ms"); // tells how long it took to update

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
	

public void runPrPowerWorld4(ArrayList<String[]> editStack) {
		
		end_time = System.currentTimeMillis();
		ArrayList<Map<String, Object>> attributeslist_PVG = new ArrayList<Map<String, Object>>(); // additional ArrayList for PVpowergen
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		int counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
		
		for (String key : ArcGISFIDgenpvtoPWBusNum.keySet()) {
			try {

				QueryParameters qParameter_PVG = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for  predefined data
				qParameter_PVG.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
				qParameter_PVG.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_PVG = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_PVG = null; // create an instance of Feature to store an ArcGIS element

				qTask_PVG = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/PVfarm2/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_PVG = qTask_PVG.execute(qParameter_PVG); // FeatureResult is used to store information from ArcGIS database requested using qParameter_PG and qTask_PG
				graphic_PVG = (Feature) fResult_PVG.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_PG and stores it in graphic_PG; qParameter_PG requests information about a single element only
				attributeslist_PVG.add(graphic_PVG.getAttributes()); // append information about the element in graphic_PG to ArrayList attributeslist_LP

				counter++;
				if (counter == 3) {
					System.out.println("done do 3");
					break;
				}
			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err.
			}
		}
		
		System.out.println("success step 1");
		
		writePrPWCSV4(attributeslist_PVG);/* attributeslist_LP,attributeslist_PVG,attributeslist_MG, attributeslist_BG,attributeslist_DG*/

		System.out.println("success writing csv");
		
// the following code extract the required input data set from arcgis database
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);
		List<List<Double>> xData = new ArrayList<>(1);				
		List<Double> xRow = new ArrayList<>();
		List<List<Double>> yData;

		String[] ArcGISFID = null;
		ArcGISFID = new String[3];                        // for the simplified parameterised PW model, 6 inputs from 3 of the BusNum are required

		for (int j = 0; j < 3; j++) {
			String BusNum = XPointtoBusNumPV.get(j);          // get the BusNum so that we can look for the ArcGIS FID, then extract the input x-value
			System.out.println(BusNum);
			ArcGISFID[j] = PWBusNumtoArcGISFIDgenpv.get(BusNum);  // get the ArcGIS FID for the input x-values
		}

		BufferedReader fileReader = null;
		BufferedReader fileReader1 = null;
		
		FileWriter XValue = null;

		try {
			
			XValue = new FileWriter(XVALUE4);
			
			for (int k = 0; k < 3; k++) {
				String line = null;
				fileReader = new BufferedReader(new FileReader(PVPIN)); // read the pwr_p data from CSV file
				fileReader.readLine();                                  // Read the CSV file header to skip it
				while ((line = fileReader.readLine()) != null) {
					String[] data = line.split(",");
					System.out.println(ArcGISFID[k] + " & " + data[1]);
					if (ArcGISFID[k].equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
						break;
					}
				}
			}

			for (int k = 0; k < 3; k++) {
				String line1 = null;
				fileReader1 = new BufferedReader(new FileReader(PVVIN)); // read the  pwr_Q data from CSV file
				while ((line1 = fileReader1.readLine()) != null) {
					String[] data = line1.split(",");
					if (ArcGISFID[k].equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k+3 + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue1" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));                            // convert string to double and add to xRow
						break;
					}
				}
			}
			
			
			XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
			XValue.close();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
				fileReader1.close();
				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		// end of extracting the input data set from arcgis database

	xData.add(xRow);                                                                                  // pass all the collected input x-value to xData
				
		System.out.println("xData=" + xData);

		String simDir = Sim4;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {			
			// System.out.println(MoDSAPI.class);
			fileWriter = new FileWriter(PrPWOUTPVCSV); // filewriter for the
			System.load("C:/apache-tomcat-8.0.24/webapps/ROOT/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line		
			
//			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);

				for (int j = 0; j < yNames.size(); j++) {
					fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
					fileWriter.append(",");
				}
				
			} catch (Error e) {
		e.printStackTrace();
				
			} catch (IOException e) {
		e.printStackTrace();			
			}

		yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
		System.out.println("Success!");														
		System.out.println("yData=" + yData); 
		
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
		
		end_time = System.currentTimeMillis();
		interval = end_time - start_time;
		System.out.println("The Process Took You: "+ interval + "ms");

		// write the output data to console
	    readPrPWCSV4();

	}/**This method:
     * 1) collects all the input data for the PowerWorld model of the whole Jurong Island;
     * 2) calls python script, run the PowerWorld model, generate output csv file
     * 3) update the output to ArcGIS database (the updating function is still requires debug)*/
	
		
	
	public void writePrPWCSV4(ArrayList<Map<String, Object>> attributeslist_PVG)
	
	{

		FileWriter fW_PV_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
		FileWriter fW_PV_V = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

		try {
			
			fW_PV_P = new FileWriter(PVPIN);
			fW_PV_V = new FileWriter(PVVIN);

			
			fW_PV_P.append("   ,OBJECTID, P_set_MW, V_set_kV");
			fW_PV_P.append("\n");
			fW_PV_V.append("   ,OBJECTID, V_set_kV");
			fW_PV_V.append("\n");
			
			
			
			//for loadpoint  writing
			for (int i = 0; i < attributeslist_PVG.size(); i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
				
				if (i == 3) {				
						break;
					}
					for (String key : attributeslist_PVG.get(i).keySet()) { // access all feature fields ("key") of the Load_Point  ("LP") layer
						if (key == "OBJECTID") {
							fW_PV_P.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
							fW_PV_P.append(","); // ZL-151218
							fW_PV_P.append(String.valueOf(attributeslist_PVG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_PV_P.append(", ");
							fW_PV_P.append(String.valueOf(attributeslist_PVG.get(i).get("P_set_MW"))); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
							fW_PV_P.append(", ");
							fW_PV_P.append(String.valueOf(attributeslist_PVG.get(i).get("V_set_kV")));
							fW_PV_P.append("\n");

							fW_PV_V.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
							fW_PV_V.append(","); // ZL-151218
							fW_PV_V.append(String.valueOf(attributeslist_PVG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
							fW_PV_V.append(", ");
							fW_PV_V.append(String.valueOf(attributeslist_PVG.get(i).get("V_set_kV"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
							fW_PV_V.append("\n");
						}
					}
				}

			
			
			fW_PV_P.flush(); // passes the data from fW_PG to PGIN.csv
			fW_PV_V.flush(); // passes the data from fW_PG to PGIN.csv
			
			
			fW_PV_P.close();
			fW_PV_V.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	// update the ArcGIS database according to the output of the PrPW model ZL-20160111
	public void readPrPWCSV4() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrPWOUTPVCSV));
			fileReader.readLine();     // Read the CSV file header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable PVTable = new GeodatabaseFeatureServiceTable( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/PVfarm2/FeatureServer", user, 0);
			PVTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			PVTable.initialize();
			System.out.println("status of PV=" + PVTable.getStatus());
			PVTable.getInitializationError();								                                  

			final CountDownLatch latch = new CountDownLatch(1); // ZL-151207 handles one asynchronous processes, only continues Thread when it reaches 0
			PVTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
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
//				System.out.println("data= " + data);
				
//				String[] ArcGISFID = null;
//				ArcGISFID = new String[209]; 
				

				
				for (int j = 0; j < 3; j++) {
					System.out.println(j);
					String PWBusNum = XPointtoBusNumPV.get(j);
					System.out.println(PWBusNum);
					String ArcGISFID = PWBusNumtoArcGISFIDgenpv.get(PWBusNum);
					
//					String BusNum = XPointtoBusNum.get(j);
//					System.out.println(BusNum);
//					ArcGISFID = PWBusNumtoArcGISFID.get(BusNum);
					System.out.println(ArcGISFID);
										
					if (ArcGISFID != null) {
						Map<String, Object> PVAttributes = PVTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
						if (!data[29 + 14 * j].trim().isEmpty()) {
							PVAttributes .put("theta_act", Float.parseFloat(data[29 + 14 * j] .trim())); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
						}
						if (!data[30 + 14 * j].trim().isEmpty()) { 
							PVAttributes.put("V_act_kV", Float.parseFloat(data[30 + 14 * j].trim()));
						}
						if (!data[33 + 14 * j].trim().isEmpty()) {
							PVAttributes.put("P_out_MW",Float.parseFloat(data[33 + 14 * j].trim()));
						}
						if (!data[34 + 14 * j].trim().isEmpty()) {
							PVAttributes.put("Q_out_Mvar",Float.parseFloat(data[34 + 14 * j].trim()));						}
						
						PVTable.updateFeature( Long.parseLong(ArcGISFID), PVAttributes); // update feature table locally
					}
				}
				
				}
				
			PVTable.applyEdits(null); // commit local updates onto server
			System.out.println("Updating process took "+ String.valueOf(System.currentTimeMillis() - start)+ "ms"); // tells how long it took to update

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
			
	
public void runPrPowerWorld3(ArrayList<String[]> editStack) {
		
		end_time = System.currentTimeMillis();
		
		ArrayList<Map<String, Object>> attributeslist_WG = new ArrayList<Map<String, Object>>(); // additional ArrayList for windpowergen
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("semakausimulator", "c4tsemakau");

		int counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
		for (String key : ArcGISFIDgenwindtoPWBusNum.keySet()) {
			try {

				QueryParameters qParameter_WG = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for  predefined data
				qParameter_WG.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
				qParameter_WG.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_WG = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_WG = null; // create an instance of Feature to store an ArcGIS element

				qTask_WG = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/ArcGIS/rest/services/windturbine2/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_WG = qTask_WG.execute(qParameter_WG); // FeatureResult is used to store information from ArcGIS database requested using qParameter_PG and qTask_PG
				graphic_WG = (Feature) fResult_WG.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_PG and stores it in graphic_PG; qParameter_PG requests information about a single element only
				attributeslist_WG.add(graphic_WG.getAttributes()); // append information about the element in graphic_PG to ArrayList attributeslist_LP

				counter++;
				if (counter == 4) {
					break;
				}
			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err.
			}
		}
		
		
		System.out.println("success step 1");
		
		writePrPWCSV3(attributeslist_WG);/* attributeslist_LP,attributeslist_PVG,attributeslist_MG, attributeslist_BG,attributeslist_DG*/

		System.out.println("success writing csv");
		
// the following code extract the required input data set from arcgis database
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);
		List<List<Double>> xData = new ArrayList<>(1);				
		List<Double> xRow = new ArrayList<>();
		List<List<Double>> yData;

		String[] ArcGISFID = null;
		ArcGISFID = new String[13];                        // for the simplified parameterised PW model, 6 inputs from 3 of the BusNum are required

		for (int j = 0; j < 4; j++) {
			String BusNum = XPointtoBusNumWT.get(j);          // get the BusNum so that we can look for the ArcGIS FID, then extract the input x-value
			System.out.println(BusNum);
			ArcGISFID[j] = PWBusNumtoArcGISFIDgenwind.get(BusNum);  // get the ArcGIS FID for the input x-values
		}

		BufferedReader fileReader = null;
		BufferedReader fileReader1 = null;
		
		FileWriter XValue = null;

		try {
					
			XValue = new FileWriter(XVALUE3);
			
			for (int k = 0; k < 4; k++) {
				String line = null;
				fileReader = new BufferedReader(new FileReader(WPGPIN)); // read the pwr_p data from CSV file
				fileReader.readLine();                                  // Read the CSV file header to skip it
				while ((line = fileReader.readLine()) != null) {
					String[] data = line.split(",");
					System.out.println(ArcGISFID[k] + " & " + data[1]);
					if (ArcGISFID[k].equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
						break;
					}
				}
			}

			for (int k = 0; k < 4; k++) {
				String line1 = null;
				fileReader1 = new BufferedReader(new FileReader(WPGVIN)); // read the  pwr_Q data from CSV file
				while ((line1 = fileReader1.readLine()) != null) {
					String[] data = line1.split(",");
					if (ArcGISFID[k].equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
						XValue.append("X" + k+4 + "=" + data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue1" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));                            // convert string to double and add to xRow
						break;
					}
				}
			}
			// to get the data for the powerGen
		
			
			XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
			XValue.close();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
				fileReader1.close();
				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		// end of extracting the input data set from arcgis database

	xData.add(xRow);                                                                                  // pass all the collected input x-value to xData
				
		System.out.println("xData=" + xData);

		String simDir = Sim3;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {			
			// System.out.println(MoDSAPI.class);
			fileWriter = new FileWriter(PrPWOUT2CSV); // filewriter for the
			System.load("C:/apache-tomcat-8.0.24/webapps/WTlib/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line		
			
//			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);

				for (int j = 0; j < yNames.size(); j++) {
					fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
					fileWriter.append(",");
				}
				
			} catch (Error e) {
		e.printStackTrace();
				
			} catch (IOException e) {
		e.printStackTrace();			
			}

		yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
		System.out.println("Success!");														
		System.out.println("yData=" + yData); 
		
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
		
		end_time = System.currentTimeMillis();
		interval = end_time - start_time;
		System.out.println("The Process Took You: "+ interval + "ms");

		// write the output data to console
	    readPrPWCSV3();

	}

public void writePrPWCSV3(ArrayList<Map<String, Object>> attributeslist_WG)

{

	FileWriter fW_WG_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from loadpoint layer.
	FileWriter fW_WG_V = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from loadpoint layer.
	
	try {
		fW_WG_P = new FileWriter(WPGPIN);
		fW_WG_V = new FileWriter(WPGVIN);
		

		fW_WG_P.append("   ,OBJECTID, P_set_MW, V_set_kV");
		fW_WG_P.append("\n");
		fW_WG_V.append("   ,OBJECTID, V_set_kV");
		fW_WG_V.append("\n");
		
		
		//for wind turbine writing
		for (int i = 0; i < attributeslist_WG.size(); i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
	
		if (i == 4) {				
				break;
			}
			for (String key : attributeslist_WG.get(i).keySet()) { // access all feature fields ("key") of the Load_Point  ("LP") layer
				if (key == "OBJECTID") {
					fW_WG_P.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
					fW_WG_P.append(","); // ZL-151218
					fW_WG_P.append(String.valueOf(attributeslist_WG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
					fW_WG_P.append(", ");
					fW_WG_P.append(String.valueOf(attributeslist_WG.get(i).get("P_set_MW"))); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
					fW_WG_P.append(", ");
					fW_WG_P.append(String.valueOf(attributeslist_WG.get(i).get("V_set_kV")));
					fW_WG_P.append("\n");

					fW_WG_V.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
					fW_WG_V.append(","); // ZL-151218
					fW_WG_V.append(String.valueOf(attributeslist_WG.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
					fW_WG_V.append(", ");
					fW_WG_V.append(String.valueOf(attributeslist_WG.get(i).get("V_set_kV"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
					fW_WG_V.append("\n");
				}
			}
		}
		
		
		
		fW_WG_P.flush(); // passes the data from fW_LP_P to LPPIN.csv
		fW_WG_V.flush(); // passes the data from fW_LP_Q to LPQIN.csv
		
		
		fW_WG_P.close();
		fW_WG_V.close();
		
	} catch (Exception e) {
		e.printStackTrace();
	}
}

// update the ArcGIS database according to the output of the PrPW model ZL-20160111
public void readPrPWCSV3() {
	BufferedReader fileReader = null;
	UserCredentials user = new UserCredentials();
	user.setUserAccount("semakausimulator", "c4tsemakau");

	try {
		long start = System.currentTimeMillis(); // start a timer
		String line = null;
		fileReader = new BufferedReader(new FileReader(PrPWOUT2CSV));
		fileReader.readLine();     // Read the CSV file header to skip it
		QueryParameters loadAllFeatures = new QueryParameters();
		loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
		
		GeodatabaseFeatureServiceTable windturbineTable = new GeodatabaseFeatureServiceTable( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/windturbine2/FeatureServer", user, 0);
		windturbineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
		windturbineTable.initialize();
		System.out.println("status of wind turbine=" + windturbineTable.getStatus());
		windturbineTable.getInitializationError();								                                  

		final CountDownLatch latch = new CountDownLatch(1); // ZL-151207 handles one asynchronous processes, only continues Thread when it reaches 0
		windturbineTable.populateFromService(loadAllFeatures, false,
				new CallbackListener<Boolean>() {
					@Override
					public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
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
//			System.out.println("data= " + data);
			
//			String[] ArcGISFID = null;
//			ArcGISFID = new String[209]; 
			

			
			for (int j = 0; j < 4; j++) {
				System.out.println(j);
				String PWBusNum = XPointtoBusNumWT.get(j);
				System.out.println(PWBusNum);
				String ArcGISFID = PWBusNumtoArcGISFIDgenwind.get(PWBusNum);
				
//				String BusNum = XPointtoBusNum.get(j);
//				System.out.println(BusNum);
//				ArcGISFID = PWBusNumtoArcGISFID.get(BusNum);
				System.out.println(ArcGISFID);
				
				/*int i;
				if (j==1){
				 i=j;
				}
				else
				{ i=2*j;
				
				}*/
				
				if (ArcGISFID != null) {
					Map<String, Object> WindTurbineAttributes = windturbineTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
					if (!data[1 + 5 * j].trim().isEmpty()) {
						WindTurbineAttributes .put("theta_act", Float.parseFloat(data[1 + 5 * j] .trim())); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
					}
					if (!data[2 + 5 * j].trim().isEmpty()) { 
						WindTurbineAttributes.put("V_act_kV", Float.parseFloat(data[2 + 5 * j].trim()));
					}
					if (!data[3 + 5 * j].trim().isEmpty()) {
						WindTurbineAttributes.put("P_out_MW",Float.parseFloat(data[3 + 5 * j].trim()));
					}
					if (!data[4 + 5 * j].trim().isEmpty()) {
						WindTurbineAttributes.put("Q_out_MVar",Float.parseFloat(data[4 + 5 * j].trim()));						}
					
					windturbineTable.updateFeature( Long.parseLong(ArcGISFID), WindTurbineAttributes); // update feature table locally
				}
			}
			
			}
			
		windturbineTable.applyEdits(null); // commit local updates onto server
		System.out.println("Updating process took "+ String.valueOf(System.currentTimeMillis() - start)+ "ms"); // tells how long it took to update

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


public void runPrPowerWorld2(ArrayList<String[]> editStack) {
	
	
	end_time = System.currentTimeMillis();
	ArrayList<Map<String, Object>> attributeslist_LP = new ArrayList<Map<String, Object>>(); // additional ArrayList for loadpoints
	UserCredentials user = new UserCredentials();
	user.setUserAccount("semakausimulator", "c4tsemakau");

	int counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
	for (String key : ArcGISFIDloadtoPWBusNum.keySet()) {
		try {				
			String[] temp = new String[12];	
		    temp[counter]= key; 
		   				
			QueryParameters qParameter_LP = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
			qParameter_LP.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
			qParameter_LP.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
			QueryTask qTask_LP = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
			Feature graphic_LP = null; // create an instance of Feature to store an ArcGIS element
			
//			System.out.println("We are here");
							
			qTask_LP = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint2/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
			FeatureResult fResult_LP = qTask_LP.execute(qParameter_LP); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and qTask_LP
			graphic_LP = (Feature) fResult_LP.iterator().next(); // queryResult.iterator() iterates over the elements  in fResult_LP and stores it in  graphic_LP; qParameter_LP  requests information about a single element only
			attributeslist_LP.add(graphic_LP.getAttributes());  // append information about the element in graphic_LP to ArrayList attributeslist_LP
			
//			System.out.println("Loading No."+counter+ " The key is: "+key );				
//			System.out.println("temp["+counter+"] is "+ temp[counter]);
			
			counter++;

            if (counter == 12) {		
				System.out.print("Done loading 12");
				break;
			}
		} catch (Exception e) {
			e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
		}
	}

	
	System.out.println("success step 1");
	
	writePrPWCSV2(attributeslist_LP);/* attributeslist_LP,attributeslist_PVG,attributeslist_MG, attributeslist_BG,attributeslist_DG*/

	System.out.println("success writing csv");
	
//the following code extract the required input data set from arcgis database
//	ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);
	List<List<Double>> xData = new ArrayList<>(1);				
	List<Double> xRow = new ArrayList<>();
	List<List<Double>> yData;

	String[] ArcGISFID = null;
	ArcGISFID = new String[13];                        // for the simplified parameterised PW model, 6 inputs from 3 of the BusNum are required

	for (int j = 0; j < 12; j++) {
		String BusNum = XPointtoBusNumLP.get(j);          // get the BusNum so that we can look for the ArcGIS FID, then extract the input x-value
		System.out.println(BusNum);
		ArcGISFID[j] = PWBusNumtoArcGISFIDload.get(BusNum);  // get the ArcGIS FID for the input x-values
	}

	BufferedReader fileReader = null;
	BufferedReader fileReader1 = null;
	
	FileWriter XValue = null;

	try {
		
		XValue = new FileWriter(XVALUE2);
		
		for (int k = 0; k < 12; k++) {
			String line = null;
			fileReader = new BufferedReader(new FileReader(LPPIN)); // read the pwr_p data from CSV file
			fileReader.readLine();                                  // Read the CSV file header to skip it
			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				System.out.println(ArcGISFID[k] + " & " + data[1]);
				if (ArcGISFID[k].equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
					XValue.append("X" + k + "=" + data[2].trim());
					XValue.append("\n");
					System.out.println("Xvalue" + k + "=" + data[2]);
					xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
					break;
				}
			}
		}

		for (int k = 0; k < 12; k++) {
			String line1 = null;
			fileReader1 = new BufferedReader(new FileReader(LPQIN)); // read the  pwr_Q data from CSV file
			while ((line1 = fileReader1.readLine()) != null) {
				String[] data = line1.split(",");
				if (ArcGISFID[k].equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
					XValue.append("X" + k+12 + "=" + data[2].trim());
					XValue.append("\n");
					System.out.println("Xvalue1" + k + "=" + data[2]);
					xRow.add(Double.parseDouble(data[2].trim()));                            // convert string to double and add to xRow
					break;
				}
			}
		}
		
		
		XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
		XValue.close();

	} catch (Exception e) {
		e.printStackTrace();
	} finally {
		try {
			fileReader.close();
			fileReader1.close();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	// end of extracting the input data set from arcgis database

xData.add(xRow);                                                                                  // pass all the collected input x-value to xData
			
	System.out.println("xData=" + xData);

	String simDir = Sim2;                                                                          // pass the directory of the power world sorrogate model to simDir
	String modelName = "HDMR_Alg_1";
	FileWriter fileWriter = null;
	try {			
		// System.out.println(MoDSAPI.class);
		fileWriter = new FileWriter(PrPWOUTLPCSV); // filewriter for the
		System.load("C:/apache-tomcat-8.0.24/webapps/LPlib/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line		
	

		ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);

			for (int j = 0; j < yNames.size(); j++) {
				fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
				fileWriter.append(",");
			}
			
		} catch (Error e) {
	e.printStackTrace();
			
		} catch (IOException e) {
	e.printStackTrace();			
		}

	yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
	System.out.println("Success!");														
	System.out.println("yData=" + yData); 
	
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
	
	end_time = System.currentTimeMillis();
	interval = end_time - start_time;
	System.out.println("The Process Took You: "+ interval + "ms");

	// write the output data to console
    readPrPWCSV2();
}

public void writePrPWCSV2(ArrayList<Map<String, Object>> attributeslist_LP)

{

	FileWriter fW_LP_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
	FileWriter fW_LP_Q = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

	try {
		
		fW_LP_P = new FileWriter(LPPIN);
		fW_LP_Q = new FileWriter(LPQIN);

		
		fW_LP_P.append("   ,OBJECTID, P_set_MW, Q_set_MVar");
		fW_LP_P.append("\n");
		fW_LP_Q.append("   ,OBJECTID, Q_set_MVar");
		fW_LP_Q.append("\n");
		
		
		
		//for loadpoint  writing
		for (int i = 0; i < attributeslist_LP.size(); i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
			
			if (i == 12) {				
					break;
				}
				for (String key : attributeslist_LP.get(i).keySet()) { // access all feature fields ("key") of the Load_Point  ("LP") layer
					if (key == "OBJECTID") {
						fW_LP_P.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
						fW_LP_P.append(","); // ZL-151218
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
						fW_LP_P.append(", ");
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get("P_set_MW"))); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
						fW_LP_P.append(", ");
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get("Q_set_MVar")));
						fW_LP_P.append("\n");

						fW_LP_Q.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
						fW_LP_Q.append(","); // ZL-151218
						fW_LP_Q.append(String.valueOf(attributeslist_LP.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
						fW_LP_Q.append(", ");
						fW_LP_Q.append(String.valueOf(attributeslist_LP.get(i).get("Q_set_MVar"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
						fW_LP_Q.append("\n");
					}
				}
			}

		
		
		fW_LP_P.flush(); // passes the data from fW_PG to PGIN.csv
		fW_LP_Q.flush(); // passes the data from fW_PG to PGIN.csv
		
		
		fW_LP_P.close();
		fW_LP_Q.close();
	} catch (Exception e) {
		e.printStackTrace();
	}
}

// update the ArcGIS database according to the output of the PrPW model ZL-20160111
//@SuppressWarnings("deprecation")
public void readPrPWCSV2() {
	BufferedReader fileReader = null;
	UserCredentials user = new UserCredentials();
	user.setUserAccount("semakausimulator", "c4tsemakau");

	try {
		long start = System.currentTimeMillis(); // start a timer
		String line = null;
		fileReader = new BufferedReader(new FileReader(PrPWOUTLPCSV));
		fileReader.readLine();     // Read the CSV file header to skip it
		QueryParameters loadAllFeatures = new QueryParameters();
		loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
		
		GeodatabaseFeatureServiceTable LoadPointTable = new GeodatabaseFeatureServiceTable( "http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint2/FeatureServer", user, 0);
		LoadPointTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
		LoadPointTable.initialize();
		System.out.println("status of loadpoints=" + LoadPointTable.getStatus());
		LoadPointTable.getInitializationError();								                                  

		final CountDownLatch latch = new CountDownLatch(1); // ZL-151207 handles one asynchronous processes, only continues Thread when it reaches 0
		LoadPointTable.populateFromService(loadAllFeatures, false,
				new CallbackListener<Boolean>() {
					@Override
					public void onCallback(Boolean status) { // Asynchronous callback: code must wait for populate from service to finish loading features
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

			
			for (int j = 0; j < 12; j++) {
				System.out.println(j);
				String PWBusNum = XPointtoBusNumLP.get(j);
				System.out.println(PWBusNum);
				String ArcGISFID = PWBusNumtoArcGISFIDload.get(PWBusNum);
				
				System.out.println(ArcGISFID);
				
				int i;
				if (j==0){
				 i=j;
				}
				else
				{ i=10+j;
				
				}
				
				if (ArcGISFID != null) {
					Map<String, Object> LoadPointAttributes = LoadPointTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
					if (!data[1 + 7 * i].trim().isEmpty()) {
						LoadPointAttributes .put("theta_act", Float.parseFloat(data[1 + 7 * i] .trim())); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
					}
					if (!data[2 + 7 * i].trim().isEmpty()) { 
						LoadPointAttributes.put("V_act_kV", Float.parseFloat(data[2 + 7 * i].trim()));
					}
					if (!data[3 + 7 * i].trim().isEmpty()) {
						LoadPointAttributes.put("Pwr_P_MW",Float.parseFloat(data[3 + 7 * i].trim()));
					}
					if (!data[4 + 7 * i].trim().isEmpty()) {
						LoadPointAttributes.put("Pwr_Q_MVar",Float.parseFloat(data[4 + 7 * i].trim()));						}
					
					LoadPointTable.updateFeature( Long.parseLong(ArcGISFID), LoadPointAttributes); // update feature table locally
				}
			}
			
			}
			
		LoadPointTable.applyEdits(null); // commit local updates onto server
		System.out.println("Updating process took "+ String.valueOf(System.currentTimeMillis() - start)+ "ms"); // tells how long it took to update

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
		
