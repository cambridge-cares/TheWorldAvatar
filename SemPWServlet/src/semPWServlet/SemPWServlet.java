package semPWServlet;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;


// You need to add a copy of Apache Tomcat's servlet-api.jar from Tomcat's 'lib' folder, then
// add a reference to that JAR to the project's classpath, or
// put a copy in the Eclipse project and add it to the classpath from there. Here is how:
// Right-click the project, click "Properties", choose "Java Build Path", click "Add External JARs",
// find servlet-api.jar in the Apache Tomcat library ("lib") folder and select it, click "OK".
// If you don't do this, the import library commands will be thrown as errors.
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


// You need to add a copy of the ArcGIS_Runtime_Java.jar from the Eclipse folder, that was added when ArcGIS_Runtime library was added as plugin.
// Right-click the project "SemPWServlet", choose "Properties", choose "Java Build Path", click "Add External JARs",
// and click "OK". Search on your PC in which subfolder exactly the ArcGIS Runtime .jar files reside.
// If you don't do this, the import library commands for these com.esri libraries below will be thrown as errors.
import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.EsriSecurityException;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;

public class SemPWServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	public static Map<String, String> ArcGISFIDtoPWBusNum = new HashMap<>();                                           // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFID = new HashMap<>();                                           // reverse mapping BusNum to ArcGIS FID
//	public static Map<String, String> SubstationtoPWBusNum = new HashMap<>();                                          // Maps Substation number to HV and LV bus numbers
//	public static Map<String, String> PWBusNumtoWindTurbine = new HashMap<>();                                         // reverse mapping
 	public static String SMGINCSV = new String("C:/apache-tomcat-8.0.24/webapps/semakausimulator/SMGIN.CSV");                   // specifies where the SMGIN.CSV file is written to and read from.
	public static String SMGBUSCSV = new String("C:/apache-tomcat-8.0.24/webapps/semakausimulator/SMGBUS.CSV");                 // specifies where the SMGBUS.CSV file is written to and read from.
	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/semakausimulator/SemPWrun.pyw"); // ensure that python environment variable is set to python34

    public SemPWServlet() {
        super();
		// Hard Coded ArcGISFIDtoPWBusNum
		ArcGISFIDtoPWBusNum.put( "0","1");    // NEA transfer Hub         AC Load
		ArcGISFIDtoPWBusNum.put( "1","5");    // Microgrid 1 Windturbine  AC generator
		ArcGISFIDtoPWBusNum.put( "2","7");    // Microgrid 2 Windturbine  AC generator
		ArcGISFIDtoPWBusNum.put( "3","9");    // Microgrid 3 Windturbine  AC generator
//		ArcGISFIDtoPWBusNum.put( "4","6");    // Microgrid 1 Solarfarm    DC generator
//		ArcGISFIDtoPWBusNum.put( "5","8");    // Microgrid 2 Solarfarm    DC generator
//		ArcGISFIDtoPWBusNum.put( "6","10");   // Microgrid 3 Solarfarm    DC generator
//		ArcGISFIDtoPWBusNum.put( "6","11");   // Diesel                   AC generator
		ArcGISFIDtoPWBusNum.put( "5","13");   // Desalination Plant 3     AC Load
		ArcGISFIDtoPWBusNum.put( "6","14");   // Fish Hatchery            AC Load
		ArcGISFIDtoPWBusNum.put( "7","15");   // Desalination Plant 2     AC Load
		ArcGISFIDtoPWBusNum.put( "8","16");   // Desalination Plant 1     AC Load
		ArcGISFIDtoPWBusNum.put( "9","17");   // Phase1 (admin buildings) AC Load
		ArcGISFIDtoPWBusNum.put("10","18");   // sub loads1 (microgrid 1 Windturbine AC load)
		ArcGISFIDtoPWBusNum.put("11","19");   // DC  loads1 (microgrid 1 Solarfarm   DC load)
		ArcGISFIDtoPWBusNum.put("12","20");   // sub loads2 (microgrid 2 Windturbine AC load)
		ArcGISFIDtoPWBusNum.put("13","21");   // DC  loads2 (microgrid 2 Solarfarm   DC load)
		ArcGISFIDtoPWBusNum.put("14","22");   // sub loads3 (microgrid 3 Windturbine AC load)
		ArcGISFIDtoPWBusNum.put("15","23");   // DC  loads3 (microgrid 3 Solarfarm   DC load)

		for (Map.Entry<String, String> entry : ArcGISFIDtoPWBusNum.entrySet()) { // reverse mapping
			PWBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
		}


// Each DC/AC transformer is uniquely described by two reference numbers in PowerWorld, one for the ACbus and one for the DCbus.
// The ArcGIS reference is a string ending in either "AC" or "DC". The PowerWorld reference is an integer.

//		WindTurbinetoPWBusNum.put("1", "10");  // Microgrid 1 Windturbine  AC generator
//		WindTurbinetoPWBusNum.put("2", "10");  // Microgrid 2 Windturbine  AC generator
//		WindTurbinetoPWBusNum.put("3", "10");  // Microgrid 3 Windturbine  AC generator


//		for (Map.Entry<String, String> entry : SubstationtoPWBusNum.entrySet()) {
//			PWBusNumtoSubstation.put(entry.getValue(), entry.getKey().substring(3, 4)); // remove all characters from key, only need FID
//		} 
    } // of public SemPWServlet()

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ArrayList<String[]> editStack = new ArrayList<String[]>();                                   // reconstruct editStack from query string received
		String[] layers = request.getParameter("layers").split(",");
		String[] FIDs = request.getParameter("FIDs").split(",");
		for (int i=0; i<layers.length; i++) {
			editStack.add(new String[] {layers[i], FIDs[i]});
		}
		System.out.println("---------------------------------------");
		ServletConfig config = getServletConfig();                                                  // if this is defined outside a function, nullpointerexception is raised (why?)
		ServletContext context = config.getServletContext();                                        // use context.log() to write to localhost.log in Tomcat\logs folder
		context.log("Received POST request from IP Address: " + request.getRemoteAddr());           // log user's IP address
		System.out.println("Received POST request from IP Address: " + request.getRemoteAddr());
		runSemPW(editStack);
	} // of protected void doPost()
	
	// allows manual updating using a browser
	// e.g. entering http://www.jparksimulator.com/SemPWServlet/?layers=Loadpoints&FIDs=10
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ArrayList<String[]> editStack = new ArrayList<String[]>();		
		String[] layers = request.getParameter("layers").split(",");
		String[] FIDs = request.getParameter("FIDs").split(",");
		for (int i=0; i<layers.length; i++) {
			editStack.add(new String[] {layers[i], FIDs[i]});
		}
		System.out.println("---------------------------------------");
		ServletConfig config = getServletConfig();
		ServletContext context = config.getServletContext();
		context.log("Received GET request from IP Address: " + request.getRemoteAddr());		
		System.out.println("Received GET request from IP Address: " + request.getRemoteAddr());
		runSemPW(editStack);
	} // of protected void doGet()
	
	public void runSemPW(ArrayList<String[]> editStack) {
		ArrayList<String[]> skeleton = new ArrayList<String[]>();                                    // initialises an array of strings containing PW fields, ArcGIS fields corresponding to PW fields 
		ArrayList<Map<String, Object>> attributeslist = new ArrayList<Map<String, Object>>();        // initialises an array of PW object type
		ArrayList<String> layers = new ArrayList<String>();
	    UserCredentials user = new UserCredentials();                                                // initialises the variable used in the following line
	    user.setUserAccount("semakausimulator", "c4tsemakau");                                       // provides access credentials to the secure feature layer service using login username and password
		
		for (int i=0; i<editStack.size(); i++) {                                                     // for each feature in editStack, append something to skeleton, attributeslist and layers
			String layer = (String) editStack.get(i)[0];
			String graphicFID = (String) editStack.get(i)[1];
			
			QueryParameters query = new QueryParameters();                                           // initialises "query" variable.
			query.setWhere("FID='" + graphicFID + "'");                                              // find graphic using FID
			query.setOutFields(new String[] {"*"});                                                  // fetch all attributes using *
			QueryTask task = null;
			Feature graphic = null;
			
			// check if feature in editStack is part of power grid
			if (layer.equals("Loadpoint")                                     // implement
					||layer.equals("EnergyStorage")                           //
					||layer.equals("LoadTLine")                               //
					||layer.equals("MarinePowerGen")                          //
					||layer.equals("MarineTLine")                             //
					||layer.equals("Solarfarm")                               // 
					||layer.equals("SolarInverter")                           //
					||layer.equals("SolarPowerTLine")                         //
					||layer.equals("Windfarm")                                //
					||layer.equals("WindPowerTLine")                          //
					||layer.equals("WindTransformer")                         // 
					||layer.equals("WindTurbine")) {                          // implement
				// consider all possible cases in power grid
				// PW variable names can be found in "Case Object Fields.xslx"
//  1
				if (layer.equals("Loadpoint")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to Loadpoint (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  2
				} else if (layer.equals("EnergyStorage")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to EnergyStorage (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/EnergyStorage/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  3
				} else if (layer.equals("LoadTLine")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to LoadTLine (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/LoadTLine/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  4
				} else if (layer.equals("MarinePowerGen")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to MarinePowerGen (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/MarinePowerGen/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  5
				} else if (layer.equals("MarineTLine")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to MarineTLine (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/MarineTLine/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  6
				} else if (layer.equals("Solarfarm")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to Solarfarm (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Solarfarm/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  7
				} else if (layer.equals("SolarInverter")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to SolarInverter (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/SolarInverter/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  8
				} else if (layer.equals("SolarPowerTLine")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to SolarPowerTLine (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/SolarPowerTLine/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
//  9
				} else if (layer.equals("Windfarm")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to Windfarm (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Windfarm/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
// 10
				} else if (layer.equals("WindPowerTLine")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to WindPowerTLine (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/WindPowerTLine/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
// 11
				} else if (layer.equals("WindTransformer")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to WindTransformer (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/WindTransformer/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
// 12
				} else if (layer.equals("WindTurbine")) {
					skeleton.add(new String[]{"BusNum,BusNomVolt","FID,volt_nom","Bus"});                        // variable names specific to WindTurbine (e.g. BusGenMW=PwrGenMW, BusGenMVR=PwrGenMVR)
					skeleton.add(new String[]{"BusNum,GenID,BusGenMW,BusGenMVR","FID,BusGenID,pwr_P,pwr_Q","Load"});  // can only modify MW and MVR at load, not bus
					try {
						task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/WindTurbine/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}					
				}
				//
				// Expand here if you want to add more layers.
				//
				try {
					FeatureResult queryResult = task.execute(query);
					graphic = (Feature) queryResult.iterator().next();      // queryResult.iterator() should only return one feature
				} catch (Exception e) {
					e.printStackTrace();
				}
				attributeslist.add(graphic.getAttributes());                // map of attributes
				attributeslist.add(graphic.getAttributes());                // each feature has to update two elements in PowerWorld, hence duplicate
				layers.add(layer);                                          // layer reference
				layers.add(layer);
			} // of if (layer.equals("Loadpoint")||...
		} // of for (int i=0; i<editStack.size(); i++)

		// run PowerWorld only if editStack contains power grid features
		if (!skeleton.isEmpty()) {
			writeCSV(skeleton, attributeslist, layers);
			runPyScript();
			readCSV();
		}
	} // of public void runSemPW(ArrayList<String[]> editStack)

		
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
						if (ArcGISfields[j].equals("FID")) {                                    // ArcGIS element is FID
							String ArcGISFID = String.valueOf(attributes.get("FID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDtoPWBusNum.get(ArcGISFID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("LoadID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
//  2
/*					} else if (layer.equals("EnergyStorage")) {                                 // specific to "EnergyStorage"
						if (ArcGISfields[j].equals("FID")) {                                    // ArcGIS element is FID
							String ArcGISFID = String.valueOf(attributes.get("FID"));           // String.valueOf() converts any data type (in this case an integer) to string
							fileWriter.append(ArcGISFIDtoPWBusNum.get(ArcGISFID));              // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("LoadID")) {
							fileWriter.append("1");                                             // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // float values from ArcGIS converted to string
						}
*/
//  3
/*					} else if (layer.equals("LoadTLine")) {
						if (ArcGISfields[j].equals("FID")) {
							int BusNum = (int) attributes.get("FID") + 4;                       // special case where PowerWorld bus number is ArcGIS FID + 4
							fileWriter.append(String.valueOf(BusNum));
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // GenID is 1 by default in REIDSSemGrid.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // GenMW
						}
*/
//  4
/*					} else if (layer.equals("MarinePowerGen")) {
						String substationID = "UHT" + String.valueOf(attributes.get("FID"));
						String HVNomVolt = SubstationtoPWBusNum.get(substationID+"HV");
						String LVNomVolt = SubstationtoPWBusNum.get(substationID+"LV");
						if (ArcGISfields[j].equals("FID")) {
							if (ArcGISfields[j+1].equals("HV_kV")) {                            // check if current entry is looking for HV or LV bus
								fileWriter.append(HVNomVolt);
							} else {
								fileWriter.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")||ArcGISfields[j].equals("LV_kV")) { // input value from ArcGIS
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j])));
						}
*/
//  5
/*
					} else if (layer.equals("MarineTLine")) {                                   // essentially same as UHT substation
						String substationID = "EHT" + String.valueOf(attributes.get("FID"));
						String HVNomVolt = SubstationtoPWBusNum.get(substationID+"HV");
						String LVNomVolt = SubstationtoPWBusNum.get(substationID+"LV");
						if (ArcGISfields[j].equals("FID")) {
							if (ArcGISfields[j+1].equals("HV_kV")) {
								fileWriter.append(HVNomVolt);
							} else {
								fileWriter.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")||ArcGISfields[j].equals("LV_kV")) {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j])));
						}
*/
//  6
/*
					} else if (layer.equals("Solarfarm")) {
						if (ArcGISfields[j].equals("FID")) {
							int BusNum = (int) attributes.get("FID") + 4;                       // special case where PowerWorld bus number is ArcGIS FID + 4
							fileWriter.append(String.valueOf(BusNum));
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // GenID is 1 by default in jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // GenMW
						}
*/
//  7
/*
					} else if (layer.equals("SolarInverter")) {
						String substationID = "UHT" + String.valueOf(attributes.get("FID"));
						String HVNomVolt = SubstationtoPWBusNum.get(substationID+"HV");
						String LVNomVolt = SubstationtoPWBusNum.get(substationID+"LV");
						if (ArcGISfields[j].equals("FID")) {
							if (ArcGISfields[j+1].equals("HV_kV")) {                            // check if current entry is looking for HV or LV bus
								fileWriter.append(HVNomVolt);
							} else {
								fileWriter.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")||ArcGISfields[j].equals("LV_kV")) { // input value from ArcGIS
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j])));
						}
*/
//  8
/*
					} else if (layer.equals("SolarPowerTLine")) {                               // essentially same as UHT substation
						String substationID = "EHT" + String.valueOf(attributes.get("FID"));
						String HVNomVolt = SubstationtoPWBusNum.get(substationID+"HV");
						String LVNomVolt = SubstationtoPWBusNum.get(substationID+"LV");
						if (ArcGISfields[j].equals("FID")) {
							if (ArcGISfields[j+1].equals("HV_kV")) {
								fileWriter.append(HVNomVolt);
							} else {
								fileWriter.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")||ArcGISfields[j].equals("LV_kV")) {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j])));
						}
*/
//  9
/*
					} else if (layer.equals("Windfarm")) {
						if (ArcGISfields[j].equals("FID")) {
							int BusNum = (int) attributes.get("FID") + 4;                       // special case where PowerWorld bus number is ArcGIS FID + 4
							fileWriter.append(String.valueOf(BusNum));
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // GenID is 1 by default in jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // GenMW
						}
*/
// 10
/*
					} else if (layer.equals("WindPowerTLine")) {                                // essentially same as UHT substation
						String substationID = "EHT" + String.valueOf(attributes.get("FID"));
						String HVNomVolt = SubstationtoPWBusNum.get(substationID+"HV");
						String LVNomVolt = SubstationtoPWBusNum.get(substationID+"LV");
						if (ArcGISfields[j].equals("FID")) {
							if (ArcGISfields[j+1].equals("HV_kV")) {
								fileWriter.append(HVNomVolt);
							} else {
								fileWriter.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")||ArcGISfields[j].equals("LV_kV")) {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j])));
						}
*/
// 11
/*
						} else if (layer.equals("WindTransformer")) {                               // essentially same as UHT substation
							String substationID = "EHT" + String.valueOf(attributes.get("FID"));
							String HVNomVolt = SubstationtoPWBusNum.get(substationID+"HV");
							String LVNomVolt = SubstationtoPWBusNum.get(substationID+"LV");
							if (ArcGISfields[j].equals("FID")) {
								if (ArcGISfields[j+1].equals("HV_kV")) {
									fileWriter.append(HVNomVolt);
								} else {
									fileWriter.append(LVNomVolt);
								}
							} else if (ArcGISfields[j].equals("HV_kV")||ArcGISfields[j].equals("LV_kV")) {
								fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j])));
							}
*/
//  12
					} else if (layer.equals("WindTurbine")) {
						if (ArcGISfields[j].equals("FID")) {
							int BusNum = (int) attributes.get("FID") + 4;                       // special case where PowerWorld bus number is ArcGIS FID + 4
							fileWriter.append(String.valueOf(BusNum));
						} else if (ArcGISfields[j].equals("GenID")) {
							fileWriter.append("1");                                             // GenID is 1 by default in jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // GenMW
						}
					} 
					//
					// CONTINUE ELSE IF FOR OTHER LAYERS
					//
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
	} // of public void writeCSV()
	
	public void runPyScript() {
		try {
			Process p = Runtime.getRuntime().exec(runPythonCommand);
			p.waitFor();
			System.out.println("Exit Value (0 means success): " + p.exitValue()); // if console prints 0 it means success

			Calendar cal = new GregorianCalendar();
			String hr; String min; String sec;
			if (cal.get(Calendar.HOUR_OF_DAY)<10) {
				hr = "0"+String.valueOf(cal.get(Calendar.HOUR_OF_DAY));
			} else {
				hr = String.valueOf(cal.get(Calendar.HOUR_OF_DAY));
			}
			if (cal.get(Calendar.MINUTE)<10) {
				min = "0"+String.valueOf(cal.get(Calendar.MINUTE));
			} else {
				min = String.valueOf(cal.get(Calendar.MINUTE));
			}
			if (cal.get(Calendar.SECOND)<10) {
				sec = "0"+String.valueOf(cal.get(Calendar.SECOND));
			} else {
				sec = String.valueOf(cal.get(Calendar.SECOND));
			}
			ServletConfig config = getServletConfig();
			ServletContext context = config.getServletContext();
			context.log("Process started on "+cal.get(Calendar.DAY_OF_MONTH)+"/"+(cal.get(Calendar.MONTH)+1)+
					"/"+cal.get(Calendar.YEAR)+" "+hr+":"+min+":"+sec);                              // log the time that user runs PowerWorld
			System.out.println("Process started on "+cal.get(Calendar.DAY_OF_MONTH)+"/"+(cal.get(Calendar.MONTH)+1)+
					"/"+cal.get(Calendar.YEAR)+" "+hr+":"+min+":"+sec);

			BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line;                                                                             // retrieves console from python script
			System.out.println("Python input:");
			while ((line=br.readLine())!=null) {
				context.log(line);
				System.out.println(line);                                                            // print input array from Python (see python code for more details)
			}
			line = br.readLine();
		} catch (Exception e) {
			e.printStackTrace();
		}
	} // of public void runPyScript()
	
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
			loadAllFeatures.setWhere("FID IS NOT NULL"); // Load all features using SQL command

			GeodatabaseFeatureServiceTable LoadpointTable       = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint/FeatureServer", user, 0);
			LoadpointTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadpointTable.initialize();
			GeodatabaseFeatureServiceTable EnergyStorageTable   = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/EnergyStorage/FeatureServer", user, 0);
			EnergyStorageTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			EnergyStorageTable.initialize();
			GeodatabaseFeatureServiceTable LoadTLineTable       = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/LoadTLine/FeatureServer", user, 0);
			LoadTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadTLineTable.initialize();
			GeodatabaseFeatureServiceTable MarinePowerGenTable  = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/MarinePowerGen/FeatureServer", user, 0);
			MarinePowerGenTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MarinePowerGenTable.initialize();
			GeodatabaseFeatureServiceTable MarineTLineTable     = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/MarineTLine/FeatureServer", user, 0);
			MarineTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MarineTLineTable.initialize();
			GeodatabaseFeatureServiceTable SolarfarmTable       = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Solarfarm/FeatureServer", user, 0);
			SolarfarmTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			SolarfarmTable.initialize();
			GeodatabaseFeatureServiceTable SolarPowerTLineTable = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/SolarPowerTLine/FeatureServer", user, 0);
			SolarPowerTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			SolarPowerTLineTable.initialize();
			GeodatabaseFeatureServiceTable WindfarmTable        = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Windfarm/FeatureServer", user, 0);
			WindfarmTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			WindfarmTable.initialize();
			GeodatabaseFeatureServiceTable WindPowerTLineTable  = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/WindPowerTLine/FeatureServer", user, 0);
			WindPowerTLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			WindPowerTLineTable.initialize();
			GeodatabaseFeatureServiceTable WindTransformerTable = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/WindTransformer/FeatureServer", user, 0);			
			WindTransformerTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			WindTransformerTable.initialize();
			GeodatabaseFeatureServiceTable WindTurbineTable     = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/WindTurbine/FeatureServer", user, 0);			
			WindTurbineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			WindTurbineTable.initialize();

			//
			// EXPAND CODE HERE
			//
			
			final CountDownLatch latch = new CountDownLatch(2); // handles four asynchronous processes, only continues Thread when it reaches 0
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
				String ArcGISFID = PWBusNumtoArcGISFID.get(data[0].trim());                  // .trim() removes trailing white spaces
				
				if (ArcGISFID != null) {                                                     // if PowerWorld bus number can map to an ArcGIS LoadPoint FID
					Map<String, Object> LoadpointAttributes = LoadpointTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
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
					writeDesiredOutputs("pwr_P,pwr_Q,volt_nom,volt_act,theta_act", new int[]{2,3,4,5,6}, data, LoadpointAttributes);
					LoadpointTable.updateFeature(Long.parseLong(ArcGISFID), LoadpointAttributes);   // update feature table locally
				}

				if (PWBusNum==5 || PWBusNum==7 || PWBusNum==9 ) {                                                                 // WindTurbine generator buses
//					Map<String, Object> WindTurbineAttributes = WindTurbineTable.getFeature((long) (PWBusNum)).getAttributes();   // get FID
					Map<String, Object> WindTurbineAttributes = WindTurbineTable.getFeature((long) (PWBusNum-4)).getAttributes(); // subtract 4 from PWBusNum to get FID
					writeDesiredOutputs("volt_nom,volt_act,theta_act,PwrGenMW,PwrGenMVR", new int[] {4,5,6,7,8}, data, WindTurbineAttributes);
					WindTurbineTable.updateFeature((long) (PWBusNum), WindTurbineAttributes);
				}
/*
				if (PWBusNum==5 || PWBusNum==7 || PWBusNum==9 ) {                                                             // alternative for WindTurbine generator buses
					String LoadpointFID = PWBusNumtoArcGISFID.get(String.valueOf(PWBusNum));
					Map<String, Object> WindTurbineAttributes = WindTurbineTable.getFeature(Long.parseLong(WindTurbineFID)).getAttributes();

					if (!data[4].trim().isEmpty()) {
						WindTurbineAttributes.put("volt_nom", Float.parseFloat(data[4].trim()));          // BUSNOMVOLT
						}
					if (!data[5].trim().isEmpty()) {
						WindTurbineAttributes.put("volt_act", Float.parseFloat(data[5].trim()));          // BUSACTVOLT
						}
					if (!data[6].trim().isEmpty()) {
						WindTurbineAttributes.put("theta_act", Float.parseFloat(data[6].trim()));         // BUSANGLE
						}
					if (!data[7].trim().isEmpty()) {
						WindTurbineAttributes.put("PwrGenMW", Float.parseFloat(data[7].trim()));          // BUSGENMW
						}
					if (!data[8].trim().isEmpty()) {
						WindTurbineAttributes.put("PwrGenMVR", Float.parseFloat(data[8].trim()));         // BUSGENMVR
						}
					WindTurbineTable.updateFeature(Long.parseLong(WindTurbineFID), WindTurbineAttributes);
				}				
*/

/*
				if ((PWBusNum>=2 && PWBusNum<=4)||(PWBusNum>=10 && PWBusNum<=12)) {                 // UHT Substation
					String SubstationFID = PWBusNumtoSubstation.get(String.valueOf(PWBusNum));
					Map<String, Object> UHTSubstationAttributes = UHTSubstationTable.getFeature(Long.parseLong(SubstationFID)).getAttributes();
					// cannot use writeDesiredOutputs() function here because have to consider if bus is HV or LV
					if (!data[4].trim().isEmpty()) {
						if (PWBusNum>=2 && PWBusNum<=4) {                                           // high voltage bus
							UHTSubstationAttributes.put("HV_kV", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
						} else {                                                                    // low voltage bus
							UHTSubstationAttributes.put("LV_kV", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
						}
					}
					if (!data[5].trim().isEmpty()) {
						if (PWBusNum>=2 && PWBusNum<=4) {
							UHTSubstationAttributes.put("HV_kV_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT
						} else {
							UHTSubstationAttributes.put("LV_kV_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT
						}
					}
					if (!data[6].trim().isEmpty()) {
						if (PWBusNum>=2 && PWBusNum<=4) {
							UHTSubstationAttributes.put("HV_theta", Float.parseFloat(data[6].trim()));  // BUSANGLE
						} else {
							UHTSubstationAttributes.put("LV_theta", Float.parseFloat(data[6].trim()));  // BUSANGLE
						}
					}
					UHTSubstationTable.updateFeature(Long.parseLong(SubstationFID), UHTSubstationAttributes);
				}
*/
/*				if (PWBusNum>=13 && PWBusNum<=23) {                                                     // all load points
					String LoadpointFID = PWBusNumtoArcGISFID.get(String.valueOf(PWBusNum));
					Map<String, Object> LoadpointAttributes = LoadpointTable.getFeature(Long.parseLong(LoadpointFID)).getAttributes();
					if (!data[2].trim().isEmpty()) {
						LoadpointAttributes.put("Pwr_P", Float.parseFloat(data[2].trim()));             // BUSLOADMW
						}
					if (!data[3].trim().isEmpty()) {
						LoadpointAttributes.put("Pwr_Q", Float.parseFloat(data[3].trim()));             // BUSLOADMVR
						}
					if (!data[4].trim().isEmpty()) {
						LoadpointAttributes.put("volt_nom", Float.parseFloat(data[4].trim()));          // BUSNOMVOLT
						}
					if (!data[5].trim().isEmpty()) {
						LoadpointAttributes.put("volt_act", Float.parseFloat(data[5].trim()));          // BUSACTVOLT
						}
					if (!data[6].trim().isEmpty()) {
						LoadpointAttributes.put("theta_act", Float.parseFloat(data[6].trim()));         // BUSANGLE
						}
					}
*/
//				LoadpointTable.updateFeature(Long.parseLong(LoadpointFID), LoadpointAttributes);
				}
			} // of while()
				LoadpointTable.applyEdits(null); // commit local updates onto server
				WindTurbineTable.applyEdits(null);
				
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
	
	public void loadTables(GeodatabaseFeatureServiceTable[] listOfTables, final CountDownLatch latch) {
		QueryParameters loadAllFeatures = new QueryParameters();
		loadAllFeatures.setWhere("FID IS NOT NULL");                                             // Load all features using SQL command
		for (GeodatabaseFeatureServiceTable table : listOfTables) {
			table.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			table.initialize();
			table.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) {                                         // Asynchronous callback: code must wait for populate from service to finish loading features
					if (status==true) {
						latch.countDown();                                                       // latch decrement if feature service table is ready
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
		}
	} // of public void loadTables
	
	public void writeDesiredOutputs(String headers, int[] dataIndex, String[] data, Map<String,Object> attributes) {		
		String[] headersList = headers.split(",");                                             // number of headers in headersList must be equal to number of integers in dataIndex
		for (int i=0; i<headersList.length; i++) {
			String value = data[dataIndex[i]].trim();                                          // remove whitespaces
			if (!value.isEmpty()) {                                                            // value is not empty
				attributes.put(headersList[i], Float.parseFloat(value));                       // value must be of type float
			}
		}
	} // of public void writeDesiredOutputs()
}
