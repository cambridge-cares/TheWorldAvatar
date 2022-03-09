package Semakau;

//PWfields and ArcGISfields unfinished (are values all numbers?)
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
import com.esri.core.io.EsriSecurityException;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;
import com.esri.map.ArcGISFeatureLayer;

public class SemPowerWorld {
	
	public static Map<String, String> ArcGISFIDtoPWBusNum = new HashMap<>(); // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFID = new HashMap<>(); // Maps BusNum to ArcGIS FID
	public static String SMGINCSV = new String("C:/apache-tomcat-8.0.28/webapps/jparksimulator/SMGIN.CSV"); // path relative to Eclipse project directory (C:\\users\daryl yong\workspace\jparksim)
	public static String SMGOUTCSV = new String("C:/apache-tomcat-8.0.28/webapps/jparksimulator/SMGOUT.CSV"); // path relative to Eclipse project directory (C:\\users\daryl yong\workspace\jparksim)
	public static String runPythonCommand = new String("C:/python34/pythonw.exe C:/apache-tomcat-8.0.28/webapps/jparksimulator/SemPWrun.pyw"); // set absolute path to pythonw.exe because ProcessBuilder.environment() does not point to python34
//  public long updatingTime;
	
	public SemPowerWorld() {
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
		
		for (Map.Entry<String, String> entry : ArcGISFIDtoPWBusNum.entrySet()) {
			PWBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
		}
	}
	
	//main function
	public void runPowerWorld(ArrayList<Object[]> editStack) {
		ArrayList<String[]> skeleton = new ArrayList<String[]>(); // array of strings containing PW fields, ArcGIS fields corresponding to PW fields and PW object type
		ArrayList<Map<String, Object>> attributeslist = new ArrayList<Map<String, Object>>();
		ArrayList<ArcGISFeatureLayer> layers = new ArrayList<ArcGISFeatureLayer>();
		UserCredentials user = new UserCredentials();
	    user.setUserAccount("semakausimulator", "c4tsemakau");        // Access secure feature layer service using login username and password
		
		
		for (int i=0; i<editStack.size(); i++) { // for each feature in editStack, append something to skeleton, attributeslist and layers
			ArcGISFeatureLayer layer = (ArcGISFeatureLayer) editStack.get(i)[0];
			int graphicFID = (int) editStack.get(i)[1];
			
			QueryParameters query = new QueryParameters();
			query.setWhere("FID='" + String.valueOf(graphicFID) + "'"); // find graphic using FID
			query.setOutFields(new String[] {"*"}); // fetch all attributes using *
			QueryTask task = null;
			Feature graphic = null;
			
			// check if feature in editStack is part of power grid
			if (  layer==Semakau.Buildingslayer      ||layer==Semakau.Desalinationlayer    ||layer==Semakau.EnergyStoragelayer
					||layer==Semakau.FishHatcherylayer   ||layer==Semakau.Loadpointlayer       ||layer==Semakau.LoadTLinelayer
					||layer==Semakau.MarinePowerGenlayer ||layer==Semakau.MarineTLinelayer     ||layer==Semakau.Roadslayer
					||layer==Semakau.Solarfarmlayer      ||layer==Semakau.SolarInverterlayer   ||layer==Semakau.SolarPowerTLinelayer
					||layer==Semakau.Windfarmlayer       ||layer==Semakau.WindPowerTLinelayer  ||layer==Semakau.WindTransformerlayer
					||layer==Semakau.WindTurbinelayer) {                    		      // consider all possible cases in power grid
				// consider all possible cases in power grid
				if (layer == Semakau.Loadpointlayer) { // variable names specific to load points (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"}); // PW variable names can be found in Case Object Fields.xlsx
					task = new QueryTask("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint/FeatureServer/0");
				} // ELSE IF OTHER TYPES OF LAYERS * TO BE CONTINUED
				try {
					FeatureResult queryResult = task.execute(query);
					graphic = (Feature) queryResult.iterator().next();
				} catch (Exception e) {
					e.printStackTrace();
				}
				attributeslist.add(graphic.getAttributes()); // map of attributes
				layers.add(layer); // layer reference
			}
		}
		// run PowerWorld only if editStack contains power grid features
		if (!skeleton.isEmpty()) {
			writeCSV(skeleton, attributeslist, layers);
			runPyScript();
			readCSV();
		}
	}
	
	public void writeCSV(ArrayList<String[]> skeleton, ArrayList<Map<String, Object>> attributeslist, 
			ArrayList<ArcGISFeatureLayer> layers) { //write input file to python
		FileWriter fileWriter = null;
		try {
			fileWriter = new FileWriter(SMGINCSV);
			for (int i=0; i<skeleton.size(); i++) { // for each feature in editStack
				fileWriter.append(skeleton.get(i)[0]); // write headers
				fileWriter.append("\n"); // new line
				String[] ArcGISfields = skeleton.get(i)[1].split(","); // produce iterable list from comma separated string
				Map<String, Object> attributes = attributeslist.get(i);
				ArcGISFeatureLayer layer = layers.get(i);

				for (int j=0; j<ArcGISfields.length-1; j++) { // for each ArcGIS attribute
					if (layer==Semakau.Loadpointlayer) { // specific to load point
						if (ArcGISfields[j].equals("FID")) { // ArcGIS element is FID
							String ArcGISFID = String.valueOf(attributes.get("FID")); // String.valueOf() converts any data type to string
							fileWriter.append(ArcGISFIDtoPWBusNum.get(ArcGISFID)); // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("LoadID")) {
							fileWriter.append("1"); // LoadID defaults to 1 in the jparksimulator.pwb
						} else {
							fileWriter.append(String.valueOf(attributes.get(ArcGISfields[j]))); // int values from ArcGIS converted to string
						}
						fileWriter.append(",");
					} // ELSE IF OTHER TYPES OF LAYERS * TO BE CONTINUED
				}
				String lastArcGISfield = ArcGISfields[ArcGISfields.length-1]; // assuming no special conditions (1 to 1 value mapping from ArcGIS to PW)
				fileWriter.append(String.valueOf(attributes.get(lastArcGISfield))); // last element without comma
				fileWriter.append("\n");
				fileWriter.append(skeleton.get(i)[2]); // PW object type (e.g. BUS, GEN, LOAD)
				fileWriter.append("\n");
				// each item is given three rows: headers (name of attributes), corresponding values and type of object (e.g. BUS, GEN, etc)
			}
		} catch (Exception e) {
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
	
	public void readCSV() { // only updates Load Points for now
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
	    user.setUserAccount("semakausimulator", "c4tsemakau");        // Access secure feature layer service using login username and password
		try {
			long start = System.currentTimeMillis();
			String line = null;
			fileReader = new BufferedReader(new FileReader(SMGOUTCSV));
			fileReader.readLine(); // Read the CSV file header to skip it
			GeodatabaseFeatureServiceTable LoadPointsTable = new GeodatabaseFeatureServiceTable("http://services3.arcgis.com/785KAqvbaBANxwtT/arcgis/rest/services/Loadpoint/FeatureServer", 0);
			LoadPointsTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadPointsTable.initialize();	
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("FID IS NOT NULL"); // Load all features
			CountDownLatch latch = new CountDownLatch(1); // handles asynchronous process, only continues Thread when it reaches 0
			LoadPointsTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
				@Override
				public void onCallback(Boolean status) { // Asynchronous function: must wait for populate from service to finish loading features
					if (status==true) {
						latch.countDown(); // latch decrement to 0: feature service table is ready
					}
				}
				@Override
				public void onError(Throwable e) {
					e.printStackTrace();
				}});
			
			latch.await(); // wait until feature service table is ready
			while ((line=fileReader.readLine())!=null) { // Continue reading lines until none left
				String[] data = line.split(","); // split string by comma
				String ArcGISFID = PWBusNumtoArcGISFID.get(data[0].trim()); // .trim() removes trailing white spaces
				if (ArcGISFID != null) { // if PowerWorld bus number can map to an ArcGISFID
					Map<String, Object> attributes = LoadPointsTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
					if (!data[2].trim().isEmpty()) { // if not an empty string
						attributes.put("pwr_P", Float.parseFloat(data[2].trim())); // BUSLOADMW
					}
					if (!data[3].trim().isEmpty()) {
						attributes.put("volt_nom", Float.parseFloat(data[3].trim())); // BUSPUVOLT
					}	
					if (!data[4].trim().isEmpty()) {
						attributes.put("theta_act", Float.parseFloat(data[4].trim())); // BUSANGLE						
					}
					if (!data[5].trim().isEmpty()) {
						attributes.put("volt_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT						
					}
					LoadPointsTable.updateFeature(Long.parseLong(ArcGISFID), attributes); // update feature locally
				}
			}
			LoadPointsTable.applyEdits(null); // commit local update onto server
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis()-start) + "ms"); // tells you how long it took to update
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
	
	public void runPyScript() {
		try {
			Process p = Runtime.getRuntime().exec(runPythonCommand);
			p.waitFor();
			System.out.println("Exit Value (0 means success): " + p.exitValue()); // 0 means success
			BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line;
			System.out.println("Python input:");
			while ((line=br.readLine())!=null) {
				System.out.println(line); // print input array from Python (output array is too long to be displayed on console, will hang system)
			}
			line = br.readLine();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}