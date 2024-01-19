package jParkSim;

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

public class PowerWorld {
	
	public static Map<String, String> ArcGISFIDtoPWBusNum = new HashMap<>(); // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFID = new HashMap<>(); // Maps BusNum to ArcGIS FID
	public static String INCSV = new String("C:/apache-tomcat-8.0.24/webapps/jparksimulator/IN.CSV");
	public static String OUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/jparksimulator/OUT.CSV");
	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/jparksimulator/PWrun.pyw"); // ensure that python environment variable is set to python34
	
	public PowerWorld() {
		// Hard Coded ArcGISFIDtoPWBusNum
		ArcGISFIDtoPWBusNum.put("61","80"); //Air Liquide
		ArcGISFIDtoPWBusNum.put("52","31"); //Azko Nobel
		ArcGISFIDtoPWBusNum.put("159","175"); //Asahi Kasei Chemicals
		ArcGISFIDtoPWBusNum.put("90","122"); //Asahi Kasei Plastics
		ArcGISFIDtoPWBusNum.put("173","182"); //CCD Singapore
		ArcGISFIDtoPWBusNum.put("78","115"); //Celanese
		ArcGISFIDtoPWBusNum.put("91","127"); //Chemical Industries
		ArcGISFIDtoPWBusNum.put("24","54"); //Chemical Specialties
		ArcGISFIDtoPWBusNum.put("23","53"); //Chemical Specialties
		ArcGISFIDtoPWBusNum.put("85","121"); //Chevron Oronite
		ArcGISFIDtoPWBusNum.put("178","138"); //Ciba Industries
		ArcGISFIDtoPWBusNum.put("8","35"); //Coim Asia Pacific
		ArcGISFIDtoPWBusNum.put("7","34"); //Continental Chemical
		ArcGISFIDtoPWBusNum.put("19","47"); //Croda
		ArcGISFIDtoPWBusNum.put("16","59"); //DENKA
		ArcGISFIDtoPWBusNum.put("44","82"); //DENKA
		ArcGISFIDtoPWBusNum.put("89","124"); //DIC Alkylphenol
		ArcGISFIDtoPWBusNum.put("100","128"); //Du Pont
		ArcGISFIDtoPWBusNum.put("73","116"); //Du Pont
		ArcGISFIDtoPWBusNum.put("74","117"); //Du Pont
		ArcGISFIDtoPWBusNum.put("75","118"); //Du Pont
		ArcGISFIDtoPWBusNum.put("151","192"); //Essar Projects Aromatics
		ArcGISFIDtoPWBusNum.put("106","150"); //ExxonMobil Asia Pacific
		ArcGISFIDtoPWBusNum.put("103","93"); //ExxonMobil Chemical
		ArcGISFIDtoPWBusNum.put("104","134"); //ExxonMobil Chemical
		ArcGISFIDtoPWBusNum.put("102","94"); //ExxonMobil Refinery
		ArcGISFIDtoPWBusNum.put("56","66"); //Faci Asia
		ArcGISFIDtoPWBusNum.put("84","108"); //HDB Building
		ArcGISFIDtoPWBusNum.put("124","153"); //Horizon
		ArcGISFIDtoPWBusNum.put("86","123"); //Horizon Korea
		ArcGISFIDtoPWBusNum.put("21","49"); //Huntsman
		ArcGISFIDtoPWBusNum.put("97","107"); //ITRO
		ArcGISFIDtoPWBusNum.put("118","205"); //Ibris Bio-fuels
		ArcGISFIDtoPWBusNum.put("107","148"); //ICES
		ArcGISFIDtoPWBusNum.put("71","102"); //Invista
		ArcGISFIDtoPWBusNum.put("149","203"); //JI Maintenance Hub
		ArcGISFIDtoPWBusNum.put("114","206"); //Katoen
		ArcGISFIDtoPWBusNum.put("115","207"); //Katoen
		ArcGISFIDtoPWBusNum.put("109","147"); //Keppel Merlimau Cogen
		ArcGISFIDtoPWBusNum.put("72","114"); //Kuraray
		ArcGISFIDtoPWBusNum.put("120","200"); //LTH Logistics
		ArcGISFIDtoPWBusNum.put("156","187"); //Lanxess
		ArcGISFIDtoPWBusNum.put("54","71"); //Linde Gas
		ArcGISFIDtoPWBusNum.put("79","113"); //Lucite
		ArcGISFIDtoPWBusNum.put("94","131"); //Mitsui Chemicals
		ArcGISFIDtoPWBusNum.put("36","86"); //Mitsui Elastomers
		ArcGISFIDtoPWBusNum.put("93","132"); //Mitsui Phenol
		ArcGISFIDtoPWBusNum.put("18","57"); //Nikko Chemicals
		ArcGISFIDtoPWBusNum.put("65","97"); //Oasis Sakra
		ArcGISFIDtoPWBusNum.put("4","38"); //Oiltanking Asia Pacific
		ArcGISFIDtoPWBusNum.put("3","39"); //Oiltanking Asia Pacific
		ArcGISFIDtoPWBusNum.put("50","40"); //Oiltanking Asia Pacific
		ArcGISFIDtoPWBusNum.put("2","41"); //Oiltanking Asia Pacific
		ArcGISFIDtoPWBusNum.put("13","63"); //Oiltanking Odfjell
		ArcGISFIDtoPWBusNum.put("12","64"); //Oiltanking Odfjell
		ArcGISFIDtoPWBusNum.put("11","52"); //Oiltanking Odfjell
		ArcGISFIDtoPWBusNum.put("9","62"); //Oiltanking Odfjell
		ArcGISFIDtoPWBusNum.put("29","75"); //PTC Chemical
		ArcGISFIDtoPWBusNum.put("28","76"); //PTC Chemical
		ArcGISFIDtoPWBusNum.put("27","77"); //PTC Chemical
		ArcGISFIDtoPWBusNum.put("6","32"); //PacificLight Power
		ArcGISFIDtoPWBusNum.put("99","103"); //Perstorp
		ArcGISFIDtoPWBusNum.put("96","98"); //Perstorp or Lonza
		ArcGISFIDtoPWBusNum.put("98","105"); //Perstorp or Lonza
		ArcGISFIDtoPWBusNum.put("45","83"); //Petrochemical Corp
		ArcGISFIDtoPWBusNum.put("64","95"); //Petrofac
		ArcGISFIDtoPWBusNum.put("31","70"); //Rotary Engineering
		ArcGISFIDtoPWBusNum.put("143","193"); //Sembcorp
		ArcGISFIDtoPWBusNum.put("82","111"); //Sembcorp Cogen
		ArcGISFIDtoPWBusNum.put("87","125"); //Sembcorp Gas
		ArcGISFIDtoPWBusNum.put("81","110"); //Sembcorp Industries
		ArcGISFIDtoPWBusNum.put("152","191"); //Sembcorp Integrated
		ArcGISFIDtoPWBusNum.put("83","109"); //Sembcorp
		ArcGISFIDtoPWBusNum.put("14","60"); //Shell Chemicals
		ArcGISFIDtoPWBusNum.put("179","137"); //Si Group
		ArcGISFIDtoPWBusNum.put("101","104"); //Glacial Acrylic
		ArcGISFIDtoPWBusNum.put("68","130"); //Glacial Acrylic
		ArcGISFIDtoPWBusNum.put("132","162"); //LNG Terminal
		ArcGISFIDtoPWBusNum.put("130","163"); //LNG Terminal
		ArcGISFIDtoPWBusNum.put("35","84"); //Singapore Petrochemicals
		ArcGISFIDtoPWBusNum.put("39","88"); //Singapore Petrochemicals
		ArcGISFIDtoPWBusNum.put("46","45"); //Singapore Refining
		ArcGISFIDtoPWBusNum.put("47","46"); //Singapore Refining
		ArcGISFIDtoPWBusNum.put("57","67"); //Stella Chemifa
		ArcGISFIDtoPWBusNum.put("5","36"); //Stepan
		ArcGISFIDtoPWBusNum.put("164","177"); //Stolthaven
		ArcGISFIDtoPWBusNum.put("38","99"); //Sumitomo Chemical
		ArcGISFIDtoPWBusNum.put("66","85"); //Sumitomo Chemical
		ArcGISFIDtoPWBusNum.put("69","100"); //Sumitomo Mitsui
		ArcGISFIDtoPWBusNum.put("88","126"); //Tate and Lyle
		ArcGISFIDtoPWBusNum.put("95","133"); //Teijin Polycarbonate
		ArcGISFIDtoPWBusNum.put("165","178"); //Tembusu Multi Utilities
		ArcGISFIDtoPWBusNum.put("174","180"); //Tembusu Multi Utilities
		ArcGISFIDtoPWBusNum.put("176","184"); //Tembusu Multi Utilities
		ArcGISFIDtoPWBusNum.put("42","89"); //The Polyolefin
		ArcGISFIDtoPWBusNum.put("43","90"); //The Polyolefin
		ArcGISFIDtoPWBusNum.put("70","101"); //Unimatec
		ArcGISFIDtoPWBusNum.put("126","156"); //Universal Terminal
		ArcGISFIDtoPWBusNum.put("116","119"); //Vopak
		ArcGISFIDtoPWBusNum.put("111","120"); //Vopak
		ArcGISFIDtoPWBusNum.put("76","208"); //Vopak
		ArcGISFIDtoPWBusNum.put("113","136"); //Vopak
		ArcGISFIDtoPWBusNum.put("77","197"); //Vopak
		ArcGISFIDtoPWBusNum.put("10","65"); //YTL PowerSeraya
		ArcGISFIDtoPWBusNum.put("148","202"); //Zeon Chemicals
		ArcGISFIDtoPWBusNum.put("67", "106"); //Public Building (00856N)
		// unidentified load points
		ArcGISFIDtoPWBusNum.put("62", "81"); //02451C
		ArcGISFIDtoPWBusNum.put("169", "142"); //01228T
		ArcGISFIDtoPWBusNum.put("48", "44"); //02464L
		ArcGISFIDtoPWBusNum.put("112", "139"); //03158V
		ArcGISFIDtoPWBusNum.put("129", "164"); //U128
		ArcGISFIDtoPWBusNum.put("168", "181"); //01772N
		ArcGISFIDtoPWBusNum.put("92", "129"); //01650A
		ArcGISFIDtoPWBusNum.put("55", "68"); //02450L
		ArcGISFIDtoPWBusNum.put("166", "179"); //03182K
		ArcGISFIDtoPWBusNum.put("34", "74"); //U33
		ArcGISFIDtoPWBusNum.put("119", "201"); //02322T
		ArcGISFIDtoPWBusNum.put("33", "72"); //03153X
		ArcGISFIDtoPWBusNum.put("121", "199"); //U120
		ArcGISFIDtoPWBusNum.put("122", "151"); //U121
		ArcGISFIDtoPWBusNum.put("123", "152"); //02263P
		ArcGISFIDtoPWBusNum.put("32", "69"); //03154L
		ArcGISFIDtoPWBusNum.put("125", "155"); //01873A
		ArcGISFIDtoPWBusNum.put("53", "33"); //02596N
		ArcGISFIDtoPWBusNum.put("127", "158"); //U126
		ArcGISFIDtoPWBusNum.put("30", "51"); //U29
		ArcGISFIDtoPWBusNum.put("80", "112"); //00712M
		ArcGISFIDtoPWBusNum.put("171", "143"); //02319T
		ArcGISFIDtoPWBusNum.put("105", "149"); //03296P
		ArcGISFIDtoPWBusNum.put("170", "144"); //03270A
		ArcGISFIDtoPWBusNum.put("133", "161"); //03325A
		ArcGISFIDtoPWBusNum.put("51", "37"); //02592P
		ArcGISFIDtoPWBusNum.put("161", "174"); //03051L
		ArcGISFIDtoPWBusNum.put("158", "170"); //03240K
		ArcGISFIDtoPWBusNum.put("137", "166"); //03307T
		ArcGISFIDtoPWBusNum.put("157", "171"); //U156
		ArcGISFIDtoPWBusNum.put("155", "188"); //03152N
		ArcGISFIDtoPWBusNum.put("154", "185"); //U153
		ArcGISFIDtoPWBusNum.put("153", "186"); //02878X
		ArcGISFIDtoPWBusNum.put("150", "204"); //01968M
		ArcGISFIDtoPWBusNum.put("108", "146"); //U107
		ArcGISFIDtoPWBusNum.put("145", "190"); //03145P
		ArcGISFIDtoPWBusNum.put("162", "172"); //03233V
		ArcGISFIDtoPWBusNum.put("144", "165"); //03312K
		ArcGISFIDtoPWBusNum.put("147", "198"); //03299K
		ArcGISFIDtoPWBusNum.put("26", "56"); //03279P
		ArcGISFIDtoPWBusNum.put("25", "55"); //U24
		ArcGISFIDtoPWBusNum.put("142", "195"); //03311A
		ArcGISFIDtoPWBusNum.put("22", "50"); //02382V
		ArcGISFIDtoPWBusNum.put("20", "48"); //02380M
		ArcGISFIDtoPWBusNum.put("141", "169"); //03303M
		ArcGISFIDtoPWBusNum.put("140", "194"); //03096L
		ArcGISFIDtoPWBusNum.put("139", "196"); //03338T
		ArcGISFIDtoPWBusNum.put("17", "58"); //03280W
		ArcGISFIDtoPWBusNum.put("138", "168"); //03333C
		ArcGISFIDtoPWBusNum.put("136", "167"); //02817L
		ArcGISFIDtoPWBusNum.put("15", "61"); //03238N
		ArcGISFIDtoPWBusNum.put("160", "176"); //03052C
		ArcGISFIDtoPWBusNum.put("135", "154"); //03119X
		ArcGISFIDtoPWBusNum.put("146", "189"); //03023W
		ArcGISFIDtoPWBusNum.put("163", "173"); //03236A
		ArcGISFIDtoPWBusNum.put("134", "157"); //01972W
		ArcGISFIDtoPWBusNum.put("131", "160"); //02818C
		ArcGISFIDtoPWBusNum.put("117", "209"); //02432X
		ArcGISFIDtoPWBusNum.put("167", "145"); //02948K
		ArcGISFIDtoPWBusNum.put("37", "87"); //03204T
		ArcGISFIDtoPWBusNum.put("110", "135"); //02502K
		ArcGISFIDtoPWBusNum.put("40", "91"); //03186C
		ArcGISFIDtoPWBusNum.put("41", "92"); //03249T
		ArcGISFIDtoPWBusNum.put("172", "141"); //03291L
		ArcGISFIDtoPWBusNum.put("49", "43"); //02954L
		ArcGISFIDtoPWBusNum.put("58", "79"); //02838V
		ArcGISFIDtoPWBusNum.put("177", "183"); //02944V
		ArcGISFIDtoPWBusNum.put("63", "96"); //01384X
		ArcGISFIDtoPWBusNum.put("1", "42"); //03033X
		
		for (Map.Entry<String, String> entry : ArcGISFIDtoPWBusNum.entrySet()) {
			PWBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
		}
	}
	
	//main function
	public void runPowerWorld(ArrayList<Object[]> editStack) throws EsriSecurityException {
		ArrayList<String[]> skeleton = new ArrayList<String[]>(); // array of strings containing PW fields, ArcGIS fields corresponding to PW fields and PW object type
		ArrayList<Map<String, Object>> attributeslist = new ArrayList<Map<String, Object>>();
		ArrayList<ArcGISFeatureLayer> layers = new ArrayList<ArcGISFeatureLayer>();
	    UserCredentials user = new UserCredentials();
	    user.setUserAccount("jparksimulator", "c4tjpark"); // Access secure feature layer service using login username and password
		
		for (int i=0; i<editStack.size(); i++) { // for each feature in editStack, append something to skeleton, attributeslist and layers
			ArcGISFeatureLayer layer = (ArcGISFeatureLayer) editStack.get(i)[0];
			int graphicFID = (int) editStack.get(i)[1];
			
			QueryParameters query = new QueryParameters();
			query.setWhere("FID='" + String.valueOf(graphicFID) + "'"); // find graphic using FID
			query.setOutFields(new String[] {"*"}); // fetch all attributes using *
			QueryTask task = null;
			Feature graphic = null;
			
			// check if feature in editStack is part of power grid
			if (layer==JParkSim.UHTSubstationlayer||layer==JParkSim.UHTLineslayer||layer==JParkSim.EHTSubstationlayer||layer==JParkSim.EHTLineslayer
					||layer==JParkSim.HTLineslayer||layer==JParkSim.PowerGenlayer||layer==JParkSim.LoadPointslayer||layer==JParkSim.BusCouplerlayer) {
				// consider all possible cases in power grid
				if (layer == JParkSim.LoadPointslayer) { // variable names specific to load points (e.g. LoadMW=pwr_P, LoadMVR=pwr_Q)
					skeleton.add(new String[]{"BusNum,LoadID,LoadMW,LoadMVR","FID,LoadID,pwr_P,pwr_Q","Load"}); // PW variable names can be found in Case Object Fields.xlsx
					task = new QueryTask("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/Load_Points/FeatureServer/0", user);
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
			fileWriter = new FileWriter(INCSV);
			for (int i=0; i<skeleton.size(); i++) { // for each feature in editStack
				fileWriter.append(skeleton.get(i)[0]); // write headers
				fileWriter.append("\n"); // new line
				String[] ArcGISfields = skeleton.get(i)[1].split(","); // produce iterable list from comma separated string
				Map<String, Object> attributes = attributeslist.get(i);
				ArcGISFeatureLayer layer = layers.get(i);

				for (int j=0; j<ArcGISfields.length-1; j++) { // for each ArcGIS attribute
					if (layer==JParkSim.LoadPointslayer) { // specific to load point
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
	    user.setUserAccount("jparksimulator", "c4tjpark"); // Access secure feature layer service using login username and password
		try {
			long start = System.currentTimeMillis();
			String line = null;
			fileReader = new BufferedReader(new FileReader(OUTCSV));
			fileReader.readLine(); // Read the CSV file header to skip it
			GeodatabaseFeatureServiceTable LoadPointsTable = new GeodatabaseFeatureServiceTable("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/Load_Points/FeatureServer", user, 0);
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
/*			
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
*/			
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