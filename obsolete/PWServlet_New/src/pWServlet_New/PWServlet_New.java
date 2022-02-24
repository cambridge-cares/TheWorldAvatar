/**
	 * Servlet for the industrial park level electrical network
	 */
package pWServlet_New;

import java.io.*;
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

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;


public class PWServlet_New extends HttpServlet {	
	
	public static File PWSurrogateModel = new File("C:/apache-tomcat-8.0.24/webapps/ROOT/PW_SurrogateModel/PWSurrogateModel.txt");
	
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReq.CSV");	
	public static String PrPWOUT = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUT.CSV");
	public static String PrPWIN = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWIN.CSV");
	
	public static Map<String, String> ArcGISFIDtoPWBusNum = new HashMap<>();   // Maps ArcGIS FID/OBJECTID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFID = new HashMap<>();   // reverse mapping BusNum to ArcGIS FID/OBJECTID
	public static Map<String, String> ArcGISFIDtoPGBusNum = new HashMap<>();   // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PGBusNumtoArcGISFID = new HashMap<>();   // reverse mapping BusNum to ArcGIS FID
	public static Map<String, String> SubstationtoPWBusNum = new HashMap<>();  // Maps substation number to HV and LV bus numbers
	public static Map<String, String> PWBusNumtoSubstation = new HashMap<>();  // reverse mapping
	
	public PWServlet_New() {
		super();
		// Hard Coded ArcGISFIDtoPWBusNum
				ArcGISFIDtoPWBusNum.put("2", "41"); // Oiltanking Asia Pacific
				ArcGISFIDtoPWBusNum.put("3", "39"); // Oiltanking Asia Pacific
				ArcGISFIDtoPWBusNum.put("4", "38"); // Oiltanking Asia Pacific
				ArcGISFIDtoPWBusNum.put("5", "36"); // Stepan
				ArcGISFIDtoPWBusNum.put("6", "32"); // PacificLight Power
				ArcGISFIDtoPWBusNum.put("7", "34"); // Continental Chemical
				ArcGISFIDtoPWBusNum.put("8", "35"); // Coim Asia Pacific
				ArcGISFIDtoPWBusNum.put("9", "62"); // Oiltanking Odfjell
				ArcGISFIDtoPWBusNum.put("61", "80"); // Air Liquide
				ArcGISFIDtoPWBusNum.put("52", "31"); // Azko Nobel
				ArcGISFIDtoPWBusNum.put("159", "175"); // Asahi Kasei Chemicals
				ArcGISFIDtoPWBusNum.put("90", "122"); // Asahi Kasei Plastics
				ArcGISFIDtoPWBusNum.put("173", "182"); // CCD Singapore
				ArcGISFIDtoPWBusNum.put("78", "115"); // Celanese
				ArcGISFIDtoPWBusNum.put("91", "127"); // Chemical Industries
				ArcGISFIDtoPWBusNum.put("24", "54"); // Chemical Specialties
				ArcGISFIDtoPWBusNum.put("23", "53"); // Chemical Specialties
				ArcGISFIDtoPWBusNum.put("85", "121"); // Chevron Oronite
				ArcGISFIDtoPWBusNum.put("178", "138"); // Ciba Industries
				ArcGISFIDtoPWBusNum.put("19", "47"); // Croda
				ArcGISFIDtoPWBusNum.put("16", "59"); // DENKA
				ArcGISFIDtoPWBusNum.put("44", "82"); // DENKA
				ArcGISFIDtoPWBusNum.put("89", "124"); // DIC Alkylphenol
				ArcGISFIDtoPWBusNum.put("100", "128"); // Du Pont
				ArcGISFIDtoPWBusNum.put("73", "116"); // Du Pont
				ArcGISFIDtoPWBusNum.put("74", "117"); // Du Pont
				ArcGISFIDtoPWBusNum.put("75", "118"); // Du Pont
				ArcGISFIDtoPWBusNum.put("151", "192"); // Essar Projects Aromatics
				ArcGISFIDtoPWBusNum.put("106", "150"); // ExxonMobil Asia Pacific
				ArcGISFIDtoPWBusNum.put("103", "93"); // ExxonMobil Chemical
				ArcGISFIDtoPWBusNum.put("104", "134"); // ExxonMobil Chemical
				ArcGISFIDtoPWBusNum.put("102", "94"); // ExxonMobil Refinery
				ArcGISFIDtoPWBusNum.put("56", "66"); // Faci Asia
				ArcGISFIDtoPWBusNum.put("84", "108"); // HDB Building
				ArcGISFIDtoPWBusNum.put("124", "153"); // Horizon
				ArcGISFIDtoPWBusNum.put("86", "123"); // Horizon Korea
				ArcGISFIDtoPWBusNum.put("21", "49"); // Huntsman
				ArcGISFIDtoPWBusNum.put("97", "107"); // ITRO
				ArcGISFIDtoPWBusNum.put("118", "205"); // Ibris Bio-fuels
				ArcGISFIDtoPWBusNum.put("107", "148"); // ICES
				ArcGISFIDtoPWBusNum.put("71", "102"); // Invista
				ArcGISFIDtoPWBusNum.put("149", "203"); // JI Maintenance Hub
				ArcGISFIDtoPWBusNum.put("114", "206"); // Katoen
				ArcGISFIDtoPWBusNum.put("115", "207"); // Katoen
				ArcGISFIDtoPWBusNum.put("109", "147"); // Keppel Merlimau Cogen
				ArcGISFIDtoPWBusNum.put("72", "114"); // Kuraray
				ArcGISFIDtoPWBusNum.put("120", "200"); // LTH Logistics
				ArcGISFIDtoPWBusNum.put("156", "187"); // Lanxess
				ArcGISFIDtoPWBusNum.put("54", "71"); // Linde Gas
				ArcGISFIDtoPWBusNum.put("79", "113"); // Lucite
				ArcGISFIDtoPWBusNum.put("94", "131"); // Mitsui Chemicals
				ArcGISFIDtoPWBusNum.put("36", "86"); // Mitsui Elastomers
				ArcGISFIDtoPWBusNum.put("93", "132"); // Mitsui Phenol
				ArcGISFIDtoPWBusNum.put("18", "57"); // Nikko Chemicals
				ArcGISFIDtoPWBusNum.put("65", "97"); // Oasis Sakra
				ArcGISFIDtoPWBusNum.put("50", "40"); // Oiltanking Asia Pacific
				ArcGISFIDtoPWBusNum.put("13", "63"); // Oiltanking Odfjell
				ArcGISFIDtoPWBusNum.put("12", "64"); // Oiltanking Odfjell
				ArcGISFIDtoPWBusNum.put("11", "52"); // Oiltanking Odfjell
				ArcGISFIDtoPWBusNum.put("29", "75"); // PTC Chemical
				ArcGISFIDtoPWBusNum.put("28", "76"); // PTC Chemical
				ArcGISFIDtoPWBusNum.put("27", "77"); // PTC Chemical
				ArcGISFIDtoPWBusNum.put("99", "103"); // Perstorp
				ArcGISFIDtoPWBusNum.put("96", "98"); // Perstorp or Lonza
				ArcGISFIDtoPWBusNum.put("98", "105"); // Perstorp or Lonza
				ArcGISFIDtoPWBusNum.put("45", "83"); // Petrochemical Corp
				ArcGISFIDtoPWBusNum.put("64", "95"); // Petrofac
				ArcGISFIDtoPWBusNum.put("31", "70"); // Rotary Engineering
				ArcGISFIDtoPWBusNum.put("143", "193"); // Sembcorp
				ArcGISFIDtoPWBusNum.put("82", "111"); // Sembcorp Cogen
				ArcGISFIDtoPWBusNum.put("87", "125"); // Sembcorp Gas
				ArcGISFIDtoPWBusNum.put("81", "110"); // Sembcorp Industries
				ArcGISFIDtoPWBusNum.put("152", "191"); // Sembcorp Integrated
				ArcGISFIDtoPWBusNum.put("83", "109"); // Sembcorp
				ArcGISFIDtoPWBusNum.put("14", "60"); // Shell Chemicals
				ArcGISFIDtoPWBusNum.put("179", "137"); // Si Group
				ArcGISFIDtoPWBusNum.put("101", "104"); // Glacial Acrylic
				ArcGISFIDtoPWBusNum.put("68", "130"); // Glacial Acrylic
				ArcGISFIDtoPWBusNum.put("132", "162"); // LNG Terminal
				ArcGISFIDtoPWBusNum.put("130", "163"); // LNG Terminal
				ArcGISFIDtoPWBusNum.put("35", "84"); // Singapore Petrochemicals
				ArcGISFIDtoPWBusNum.put("39", "88"); // Singapore Petrochemicals
				ArcGISFIDtoPWBusNum.put("46", "45"); // Singapore Refining
				ArcGISFIDtoPWBusNum.put("47", "46"); // Singapore Refining
				ArcGISFIDtoPWBusNum.put("57", "67"); // Stella Chemifa
				ArcGISFIDtoPWBusNum.put("164", "177"); // Stolthaven
				ArcGISFIDtoPWBusNum.put("38", "99"); // Sumitomo Chemical
				ArcGISFIDtoPWBusNum.put("66", "85"); // Sumitomo Chemical
				ArcGISFIDtoPWBusNum.put("69", "100"); // Sumitomo Mitsui
				ArcGISFIDtoPWBusNum.put("88", "126"); // Tate and Lyle
				ArcGISFIDtoPWBusNum.put("95", "133"); // Teijin Polycarbonate
				ArcGISFIDtoPWBusNum.put("165", "178"); // Tembusu Multi Utilities
				ArcGISFIDtoPWBusNum.put("174", "180"); // Tembusu Multi Utilities
				ArcGISFIDtoPWBusNum.put("176", "184"); // Tembusu Multi Utilities
				ArcGISFIDtoPWBusNum.put("42", "89"); // The Polyolefin
				ArcGISFIDtoPWBusNum.put("43", "90"); // The Polyolefin
				ArcGISFIDtoPWBusNum.put("70", "101"); // Unimatec
				ArcGISFIDtoPWBusNum.put("126", "156"); // Universal Terminal
				ArcGISFIDtoPWBusNum.put("116", "119"); // Vopak
				ArcGISFIDtoPWBusNum.put("111", "120"); // Vopak
				ArcGISFIDtoPWBusNum.put("76", "208"); // Vopak
				ArcGISFIDtoPWBusNum.put("113", "136"); // Vopak
				ArcGISFIDtoPWBusNum.put("77", "197"); // Vopak
				ArcGISFIDtoPWBusNum.put("10", "65"); // YTL PowerSeraya
				ArcGISFIDtoPWBusNum.put("148", "202"); // Zeon Chemicals
				ArcGISFIDtoPWBusNum.put("67", "106"); // Public Building (00856N)

				// unidentified load points
				ArcGISFIDtoPWBusNum.put("62", "81"); // 02451C
				ArcGISFIDtoPWBusNum.put("169", "142"); // 01228T
				ArcGISFIDtoPWBusNum.put("48", "44"); // 02464L
				ArcGISFIDtoPWBusNum.put("112", "139"); // 03158V
				ArcGISFIDtoPWBusNum.put("129", "164"); // U128
				ArcGISFIDtoPWBusNum.put("168", "181"); // 01772N
				ArcGISFIDtoPWBusNum.put("92", "129"); // 01650A
				ArcGISFIDtoPWBusNum.put("55", "68"); // 02450L
				ArcGISFIDtoPWBusNum.put("166", "179"); // 03182K
				ArcGISFIDtoPWBusNum.put("34", "74"); // U33
				ArcGISFIDtoPWBusNum.put("119", "201"); // 02322T
				ArcGISFIDtoPWBusNum.put("33", "72"); // 03153X
				ArcGISFIDtoPWBusNum.put("121", "199"); // U120
				ArcGISFIDtoPWBusNum.put("122", "151"); // U121
				ArcGISFIDtoPWBusNum.put("123", "152"); // 02263P
				ArcGISFIDtoPWBusNum.put("32", "69"); // 03154L
				ArcGISFIDtoPWBusNum.put("125", "155"); // 01873A
				ArcGISFIDtoPWBusNum.put("53", "33"); // 02596N
				ArcGISFIDtoPWBusNum.put("127", "158"); // U126
				ArcGISFIDtoPWBusNum.put("30", "51"); // U29
				ArcGISFIDtoPWBusNum.put("80", "112"); // 00712M
				ArcGISFIDtoPWBusNum.put("171", "143"); // 02319T
				ArcGISFIDtoPWBusNum.put("105", "149"); // 03296P
				ArcGISFIDtoPWBusNum.put("170", "144"); // 03270A
				ArcGISFIDtoPWBusNum.put("133", "161"); // 03325A
				ArcGISFIDtoPWBusNum.put("51", "37"); // 02592P
				ArcGISFIDtoPWBusNum.put("161", "174"); // 03051L
				ArcGISFIDtoPWBusNum.put("158", "170"); // 03240K
				ArcGISFIDtoPWBusNum.put("137", "166"); // 03307T
				ArcGISFIDtoPWBusNum.put("157", "171"); // U156
				ArcGISFIDtoPWBusNum.put("155", "188"); // 03152N
				ArcGISFIDtoPWBusNum.put("154", "185"); // U153
				ArcGISFIDtoPWBusNum.put("153", "186"); // 02878X
				ArcGISFIDtoPWBusNum.put("150", "204"); // 01968M
				ArcGISFIDtoPWBusNum.put("108", "146"); // U107
				ArcGISFIDtoPWBusNum.put("145", "190"); // 03145P
				ArcGISFIDtoPWBusNum.put("162", "172"); // 03233V
				ArcGISFIDtoPWBusNum.put("144", "165"); // 03312K
				ArcGISFIDtoPWBusNum.put("147", "198"); // 03299K
				ArcGISFIDtoPWBusNum.put("26", "56"); // 03279P
				ArcGISFIDtoPWBusNum.put("25", "55"); // U24
				ArcGISFIDtoPWBusNum.put("142", "195"); // 03311A
				ArcGISFIDtoPWBusNum.put("22", "50"); // 02382V
				ArcGISFIDtoPWBusNum.put("20", "48"); // 02380M
				ArcGISFIDtoPWBusNum.put("141", "169"); // 03303M
				ArcGISFIDtoPWBusNum.put("140", "194"); // 03096L                    ???
				ArcGISFIDtoPWBusNum.put("139", "196"); // 03338T
				ArcGISFIDtoPWBusNum.put("17", "58"); // 03280W
				ArcGISFIDtoPWBusNum.put("138", "168"); // 03333C
				ArcGISFIDtoPWBusNum.put("136", "167"); // 02817L
				ArcGISFIDtoPWBusNum.put("15", "61"); // 03238N
				ArcGISFIDtoPWBusNum.put("160", "176"); // 03052C
				ArcGISFIDtoPWBusNum.put("135", "154"); // 03119X
				ArcGISFIDtoPWBusNum.put("146", "189"); // 03023W
				ArcGISFIDtoPWBusNum.put("163", "173"); // 03236A
				ArcGISFIDtoPWBusNum.put("134", "157"); // 01972W
				ArcGISFIDtoPWBusNum.put("131", "160"); // 02818C
				ArcGISFIDtoPWBusNum.put("117", "209"); // 02432X
				ArcGISFIDtoPWBusNum.put("167", "145"); // 02948K
				ArcGISFIDtoPWBusNum.put("37", "87"); // 03204T
				ArcGISFIDtoPWBusNum.put("110", "135"); // 02502K
				ArcGISFIDtoPWBusNum.put("40", "91"); // 03186C
				ArcGISFIDtoPWBusNum.put("41", "92"); // 03249T
				ArcGISFIDtoPWBusNum.put("172", "141"); // 03291L
				ArcGISFIDtoPWBusNum.put("49", "43"); // 02954L
				ArcGISFIDtoPWBusNum.put("58", "79"); // 02838V
				ArcGISFIDtoPWBusNum.put("177", "183"); // 02944V
				ArcGISFIDtoPWBusNum.put("63", "96"); // 01384X
				ArcGISFIDtoPWBusNum.put("1", "42"); // 03033X

				ArcGISFIDtoPWBusNum.put("59", "73"); // Carotino
				ArcGISFIDtoPWBusNum.put("128", "159"); // Oiltanking Helios Singapore

				for (Map.Entry<String, String> entry : ArcGISFIDtoPWBusNum.entrySet()) { // reverse mapping
					PWBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
				}
				
				// FID to BusNum map for power generators
				ArcGISFIDtoPGBusNum.put("6", "1"); // Singapore utility grid
				ArcGISFIDtoPGBusNum.put("1", "5"); // Seraya 1
				ArcGISFIDtoPGBusNum.put("2", "6"); // Seraya 2
				ArcGISFIDtoPGBusNum.put("3", "7"); // Seraya 3
				ArcGISFIDtoPGBusNum.put("4", "8"); // SembCorp Cogen
				ArcGISFIDtoPGBusNum.put("5", "9"); // Keppel Merlimau

				for (Map.Entry<String, String> entry : ArcGISFIDtoPGBusNum.entrySet()) { // reverse mapping
					PGBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
				}
				
				// first bus number is high voltage bus, second bus number is low voltage bus
				SubstationtoPWBusNum.put("UHT1HV", "2");
				SubstationtoPWBusNum.put("UHT1LV", "10");
				SubstationtoPWBusNum.put("UHT2HV", "3");
				SubstationtoPWBusNum.put("UHT2LV", "11");
				SubstationtoPWBusNum.put("UHT3HV", "4");
				SubstationtoPWBusNum.put("UHT3LV", "12");
				SubstationtoPWBusNum.put("EHT1HV", "13");
				SubstationtoPWBusNum.put("EHT1LV", "22");
				SubstationtoPWBusNum.put("EHT2HV", "14");
				SubstationtoPWBusNum.put("EHT2LV", "23");
				SubstationtoPWBusNum.put("EHT3HV", "15");
				SubstationtoPWBusNum.put("EHT3LV", "24");
				SubstationtoPWBusNum.put("EHT9HV", "16");
				SubstationtoPWBusNum.put("EHT9LV", "25");
				SubstationtoPWBusNum.put("EHT4HV", "17");
				SubstationtoPWBusNum.put("EHT4LV", "26");
				SubstationtoPWBusNum.put("EHT5HV", "18");
				SubstationtoPWBusNum.put("EHT5LV", "27");
				SubstationtoPWBusNum.put("EHT6HV", "19");
				SubstationtoPWBusNum.put("EHT6LV", "28");
				SubstationtoPWBusNum.put("EHT7HV", "20");
				SubstationtoPWBusNum.put("EHT7LV", "29");
				SubstationtoPWBusNum.put("EHT8HV", "21");
				SubstationtoPWBusNum.put("EHT8LV", "30");

				for (Map.Entry<String, String> entry : SubstationtoPWBusNum.entrySet()) {
					PWBusNumtoSubstation.put(entry.getValue(), entry.getKey() .substring(3, 4)); // remove all characters from key, only need FID
				}
		
	}
	
	private static final long serialVersionUID = 1L;
	public static long start_time;
	public static long end_time;
	public static long interval;

	/**this is the main method of this servlet which is used to listen to HttpRequest, execute the corresponding model and send back HttpRsponse */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		/**reconstructing editStack from request string so that the information can be passed on to the relevant methods*/
		ArrayList<String[]> editStack = new ArrayList<String[]>(); 
		String[] layers = request.getParameter("layers").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(",");     
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); 

		for (int i = 0; i < layers.length; i++) {
			editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i]});
		}
		
		/**check whether the httpRequst has been correctly received */
		FileWriter flag = null; 
		flag = new FileWriter(httpReqCSV);
		flag.append("layers=" + layers[0]);
		flag.append(", OBJECTIDs=" + OBJECTIDs[0]);
		flag.append(", appCallFlag=" + appCallFlag[0]);
		flag.flush();
		flag.close();
				
		/**the following part of code distinguishes which functionality of the JParkSimulator has been called, and then provides the corresponding service by evaluating the associated model */
		switch (appCallFlag[0]) {					
		case "PWPr":
			System.out.println(appCallFlag[0] + " button was pressed!");
			start_time = System.currentTimeMillis();
			RunPrPW(layers,OBJECTIDs);
//			readPrPWCSV();
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;			
		    
		} 
	} 

	public void RunPrPW (String[] Layer, String[] ID){

		List<List<Double>> xData = new ArrayList<>(1); 
		List<List<Double>> yData;  
		List<Double> X_Values = new ArrayList<>();                                                   // create an array to store all the x-values
		String[] layers = Layer;                                                                     // get the modified layer
		String[] OBJECTIDs = ID;
		X_Values = getInputX(layers,OBJECTIDs);                                                      // get the input x-value set
		
		xData.add(X_Values);
		String ModelUrl;
		String ModelName;                                                 
		
		/**load the ontology file for electrical network*/
		String uri = "File:/C:/apache-tomcat-8.0.24/webapps/OntologyFiles/ElectricalNetwork.owl";
        OWLModel owlModel = null;
		try {
			owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
		} catch (OntologyLoadException e1) {
			e1.printStackTrace();
		}
		
		/**extract the ModelUrl and ModelName from the loaded ontology*/
		OWLIndividual M = owlModel.getOWLIndividual("http://www.jparksimulator.com/ElectricalNetwork.owl#"+ "SurrogateMdel_ElectricalGridOfJPark");
		ModelUrl = M.getPropertyValueLiteral(owlModel.getOWLProperty("mathematical_model_extended:hasModelUrl")).getString();
		ModelName = M.getPropertyValueLiteral(owlModel.getOWLProperty("mathematical_model_extended:hasModelName")).getString();
		
		System.load("C:/apache-tomcat-8.0.24/webapps/PWServlet_New/WEB-INF/lib/MoDS_Java_API.dll");  // load the MoDS API, the MoDS API at use is version 0.1
		
		ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(ModelUrl, ModelName);
		System.out.println("yNames= " + yNames);
		
		yData = MoDSAPI.evaluateSurrogate(ModelUrl, ModelName, xData);                               // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam"  and  the input xData that was collected before
		System.out.println("yData=" + yData);                                                        // print out the output yData to console
		
        /**filewriter to generate the output CSV file*/
		FileWriter FileWriter = null;         
		try {
			FileWriter = new FileWriter(PrPWOUT);
			for (int j = 0; j < yNames.size(); j++) {
				FileWriter.append(yNames.get(j));                                               // write the yNames to the output CSV file
				FileWriter.append(",");
			}
			for (int j = 0; j < yData.size(); j++) {
				FileWriter.append("\n");
				for (int k = 0; k < yData.get(j).size(); k++) {
					FileWriter.append(Double.toString(yData.get(j).get(k)));                        // write the yData to the output CSV file
					FileWriter.append(",");
				} 
			}
			FileWriter.flush();
			FileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}	
		/**read the simulation result from csv file and update to ArgGIS database*/
		readPrPWCSV();
	}
	
	/**Get the input x-value for the Parameterized PowerWorld model*/
	public List<Double> getInputX (String[] Layer, String[] ID){
		
		List<Double> X_Values = new ArrayList<>();                                                              // create an array to store all the x-values
		String[] layers = Layer;                                                                                // get the modified layer
		String[] OBJECTIDs = ID;  
		String[] Temp = null;                                                                                   // get the modified OBJECTID		
		String BusNum = new String();
		String LayerNam = new String();
		String ParNam = new String();
		boolean Flag ;                                                                                          // create an boolean variable to indicate ... 
		ArrayList<Map<String, Object>> attributeslist_LP = new ArrayList<Map<String, Object>>();                // additional ArrayList for loadpoints
		ArrayList<Map<String, Object>> attributeslist_PG = new ArrayList<Map<String, Object>>();                // additional ArrayList for powergen
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		
		/**load the ontology file for electrical network*/
		String uri = "File:/C:/apache-tomcat-8.0.24/webapps/OntologyFiles/ElectricalNetwork.owl";
        OWLModel owlModel = null;
		try {
			owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
		} catch (OntologyLoadException e1) {
			e1.printStackTrace();
		}
		
		try{
			InputStream ips = new FileInputStream(PWSurrogateModel);
			InputStreamReader ipsr = new InputStreamReader (ips);
			BufferedReader br = new BufferedReader(ipsr);
			String line;
			while ((line = br.readLine()) != null){
				Temp = line.split("_");
//				BusNum = Temp[Temp.length-1].substring(0, Temp[Temp.length-1].length()-1);                      // extracting the Bus Number information from the x-variable name string, and exclude the last number "1" from the string
				BusNum = Temp[Temp.length-2]; 
				LayerNam = Temp[Temp.length-3].substring(0, 1);                                                 // extract the first letter of the layer name
				ParNam = Temp[Temp.length-3];                                                                   // extracting the name of the required parameter, 
                
				/**To extract the required x-value from owl file or ArcGIS database*/
				Flag = false;                                                                                   // set the initial value of Flag to false
				for(int i=0; i<layers.length; i++ ){	
					
					if(LayerNam.equals("L") && LayerNam.equals(layers[i].substring(0, 1)) && BusNum.equals(ArcGISFIDtoPWBusNum.get(OBJECTIDs[0]))){
						
						/**query ArcGIS database to extract the user modified data, if any parameter of the load points was modified*/
						int counter = 0;
						for (String key : OBJECTIDs) {
							try {							
								QueryParameters qParameter_LP = new QueryParameters();       // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
								qParameter_LP.setWhere("OBJECTID='" + key + "'");            // define FID address  of an ArcGIS element
								qParameter_LP.setOutFields(new String[] { "*" });            // fetch all  attributes of an ArcGIS element using *
								QueryTask qTask_LP = null;                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
								Feature graphic_LP = null;                                   // create an instance of Feature to store an ArcGIS element				
						
								qTask_LP = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Load_points/FeatureServer/0",user);            // store URL address of appropriate databaseand user credentials
								FeatureResult fResult_LP = qTask_LP.execute(qParameter_LP);  // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and qTask_LP
								graphic_LP = (Feature) fResult_LP.iterator().next();         // queryResult.iterator() iterates over the elements  in fResult_LP and stores it in  graphic_LP; qParameter_LP  requests information about a single element only
								attributeslist_LP.add(graphic_LP.getAttributes());           // append information about the element in graphic_LP to ArrayList attributeslist_LP
								
								counter++;
				                if (counter == OBJECTIDs.length) {		
									System.out.print("Done loading attribute table " + OBJECTIDs.length);
									break;
								}
							} catch (Exception e) {
								e.printStackTrace();                                         // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
							}
						}								

						for (int j = 0; j < attributeslist_LP.size(); j++) {					
							for (String key : attributeslist_LP.get(j).keySet()) { 
								if (key == "OBJECTID") {
									if(ParNam.equals("LoadMW")){
										X_Values.add(Double.parseDouble(String.valueOf(attributeslist_LP.get(j).get("pwr_P"))));     //convert the extracted data from string to double
									}else if(ParNam.equals("LoadMVR")){
										X_Values.add(Double.parseDouble(String.valueOf(attributeslist_LP.get(j).get("pwr_Q"))));
									}									
								}
							}
							System.out.println("X_Values = " + X_Values);						
						}	
						Flag = true;
					}
					else if(LayerNam.equals("G") && LayerNam.equals(layers[i].substring(0, 1)) && BusNum.equals(ArcGISFIDtoPWBusNum.get(OBJECTIDs[0]))){
						/**query ArcGIS database to extract the user modified data, if any parameter of the power generators was modified*/
						int counter = 0;
						for (String key : OBJECTIDs) {
							try {

								QueryParameters qParameter_PG = new QueryParameters();       // create an instance of QueryParameters to be used for querying ArcGIS database for  predefined data
								qParameter_PG.setWhere("OBJECTID='" + key + "'");            // define FID address  of an ArcGIS element
								qParameter_PG.setOutFields(new String[] { "*" });            // fetch all  attributes of an ArcGIS element using *
								QueryTask qTask_PG = null;                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
								Feature graphic_PG = null;                                   // create an instance of Feature to store an ArcGIS element

								qTask_PG = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Generators/FeatureServer/0",user);              // store URL address of appropriate databaseand user credentials
								FeatureResult fResult_PG = qTask_PG.execute(qParameter_PG);  // FeatureResult is used to store information from ArcGIS database requested using qParameter_PG and qTask_PG
								graphic_PG = (Feature) fResult_PG.iterator().next();         // queryResult.iterator() iterates over the elements in fResult_PG and stores it in graphic_PG; qParameter_PG requests information about a single element only
								attributeslist_PG.add(graphic_PG.getAttributes());           // append information about the element in graphic_PG to ArrayList attributeslist_LP

								counter++;
				                if (counter == OBJECTIDs.length) {		
									System.out.print("Done loading attribute table " + OBJECTIDs.length);
									break;
								}
							} catch (Exception e) {
								e.printStackTrace();                                         // It prints the stack trace of the Exception to System.err.
							}
						}
						Flag = true;
					}
					
				}
			if(!Flag){	

			    /**extract the the reactive power of the load points*/
				if(Temp[Temp.length-3].equals("LoadMW")){
					OWLIndividual M = owlModel.getOWLIndividual("http://www.jparksimulator.com/ElectricalNetwork.owl#V_ActualActivePower_LoadPoint_"+ BusNum);
					String LoadActivePower = M.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
				    X_Values.add(Double.parseDouble(LoadActivePower));
					
				}
				else if(Temp[Temp.length-3].equals("LoadMVR")){
					OWLIndividual M = owlModel.getOWLIndividual("http://www.jparksimulator.com/ElectricalNetwork.owl#V_ActualReactivePower_LoadPoint_"+ BusNum);
				    String LoadReactivePower = M.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
				    X_Values.add(Double.parseDouble(LoadReactivePower));
				}
				else if(Temp[Temp.length-3].equals("GenMW")){
					OWLIndividual M = owlModel.getOWLIndividual("http://www.jparksimulator.com/ElectricalNetwork.owl#V_P_PowerGen_"+ BusNum);
				    String PowerGen_P = M.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
				    X_Values.add(Double.parseDouble(PowerGen_P));
				}
				else if(Temp[Temp.length-3].equals("GenVoltSet")){
					OWLIndividual M = owlModel.getOWLIndividual("http://www.jparksimulator.com/ElectricalNetwork.owl#V_ActualV_PowerGen_"+ BusNum);
				    String PowerGen_V = M.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
				    X_Values.add(Double.parseDouble(PowerGen_V));
				}

			}
				
			}
			br.close();
			System.out.println("The input X we want is: "+ X_Values);
		}catch (Exception e){
			System.out.println(e.toString());
		}
		
		/**filewriter to generate the Input CSV file*/      
		FileWriter FileWriter = null; 
		try {
			FileWriter = new FileWriter(PrPWIN);
			for (int j = 0; j < X_Values.size(); j++) {
				FileWriter.append(Double.toString(X_Values.get(j)));       // write the x-values to the Input CSV file
				FileWriter.append("\n");
			}
			FileWriter.flush();
			FileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return X_Values;
				
	}

	
	/**this method reads the csv file generated by the powerworld model and update to ArcGIS database */
	public void readPrPWCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
		try {
			long start = System.currentTimeMillis();                        // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrPWOUT));
			fileReader.readLine();                                          // Read the CSV file header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");               // Load all features using SQL command

			GeodatabaseFeatureServiceTable LoadPointsTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Load_points/FeatureServer", user, 0);
			LoadPointsTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadPointsTable.initialize();
			System.out.println(LoadPointsTable.getStatus());
			LoadPointsTable.getInitializationError();
			GeodatabaseFeatureServiceTable UHTSubstationTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/UHT_substations/FeatureServer", user, 0);
			UHTSubstationTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			UHTSubstationTable.initialize();
			System.out.println(UHTSubstationTable.initialize());
			GeodatabaseFeatureServiceTable EHTSubstationTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/EHT_substation/FeatureServer", user, 0);
			EHTSubstationTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			EHTSubstationTable.initialize();
			System.out.println(EHTSubstationTable.initialize());
			GeodatabaseFeatureServiceTable PowerGenTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Generators/FeatureServer", user, 0);
			PowerGenTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			PowerGenTable.initialize();
			System.out.println(PowerGenTable.initialize());
			//
			// EXPAND CODE HERE
			//

			final CountDownLatch latch = new CountDownLatch(4); // handles four asynchronous processes, only continues Thread when it reaches 0
			LoadPointsTable.populateFromService(loadAllFeatures, false,
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
			UHTSubstationTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {
							if (status == true) {
								latch.countDown();
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			EHTSubstationTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {
							if (status == true) {
								latch.countDown();
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			PowerGenTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {
							if (status == true) {
								latch.countDown();
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});

			latch.await();                                   // wait until all feature service tables are ready then continue
			while ((line = fileReader.readLine()) != null) { // Continue reading lines until none left
				String[] data = line.split(",");             // split string by comma

				for (int j = 0; j < data.length/7; j++) {
					String PWBusNum = String.valueOf(j+1);
					String ArcGISFID = PWBusNumtoArcGISFID.get(PWBusNum);
					System.out.println("ArcGISFID= " + ArcGISFID);
					
					if (ArcGISFID != null) {
						Map<String, Object> LoadPointAttributes = LoadPointsTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
						if (!data[1 + 7 * j].trim().isEmpty()) {
							LoadPointAttributes .put("theta_act", Float.parseFloat(data[1 + 7 * j] .trim()) * 2 * 3.146 / 360); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
						}
						if (!data[2 + 7 * j].trim().isEmpty()) { 
							LoadPointAttributes.put("volt_act", Float.parseFloat(data[2 + 7 * j].trim()));
						}
						if (!data[3 + 7 * j].trim().isEmpty()) {
							LoadPointAttributes.put("pwr_P_act",Float.parseFloat(data[3 + 7 * j].trim()));
						}
						if (!data[4 + 7 * j].trim().isEmpty()) {
							LoadPointAttributes.put("pwr_Q_act",Float.parseFloat(data[4 + 7 * j].trim()));
						}
						
						LoadPointsTable.updateFeature( Long.parseLong(ArcGISFID), LoadPointAttributes); // update feature table locally
					}
					if (j+1 >= 5 && j+1 <= 9) { // PowerGen buses
						Map<String, Object> PowerGenAttributes = PowerGenTable.getFeature((long) (j+1 - 4)).getAttributes(); // subtract 4 from  PWBusNum to get  FID
					/**	if (!data[4 + 5 * j].trim().isEmpty()) {
							PowerGenAttributes.put("volt_nom", Float.parseFloat(data[4 + 5 * j].trim())); // BUSNOMVOLT
						}*/ 
						if (!data[2 + 7 * j].trim().isEmpty()) {
							PowerGenAttributes.put("volt_act", Float.parseFloat(data[2 + 7 * j].trim())); // BUSKVVOLT
						}				
						 PowerGenTable.updateFeature((long) (j+1-4),PowerGenAttributes);
					}
/**					if ((j+1 >= 2 && j+1 <= 4) || (j+1 >= 10 && j+1 <= 12)) { // UHTSubstation
						String SubstationFID = PWBusNumtoSubstation.get(String.valueOf(PWBusNum));
						Map<String, Object> UHTSubstationAttributes = UHTSubstationTable.getFeature(Long.parseLong(SubstationFID)).getAttributes();
						if (!data[4 + 7 * j].trim().isEmpty()) {
							if (j+1 >= 2 && j+1 <= 4) { // high voltage bus
								UHTSubstationAttributes.put("HV_kV", Float.parseFloat(data[4 + 7 * j].trim())); // BUSNOMVOLT
							} else { // low voltage bus
								UHTSubstationAttributes.put("LV_kV", Float.parseFloat(data[4 + 7 * j].trim())); // BUSNOMVOLT
							}
						}
						if (!data[5 + 5 * j].trim().isEmpty()) {
							if (j+1 >= 2 && j+1 <= 4) {
								UHTSubstationAttributes.put("HV_kV_act", Float.parseFloat(data[5 + 5 * j].trim())); // BUSKVVOLT
							} else {
								UHTSubstationAttributes.put("LV_kV_act", Float.parseFloat(data[5 + 5 * j].trim())); // BUSKVVOLT
							}
						}
						UHTSubstationTable.updateFeature( Long.parseLong(SubstationFID), UHTSubstationAttributes);
					}*/
/**					if (j+1 >= 13 && j+1 <= 30) { // EHT Substation
						String SubstationFID = PWBusNumtoSubstation.get(String.valueOf(PWBusNum));
						Map<String, Object> EHTSubstationAttributes = EHTSubstationTable.getFeature(Long.parseLong(SubstationFID)).getAttributes();
						if (!data[4 + 5 * j].trim().isEmpty()) {
							if (j+1 >= 13 && j+1 <= 21) { // high voltage bus
								EHTSubstationAttributes.put("HV_kV", Float.parseFloat(data[4 + 5 * j].trim())); // BUSNOMVOLT
							} else { // low voltage bus
								EHTSubstationAttributes.put("LV_kV", Float.parseFloat(data[4 + 5 * j].trim())); // BUSNOMVOLT
							}
						}
						if (!data[5 + 5 * j].trim().isEmpty()) {
							if (j+1 >= 13 && j+1 <= 21) {
								EHTSubstationAttributes.put("HV_kV_act", Float.parseFloat(data[5 + 5 * j].trim())); // BUSKVVOLT
							} else {
								EHTSubstationAttributes.put("LV_kV_act", Float.parseFloat(data[5 + 5 * j].trim())); // BUSKVVOLT
							}
						}
						EHTSubstationTable.updateFeature(Long.parseLong(SubstationFID), EHTSubstationAttributes);
					}*/					
				}
			}
			LoadPointsTable.applyEdits(null); // commit local updates onto server
			PowerGenTable.applyEdits(null);
			UHTSubstationTable.applyEdits(null);
			EHTSubstationTable.applyEdits(null);

			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms"); // tells you how long it took to update
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
