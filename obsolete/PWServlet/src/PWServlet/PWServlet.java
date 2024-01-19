/*
 * this PWServlet listens (doPost method) to the httpRequest sent from JParkSim (delivered by TomCat), 
 * and then implements the associated model, gives a result, and then visualizes to the user interface (through ArcGIS database at the moment)
 * and send back httpRespond as well.
 */

package PWServlet;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;

import com.cmclinnovations.modsapi.MoDSAPI;
import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.EsriSecurityException;
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
/*import aspenPlusModels.AspenPlusModels;
import de.derivo.sparqldlapi.QueryEngine;
import prAspenPlusModels.PrAspenPlusModels;
import informationQuery.InformationQuery;*/

public class PWServlet extends HttpServlet {
		
	private static final long serialVersionUID = 1L;
	public static long start_time;
	public static long end_time;
	public static long interval;
	public static long start_time1;
	public static long end_time1;
	public static long start_time2;
	public static long end_time2;
	
	public static ArrayList<String[]> editStack;
	
	public static Map<Integer, String> OBJECTIDtogaslinenum = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the Reactor
	
	public static Map<String, String> ArcGISFIDtoPWBusNum = new HashMap<>(); // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFID = new HashMap<>(); // reverse mapping BusNum to ArcGIS FID
	public static Map<String, String> ArcGISFIDtoPGBusNum = new HashMap<>(); // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PGBusNumtoArcGISFID = new HashMap<>(); // reverse mapping BusNum to ArcGIS FID
	public static Map<String, String> SubstationtoPWBusNum = new HashMap<>(); // Maps substation number to HV and LV bus numbers
	public static Map<String, String> PWBusNumtoSubstation = new HashMap<>(); // reverse mapping
	public static Map<String, Integer> BusNumtoXPoint = new HashMap<>(); // Maps BusNum to X point for the parametrised model
	public static Map<Integer, String> XPointtoBusNum = new HashMap<>(); // reverse mapping

	public static Map<String, String> APSimNamtoOBJECTID = new HashMap<>(); // ZL-151125 Maps the old Chemical Process icon to the aspen plus model
	public static Map<Integer, String> OBJECTIDtoMXNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the mixer in chemical plant 
	public static Map<Integer, String> OBJECTIDtoMXB3 = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the mixer in chemical plant
	public static Map<Integer, String> OBJECTIDtoHXNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the heat exchanger in chemical plant
	public static Map<Integer, String> OBJECTIDtoHXB3 = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the heat exchanger in chemical plant
	public static Map<Integer, String> OBJECTIDtoCRNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the reactor in chemical plant
	public static Map<Integer, String> OBJECTIDtoCRNumB1 = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the reactor in chemical plant
	public static Map<Integer, String> OBJECTIDtoSPNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the separator in chemical plant
	public static Map<Integer, String> OBJECTIDtoDCNum = new HashMap<>(); // ZL-160114 Maps ArcGIS OBJECTID to the decanter in chemical plant
	public static Map<Integer, String> OBJECTIDtoRadF = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the RadFrac
	public static Map<Integer, String> OBJECTIDtoReactor = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the Reactor
	public static Map<Integer, String> CPIDtoMatLine = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the Reactor
	public static Map<Integer, String> OBJECTIDtoWLB3 = new HashMap<>(); 	//Maps ArcGIS OBJECTID to the Reactor
	
	
	public static Map<Integer, String> OBJECTIDtoHXB2 = new HashMap<>();
	public static Map<Integer, String> OBJECTIDtoHXB2in = new HashMap<>();
	
	public static Map<String, String> OBJECTIDtoMaterialL = new HashMap<>(); // Maps ArcGIS OBJECTID to the Material Lines in chemical plant
	public static Map<String, String> MaterialLtoOBJECTID = new HashMap<>(); // Maps ArcGIS OBJECTID to the Material Lines in chemical plant

	public static String INCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/IN.CSV");
	public static String LPPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/LPPIN.CSV");
	public static String LPQIN = new String("C:/apache-tomcat-8.0.24/webapps/input/LPQIN.CSV");
	public static String PGPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/PGPIN.CSV");
	public static String PGQIN = new String("C:/apache-tomcat-8.0.24/webapps/input/PGQIN.CSV");
	public static File fUHT1_MVR = new File("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MVR.txt");
	public static File fUHT1_MW = new File("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MW.txt");
	public static File fUHT2_MVR = new File("C:/apache-tomcat-8.0.24/webapps/input/UHT2_MVR.txt");
	public static File fUHT2_MW = new File("C:/apache-tomcat-8.0.24/webapps/input/UHT2_MW.txt");
	public static File fUHT3_MVR = new File("C:/apache-tomcat-8.0.24/webapps/input/UHT3_MVR.txt");
	public static File fUHT3_MW = new File("C:/apache-tomcat-8.0.24/webapps/input/UHT3_MW.txt");

	public static File TestModel = new File("C:/apache-tomcat-8.0.24/webapps/input/TestModel.txt"); // ZL-20151229 testing model for extracting the x-value from the data base

	public static String UHT1_MVRconst = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MVRconst.CSV");
	public static String UHT1_MVRcoeff = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MVRcoeff.CSV");
	public static String UHT1_MVRresult = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MVRresult.CSV");

	public static String UHT1_MVR = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MVR.CSV");
	public static String UHT1_MW = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT1_MW.CSV");
	public static String UHT2_MVR = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT2_MVR.CSV");
	public static String UHT2_MW = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT2_MW.CSV");
	public static String UHT3_MVR = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT3_MVR.CSV");
	public static String UHT3_MW = new String("C:/apache-tomcat-8.0.24/webapps/input/UHT3_MW.CSV");

	// public static String LGIN = new
	// String("C:/apache-tomcat-8.0.24/webapps/input/test.CSV");
	public static String BUSCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/BUS.CSV");
	public static String LPOUT = new String("C:/apache-tomcat-8.0.24/webapps/output/LPOUT.CSV");
	public static String UHTOUT = new String("C:/apache-tomcat-8.0.24/webapps/output/UHTOUT.CSV");
	public static String EHTOUT = new String("C:/apache-tomcat-8.0.24/webapps/output/EHTOUT.CSV");
	public static String PGOUT = new String("C:/apache-tomcat-8.0.24/webapps/output/PGOUT.CSV");
	public static String flag2CSV = new String("C:/apache-tomcat-8.0.24/webapps/input/flag2.CSV"); // (mjk, 151115) to see how far runPowerWorld() is being executed
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReq.CSV"); // (mjk, 151115) differentiating function calls "Run PowerWorld" and "Run parameterised PW"
	
	public static String CSVtest = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/CSVtest.CSV");
//	public static String httpReqCSV1 = new String("ftp://caresremote1.dyndns.org/home/OPAL-RT/J-Park%20Simulator/NUS%20Folder/in.CSV"); 
	
//	public static String APINCSV = new String( "C:/apache-tomcat-8.0.24/webapps/ROOT/APIN.CSV"); // ZL-151124 input CSV for aspen plus
//	public static String APOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APOUT.CSV"); // ZL-151124 output CSV from Aspen plus
	public static String PrAPMLin = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPMXin.CSV");
	public static String PrAPHXin = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPHXin.CSV");
	public static String PrAPCRin = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPCRXin.CSV");
	public static String PrAPSPin = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPSPXin.CSV");
	public static String PrAPDCin = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPDCXin.CSV");
	public static String APINsub = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APINsub.CSV");

	public static String OPALRTINCSV = new String("C:/ftptest/transmit_files_to_NTU/OPALRTin.CSV");
//	public static String OPALRTINCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/OPALRTin.CSV");
	
	public static String APPWIN = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APPWIN.CSV"); // input CSV file for the combined AP+PW model

	public static String XMLTest = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/test.xml");

	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/PWrun.pyw"); // ensure that python environment variable is set to python34
//	public static String runPythonCommandAP = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APrun.pyw"); // ZL-151124  python script calling Aspen Plus model
	public static String runPythonCommandAPPW = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APPWrun.pyw"); // python script for the AP+PW button, run AspenPlus and PowerWorld model sequentially

	public static String runPythonCommandAP = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APrun_modified.pyw"); // ZL-151124  python script calling Aspen Plus model
	public static String runPythonCommandAPWHR = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APrunWHR.pyw"); // ZL-151124  python script calling Aspen Plus With Heat Recovery model
	public static String APINCSV = new String( "C:/apache-tomcat-8.0.24/webapps/ROOT/APIN.CSV"); // ZL-151124 input CSV for aspen plus
	public static String APOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APOUT.CSV"); // ZL-151124 output CSV from Aspen plus
	public static String APWHRINCSV = new String( "C:/apache-tomcat-8.0.24/webapps/ROOT/APWHRIN.CSV"); // ZL-151124 input CSV for aspen plus with heat recovery
	public static String APWHROUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APWHROUT.CSV"); // ZL-151124 output CSV from Aspen plus with heat recovery
	
	public static String XVALUE = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUE.CSV"); // ZL-151217 x Value required by pr PW model

	public static String Sim1 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim1");
	public static String APPWSim = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APPWSim");
	
	public static String PrPWOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUT.CSV"); // output CSV file from the pr power world model
	public static String PrAPOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPOUT.CSV"); // output CSV file from the pr aspen plus model
	public static String PrAPPWOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPPWOUT.CSV"); // output CSV file from the pr aspen plus model
	public static String APPWOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APPWOUT.CSV"); // output CSV file from the pr aspen plus model
	public static String QueryCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/QueryOut.CSV"); 
	
	
	public static String HC_Sim = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/simtest1");
	public static String PrAPHCOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPHCoutCSV.CSV");
	public static String APHCINCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPHCinCSV.CSV");
	
	public PWServlet() {
		super();
		//OBJECTIDtoHXB2.put(41, "10E01B2");   //Hydrocracking
		OBJECTIDtoHXB2in.put(29, "E-701"); //Hydrocracking
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
		ArcGISFIDtoPWBusNum.put("140", "194"); // 03096L
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

		BusNumtoXPoint.put("31", 0);
		BusNumtoXPoint.put("32", 1);
		BusNumtoXPoint.put("34", 2);
		BusNumtoXPoint.put("35", 3);
		BusNumtoXPoint.put("36", 4);
		BusNumtoXPoint.put("38", 5);
		BusNumtoXPoint.put("39", 6);
		BusNumtoXPoint.put("40", 7);
		BusNumtoXPoint.put("41", 8);
		BusNumtoXPoint.put("45", 9);
		BusNumtoXPoint.put("46", 10);
		BusNumtoXPoint.put("47", 11);
		BusNumtoXPoint.put("49", 12);
		BusNumtoXPoint.put("52", 13);
		BusNumtoXPoint.put("53", 14);
		BusNumtoXPoint.put("54", 15);
		BusNumtoXPoint.put("57", 16);
		BusNumtoXPoint.put("59", 17);
		BusNumtoXPoint.put("60", 18);
		BusNumtoXPoint.put("62", 19);
		BusNumtoXPoint.put("63", 20);
		BusNumtoXPoint.put("64", 21);
		BusNumtoXPoint.put("65", 22);
		BusNumtoXPoint.put("66", 23);
		BusNumtoXPoint.put("67", 24);
		BusNumtoXPoint.put("70", 25);
		BusNumtoXPoint.put("71", 26);
		BusNumtoXPoint.put("73", 27);
		BusNumtoXPoint.put("75", 28);
		BusNumtoXPoint.put("76", 29);
		BusNumtoXPoint.put("77", 30);
		BusNumtoXPoint.put("80", 31);
		BusNumtoXPoint.put("82", 32);
		BusNumtoXPoint.put("83", 33);
		BusNumtoXPoint.put("84", 34);
		BusNumtoXPoint.put("85", 35);
		BusNumtoXPoint.put("86", 36);
		BusNumtoXPoint.put("88", 37);
		BusNumtoXPoint.put("89", 38);
		BusNumtoXPoint.put("90", 39);
		BusNumtoXPoint.put("93", 40);
		BusNumtoXPoint.put("94", 41);
		BusNumtoXPoint.put("95", 42);
		BusNumtoXPoint.put("97", 43);
		BusNumtoXPoint.put("98", 44);
		BusNumtoXPoint.put("99", 45);
		BusNumtoXPoint.put("100", 46);
		BusNumtoXPoint.put("101", 47);
		BusNumtoXPoint.put("102", 48);
		BusNumtoXPoint.put("103", 49);
		BusNumtoXPoint.put("104", 50);
		BusNumtoXPoint.put("105", 51);
		BusNumtoXPoint.put("106", 52);
		BusNumtoXPoint.put("107", 53);
		BusNumtoXPoint.put("108", 54);
		BusNumtoXPoint.put("109", 55);
		BusNumtoXPoint.put("110", 56);
		BusNumtoXPoint.put("111", 57);
		BusNumtoXPoint.put("113", 58);
		BusNumtoXPoint.put("114", 59);
		BusNumtoXPoint.put("115", 60);
		BusNumtoXPoint.put("116", 61);
		BusNumtoXPoint.put("117", 62);
		BusNumtoXPoint.put("118", 63);
		BusNumtoXPoint.put("119", 64);
		BusNumtoXPoint.put("120", 65);
		BusNumtoXPoint.put("121", 66);
		BusNumtoXPoint.put("122", 67);
		BusNumtoXPoint.put("123", 68);
		BusNumtoXPoint.put("124", 69);
		BusNumtoXPoint.put("125", 70);
		BusNumtoXPoint.put("126", 71);
		BusNumtoXPoint.put("127", 72);
		BusNumtoXPoint.put("128", 73);
		BusNumtoXPoint.put("130", 74);
		BusNumtoXPoint.put("131", 75);
		BusNumtoXPoint.put("132", 76);
		BusNumtoXPoint.put("133", 77);
		BusNumtoXPoint.put("134", 78);
		BusNumtoXPoint.put("136", 79);
		BusNumtoXPoint.put("137", 80);
		BusNumtoXPoint.put("138", 81);
		BusNumtoXPoint.put("147", 82);
		BusNumtoXPoint.put("148", 83);
		BusNumtoXPoint.put("150", 84);
		BusNumtoXPoint.put("153", 85);
		BusNumtoXPoint.put("156", 86);
		BusNumtoXPoint.put("159", 87);
		BusNumtoXPoint.put("162", 88);
		BusNumtoXPoint.put("163", 89);
		BusNumtoXPoint.put("175", 90);
		BusNumtoXPoint.put("177", 91);
		BusNumtoXPoint.put("178", 92);
		BusNumtoXPoint.put("180", 93);
		BusNumtoXPoint.put("182", 94);
		BusNumtoXPoint.put("184", 95);
		BusNumtoXPoint.put("187", 96);
		BusNumtoXPoint.put("191", 97);
		BusNumtoXPoint.put("192", 98);
		BusNumtoXPoint.put("193", 99);
		BusNumtoXPoint.put("197", 100);
		BusNumtoXPoint.put("200", 101);
		BusNumtoXPoint.put("202", 102);
		BusNumtoXPoint.put("203", 103);
		BusNumtoXPoint.put("205", 104);
		BusNumtoXPoint.put("206", 105);
		BusNumtoXPoint.put("207", 106);
		BusNumtoXPoint.put("208", 107);
		
		for (Map.Entry<String, Integer> entry : BusNumtoXPoint.entrySet()) { // reverse
																				// mapping
			XPointtoBusNum.put(entry.getValue(), entry.getKey());
		}

		// first bus number is high voltage bus, second bus number is low
		// voltage bus
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
			PWBusNumtoSubstation.put(entry.getValue(), entry.getKey()
					.substring(3, 4)); // remove all characters from key, only
										// need FID
		}

		// FID to BusNum map for power generators
		ArcGISFIDtoPGBusNum.put("6", "1"); // Singapore utility grid
		ArcGISFIDtoPGBusNum.put("1", "5"); // Seraya 1
		ArcGISFIDtoPGBusNum.put("2", "6"); // Seraya 2
		ArcGISFIDtoPGBusNum.put("3", "7"); // Seraya 3
		ArcGISFIDtoPGBusNum.put("4", "8"); // SembCorp Cogen
		ArcGISFIDtoPGBusNum.put("5", "9"); // Keppel Merlimau

		for (Map.Entry<String, String> entry : ArcGISFIDtoPGBusNum.entrySet()) { // reverse
																					// mapping
			PGBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
		}

		//
		
		OBJECTIDtoMXNum.put(1, "M-101");   //Biodiesel 1
		OBJECTIDtoMXNum.put(2, "M-102");   //Biodiesel 1
		OBJECTIDtoMXNum.put(3, "M-103");   //Biodiesel 1
		OBJECTIDtoMXNum.put(4, "M-502");     //air liquide
		OBJECTIDtoMXNum.put(5, "M-504");     //air liquide
		OBJECTIDtoMXNum.put(6, "M-501");     //air liquide
		OBJECTIDtoMXNum.put(7, "M-503");     //air liquide
		OBJECTIDtoMXNum.put(8, "M-701");     //hydrocracking
		OBJECTIDtoMXNum.put(9, "M-201");   //Biodiesel 2
		OBJECTIDtoMXNum.put(10, "M-203");  //Biodiesel 2
		OBJECTIDtoMXNum.put(11, "M-202");  //Biodiesel 2
		OBJECTIDtoMXNum.put(12, "M-209");//Biodiesel 2
		OBJECTIDtoMXNum.put(13, "M-401");  //Zeon

		OBJECTIDtoMXNum.put(15, "M-602");   //lanxess
		OBJECTIDtoMXNum.put(16, "M-108");  //lanxess
		OBJECTIDtoMXNum.put(17, "M-208");    //Biodiesel1
		OBJECTIDtoMXNum.put(18, "M-301");  //Biodiesel2
		OBJECTIDtoMXNum.put(19, "M-302");    //Biodiesel3
		OBJECTIDtoMXNum.put(20, "M-303");    //Biodiesel3
		OBJECTIDtoMXNum.put(21, "M-601");    //Biodiesel3
		OBJECTIDtoMXNum.put(21, "M-402");  
		
		OBJECTIDtoMXB3.put(18, "M-301");  //Biodiesel2
		OBJECTIDtoMXB3.put(19, "M-302");    //Biodiesel3
		OBJECTIDtoMXB3.put(20, "M-303");    //Biodiesel3		
		
		OBJECTIDtoHXNum.put(1, "E-107"); //Biodiesel1
		OBJECTIDtoHXNum.put(2, "E-102");   //Biodiesel1
		OBJECTIDtoHXNum.put(3, "E-101");   //Biodiesel1
		OBJECTIDtoHXNum.put(4, "E-106");   //Biodiesel1
		OBJECTIDtoHXNum.put(5, "E-103");   //Biodiesel1
		OBJECTIDtoHXNum.put(6, "E-104");   //Biodiesel1
		OBJECTIDtoHXNum.put(7, "E-105");   //Biodiesel1
		OBJECTIDtoHXNum.put(8, "E-202");   //Biodiesel2
		OBJECTIDtoHXNum.put(9, "E-203");   //Biodiesel2
		OBJECTIDtoHXNum.put(10, "E-204");  //Biodiesel2
		OBJECTIDtoHXNum.put(11, "E-205");  //Biodiesel2
		OBJECTIDtoHXNum.put(12, "E-401");   //Zeon
		OBJECTIDtoHXNum.put(13, "E-403");   //Zeon
		OBJECTIDtoHXNum.put(14, "HE001");   //Evonik
		OBJECTIDtoHXNum.put(15, "HE002");   //Evonik
		OBJECTIDtoHXNum.put(16, "HE003");   //Evonik
		OBJECTIDtoHXNum.put(17, "HE004");   //Evonik
		OBJECTIDtoHXNum.put(18, "HE005");   //Evonik
		OBJECTIDtoHXNum.put(19, "E-603");     //hydrocracking
		OBJECTIDtoHXNum.put(20, "E-404");  //lanxess
		OBJECTIDtoHXNum.put(21, "E-405");    //zeon
		OBJECTIDtoHXNum.put(22, "E-604");   //zeon
		OBJECTIDtoHXNum.put(23, "E-606");  //lanxess
		OBJECTIDtoHXNum.put(24, "E-605");  //lanxess
		OBJECTIDtoHXNum.put(25, "E-402");  //lanxess
		OBJECTIDtoHXNum.put(26, "E-608");//lanxess
		OBJECTIDtoHXNum.put(27, "E-607");     //zeon
		OBJECTIDtoHXNum.put(28, "E-609");     //lanxess
		OBJECTIDtoHXNum.put(29, "E-701");     //lanxess
		OBJECTIDtoHXNum.put(30, "E-702");     //lanxess
		OBJECTIDtoHXNum.put(31, "Eva2");     //hydrocracking
		OBJECTIDtoHXNum.put(32, "Cond2");     //hydrocracking
		OBJECTIDtoHXNum.put(33, "E-109");    //cogen
		OBJECTIDtoHXNum.put(34, "E-110");   //cogen
		OBJECTIDtoHXNum.put(35, "E-112");   //Biodiesel1
		OBJECTIDtoHXNum.put(36, "E-113"); //Biodiesel1
		OBJECTIDtoHXNum.put(37, "E-111"); //Biodiesel1
		OBJECTIDtoHXNum.put(38, "E-206"); //Biodiesel1
		OBJECTIDtoHXNum.put(39, "E-201");   //Biodiesel1
		OBJECTIDtoHXNum.put(40, "E-213");   //Biodiesel2
		OBJECTIDtoHXNum.put(41, "E-212");   //Biodiesel2
		OBJECTIDtoHXNum.put(42, "E-207"); //Biodiesel2
		OBJECTIDtoHXNum.put(43, "E-210"); //Biodiesel2
		OBJECTIDtoHXNum.put(44, "E-209"); //Biodiesel2
		OBJECTIDtoHXNum.put(45, "E-211"); //Biodiesel2
		OBJECTIDtoHXNum.put(46, "E-208");   //Biodiesel2
		OBJECTIDtoHXNum.put(47, "E-214");   //Biodiesel2
		OBJECTIDtoHXNum.put(48, "E-304"); //Biodiesel2
		OBJECTIDtoHXNum.put(49, "E-303");  //Biodiesel2
		OBJECTIDtoHXNum.put(50, "E-305");   //Biodiesel3
		OBJECTIDtoHXNum.put(51, "E-302");   //Biodiesel3
		OBJECTIDtoHXNum.put(52, "E-306");   //Biodiesel3
		OBJECTIDtoHXNum.put(53, "E-301");   //Biodiesel3
		OBJECTIDtoHXNum.put(54, "E-307");   //Biodiesel3
		OBJECTIDtoHXNum.put(55, "E-602");   //Biodiesel3
		OBJECTIDtoHXNum.put(56, "E-601"); //Biodiesel3
		
		OBJECTIDtoHXB3.put(48, "E-304"); //Biodiesel2
		OBJECTIDtoHXB3.put(49, "E-303");  //Biodiesel2
		OBJECTIDtoHXB3.put(50, "E-305");   //Biodiesel3
		OBJECTIDtoHXB3.put(51, "E-302");   //Biodiesel3
		OBJECTIDtoHXB3.put(52, "E-306");   //Biodiesel3
		OBJECTIDtoHXB3.put(53, "E-301");   //Biodiesel3
		OBJECTIDtoHXB3.put(54, "E-307");   //Biodiesel3
		
		CPIDtoMatLine.put(60, "3-1");
		CPIDtoMatLine.put(61, "3-2");
		CPIDtoMatLine.put(62, "3-23"); //use wrong one instead 83 as the true value
		OBJECTIDtoWLB3.put(42, "FW-302");
		

		OBJECTIDtoCRNumB1.put(1, "10D01B1"); //Biodiesel1
		OBJECTIDtoCRNumB1.put(2, "10D03B1"); //Biodiesel1
		
		OBJECTIDtoCRNum.put(1, "R-101"); //Biodiesel1
		OBJECTIDtoCRNum.put(2, "R-102"); //Biodiesel1
		OBJECTIDtoCRNum.put(3, "R-103"); //Biodiesel1
		OBJECTIDtoCRNum.put(4, "R-1"); //EVONIK
		OBJECTIDtoCRNum.put(5, "R-2"); //EVONIK
		OBJECTIDtoCRNum.put(6, "R-3"); //EVONIK
		OBJECTIDtoCRNum.put(7, "R-701"); //HYDROCRACKING
		OBJECTIDtoCRNum.put(8, "R-201"); //Biodiesel2
		OBJECTIDtoCRNum.put(9, "R-202"); //Biodiesel2
		OBJECTIDtoCRNum.put(10, "R-401"); //ZEON
		OBJECTIDtoCRNum.put(11, "R-702"); //HYDROCRACKING
		OBJECTIDtoCRNum.put(12, "R-601"); //LANXESS
		OBJECTIDtoCRNum.put(13, "R-104"); //Biodiesel1
		OBJECTIDtoCRNum.put(14, "R-105"); //Biodiesel1
		OBJECTIDtoCRNum.put(15, "R-203"); //Biodiesel2
		OBJECTIDtoCRNum.put(16, "R-204"); //Biodiesel2
		OBJECTIDtoCRNum.put(17, "R-205"); //Biodiesel2
		OBJECTIDtoCRNum.put(18, "R-302"); //Biodiesel3
		OBJECTIDtoCRNum.put(19, "R-301"); //Biodiesel3
		OBJECTIDtoCRNum.put(20, "R-303"); //Biodiesel3

		OBJECTIDtoSPNum.put(1, "10D02");

		OBJECTIDtoDCNum.put(1, "10D02D");
		
		OBJECTIDtoRadF.put(1, "T-103"); //Biodiesel1
		OBJECTIDtoRadF.put(2, "T-102"); //Biodiesel1
		OBJECTIDtoRadF.put(3, "T-501");
		OBJECTIDtoRadF.put(4, "T-701");  
		OBJECTIDtoRadF.put(5, "T-202"); //Biodiesel2
		OBJECTIDtoRadF.put(6, "T-203"); //Biodiesel2
		OBJECTIDtoRadF.put(7, "T-502"); //Biodiesel1		
		OBJECTIDtoRadF.put(8, "T-503"); //Biodiesel1
		OBJECTIDtoRadF.put(9, "T-504");
		OBJECTIDtoRadF.put(10, "T-302"); //Biodiesel3
		OBJECTIDtoRadF.put(11, "T-303"); //Biodiesel3

		
		/*OBJECTIDtoReactor.put(1, "10D01");
		OBJECTIDtoReactor.put(2, "10D03");*/
		
		OBJECTIDtoReactor.put(1, "R-101");//Biodiesel1
		OBJECTIDtoReactor.put(2, "R-102");//Biodiesel1
		OBJECTIDtoReactor.put(3, "R-103");//Biodiesel1
		OBJECTIDtoReactor.put(4, "R-1");//EVONIK
		OBJECTIDtoReactor.put(5, "R-2");//EVONIK
		OBJECTIDtoReactor.put(6, "R-3");//EVONIK
		OBJECTIDtoReactor.put(7, "R-701");//HYDROCRACKING
		OBJECTIDtoReactor.put(8, "R-201");//Biodiesel2
		OBJECTIDtoReactor.put(9, "R-202");//Biodiesel2
		OBJECTIDtoReactor.put(10, "R-401");//ZEON
		OBJECTIDtoReactor.put(11, "R-702");//HYDROCRACKING
		OBJECTIDtoReactor.put(12, "R-601");//LANXESS
		OBJECTIDtoReactor.put(13, "R-104");//Biodiesel1
		OBJECTIDtoReactor.put(14, "R-105");//Biodiesel1
		OBJECTIDtoReactor.put(15, "R-203");//Biodiesel2
		OBJECTIDtoReactor.put(16, "R-204");//Biodiesel2
		OBJECTIDtoReactor.put(17, "R-205");//Biodiesel2
		OBJECTIDtoReactor.put(18, "R-302");//Biodiesel3
		OBJECTIDtoReactor.put(19, "R-301");//Biodiesel3
		OBJECTIDtoReactor.put(20, "R-303");//Biodiesel3			
/**
		OBJECTIDtoMaterialL.put("104", "MeOH");
		OBJECTIDtoMaterialL.put("126", "OIL");
		OBJECTIDtoMaterialL.put("100", "FINALPRD");
**/
		OBJECTIDtogaslinenum.put(7, "FUEL SUPPLYB1"); //Biodiesel1
		OBJECTIDtogaslinenum.put(20, "FUEL SUPPLYB2"); //Biodiesel2
		
		for (Map.Entry<String, String> entry : OBJECTIDtoMaterialL.entrySet()) { // reverse mapping
			MaterialLtoOBJECTID.put(entry.getValue(), entry.getKey());
		}

	} // of constructor

	/**this is the main method of this servlet which is used to listen to HttpRequest, excute the corresponding model and send back HttpRsponse */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		/**reconstructing editStack from request string so that the information can be passed on to the relevant methods*/
		ArrayList<String[]> editStack = new ArrayList<String[]>(); 
		String[] layers = request.getParameter("layers").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(",");     // OBJECTID indicating parameter of which unit in chemical process has been changed
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // (mjk, 151115) adding flag indicating which  function has been called: PowerWorld,  parameterised  PW, AspenPlus, parameterised AP
		String[] QueryT = request.getParameter("QueryT").split(",");

		for (int i = 0; i < layers.length; i++) {
			editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i], QueryT[i]});
		}
 
		/**check wheather the httpRequst has been correctly recieved */
		FileWriter flag1 = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
		flag1 = new FileWriter(httpReqCSV);
		flag1.append("layers=" + layers[0]);
		flag1.append(", OBJECTIDs=" + OBJECTIDs[0]);
		flag1.append(", appCallFlag=" + appCallFlag[0]);
		flag1.append(", QueryT=" + QueryT[0]);
		flag1.flush();
		flag1.close(); // (mjk, 151115) writing this file works fine.
				
		/**the following part of code distinguishes which functionality of the JParkSimulator has been called, and then provides the corresponding service by evaluating the associated model */
		switch (appCallFlag[0]) {		
		case "AP":                 //when Aspen plus model was called, the following command will be executed
			System.out.println(appCallFlag[0] + " button was pressed! (PWServlet)");
			start_time = System.currentTimeMillis();  // start a timer in order to track the evaluating time
//			AspenPlusModels.runAspenPlus(editStack);  // evaluate aspen plus model
			PCrunAspenPlus(editStack);
			end_time = System.currentTimeMillis();
			System.out.println("runAspenPlus takes: "+(end_time-start_time)); // print out evaluating time to consol		
			break;
			
		case("APHR"):             //when Aspen plus model with waste heat recovery was called, the following command will be executed
			System.out.println(appCallFlag[0] + " button was pressed! (PWServlet)");
		    start_time = System.currentTimeMillis(); // start a timer in order to track the evaluating time
//		    AspenPlusModels.runAspenPlusWithWasteHeatRecovery(editStack);                   // PC (Run Aspen Plus with Waste Heat Recovery model when run AspenPlus With Heat Recovery Button (APHR) is pressed)
		    PCrunAspenPlusWithWasteHeatRecovery(editStack);
		    end_time = System.currentTimeMillis();
		    System.out.println("runAspenPlus takes: "+(end_time-start_time));			
		    break;
		    
		case "PrAP":       //when parameterised Aspen plus model with waste heat recovery was called, the following command will be executed
			System.out.println(appCallFlag[0] + " button was pressed! (PWServlet)");
			start_time = System.currentTimeMillis();
//			PrAspenPlusModels.runPrAspenPlusWOWHR(editStack);
			end_time = System.currentTimeMillis();
			System.out.println("runPrAspenPlus takes: "+(end_time-start_time));
			break;
			
		case "PW":
			System.out.println(appCallFlag[0] + " button was pressed! (doPOST)");
//			start_time = System.currentTimeMillis();
			runPowerWorld(editStack);                                                    // run Power World model when run PowerWorld Button was pressed
//			end_time = System.currentTimeMillis();
//			System.out.println("runPowerWorld takes: "+(end_time-start_time));
			break;
		case "PWPr":
			System.out.println(appCallFlag[0] + " button was pressed! (doPOST)");
			start_time = System.currentTimeMillis();
			runPrPowerWorld(editStack); 						                          // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed		
			end_time = System.currentTimeMillis();
			System.out.println("runPrPowerWorld takes: "+(end_time-start_time));
			break;			
		case "APPW":
			System.out.println(appCallFlag[0] + " button was pressed! (doPOST)");
			start_time = System.currentTimeMillis();
			runAspenPlusandPowerWorld(editStack);
			end_time = System.currentTimeMillis();
			System.out.println("runAPPW takes: "+(end_time-start_time));
			break;
		case "PrAPPW":
			System.out.println(appCallFlag[0] + " button was pressed! (doPOST)");
			start_time = System.currentTimeMillis();
			runPrAspenPlusandPowerWorld(editStack);
			end_time = System.currentTimeMillis();
			System.out.println("runPrAPPW takes: "+(end_time-start_time));
			break;				
		case ("PrAPO"):                                                                  //run parameterised aspen plus with OntoCAPE
			System.out.println(appCallFlag[0] + " Button was pressed! (doPOST)");
		    start_time = System.currentTimeMillis();
		    runParameterisedAPwithOntoCAPE(editStack);	
		    end_time = System.currentTimeMillis();
			System.out.println("runPrAPO takes: "+(end_time-start_time));
		    break;	
		case ("PrAPHC"):                                                                  //run parameterised aspen plus with OntoCAPE
			System.out.println(appCallFlag[0] + " Button was pressed! (doPOST)");
		    start_time = System.currentTimeMillis();
		    runParameterisedAPhydrocracking(editStack);	
		    end_time = System.currentTimeMillis();
			System.out.println("runPrAPHC takes: "+(end_time-start_time));
		    break;
		case ("Query"):					
            System.out.println("start extracting information.");
//		    final String GISInformation=(BiodieselPlantQuery.PerformQuery(QueryT[0])).toString();
//		    final String GISInformation=PerformQuery(QueryT[0]).toString();
		    System.out.println(QueryT[0]);
		   // final String GISInformation=InformationQuery.Query(QueryT[0]);
		   // System.out.println(GISInformation);
		    //response.setContentLength(GISInformation.length());
		    //response.getOutputStream().write(GISInformation.getBytes());		    
		    response.getOutputStream().flush();
		    response.getOutputStream().close();
		    System.out.println("Success!");
		    break;
		case ("OPALRT"):                                                               //excute this part of code when OPAL-RT was called
			System.out.println("OPALRT was called");
		   // writeOPALRTCSV();                                                          //write the input .csv file for OPAL-RT to a local file
		   // uploadCSV();                                                               //upload the input .csv file for OPAL-RT to the cloud
			callOPALRT ();
			
			//add the commands for receiveing the signal from NTU Servlet here;
			downloadCSV();
			System.out.println("now try uploading csv");
//			new PWServlet().killProcess();
//			Runtime.getRuntime().exec("cmd /c start   D:\\send_from_NUS.bat");
		    
		} 
	} // of doPost()

	//allows manual updating using a browser e.g. entering  http://localhost:8080/PWServlet/?layers=Load_Points&FIDs=103
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ArrayList<String[]> editStack = new ArrayList<String[]>();
		String[] layers = request.getParameter("layers").split(",");
		// String[] FIDs = request.getParameter("OBJECTIDs").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(","); // ZL-151209 OBJECTID indicating parameter of which unit in chemical process has been changed
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // (mjk, 151115) adding flag indicating which function has been called: PowerWorld, parameterised PW, AspenPlus, parameterised AP

		for (int i = 0; i < layers.length; i++) {editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i] }); // Here, "editStack" for only one layer modification looks like this: [Load_Points,103,PW]
		}
		switch (appCallFlag[0]) {
		case "AP":
			System.out.println(appCallFlag[0] + " button was pressed!");
			runAspenPlus(editStack); // run Aspen Plus model when run AspenPlus Button was pressed
			break;
		case "PrAP": // if PrAP button was pressed, then the following action will be taken
			System.out.println(appCallFlag[0] + " button was pressed!");
//			runPrAspenPlus(editStack); // run Parametrised Aspen Plus model when run Parametrised AspenPlus Button was pressed
			runPrAspenPlusandPowerWorld(editStack);
			break;
		case "PW":
			System.out.println(appCallFlag[0] + " button was pressed!");
			runPowerWorld(editStack); // run Power World model when run PowerWorld Button was pressed
			break;
		case "PWPr":
			System.out.println(appCallFlag[0] + " button was pressed!");
			runPrPowerWorld(editStack); // run Parametrised Power world model when run Parametrised PowerWorld Button was pressed
			break;
		case "APPW":
			System.out.println(appCallFlag[0] + " button was pressed!");
			runAspenPlusandPowerWorld(editStack);
			                                                                      //updating the output of APPW to JPS
			break;
		case "PrAPPW":
			System.out.println(appCallFlag[0] + " button was pressed!");
			runPrAspenPlusandPowerWorld(editStack);
			break;
		} // ZL-151126
	}

public void runParameterisedAPhydrocracking(ArrayList<String[]> editStack) {
		
//		String appCallFlag = null;
//		appCallFlag = editStack.get(0)[2];                                               // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)
		List<Double> xRow = new ArrayList<>();                                            // extra arraylist to collect the x-value required as input to the pr aspen plus model
		List<List<Double>> xData = new ArrayList<>(1);                                    // arraylist to
		List<List<Double>> yData; 
		
		start_time1 = System.currentTimeMillis();
		xRow=getAPHCInput(editStack);
		end_time1 = System.currentTimeMillis();
		xData.add(xRow); 
		
		String simDir = HC_Sim;
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {
			
			fileWriter = new FileWriter(PrAPHCOUTCSV);                                        // filewriter for the output of pr aspenplus model
            System.load("C:/apache-tomcat-8.0.24/webapps/HClib/MoDS_Java_API_0.1.dll");              //the MoDS API at use is version 0.1
			
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
		readPrAPHCCSV();
		end_time2 = System.currentTimeMillis();
		System.out.println("getAPHCInput takes: "+( end_time1-start_time1));
		System.out.println("readPrAPCSV takes: "+( end_time2-start_time2));
	}
	
	public ArrayList<Double> getAPHCInput(ArrayList<String[]> editStack){ 
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
						
		for (Integer key : OBJECTIDtoHXB2in.keySet()) {
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
			filewriterAPIN = new FileWriter(APHCINCSV); // to put the input values for the AspenPlus subset model
			filewriterAPIN.append("Hydro, VGO");
			filewriterAPIN.append("\n");

			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {
//41, 48						
						if (OBJECTIDtoHXB2in.get(i + 29).equals("E-701")) { // "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn3Qnt")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")));
//							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn3Qnt")))); // add the temperature of oil to xRow
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")))); // add the temperature of oil to xRow
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
	
	public void readPrAPHCCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrAPHCOUTCSV));
			fileReader.readLine();     // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable RadFracTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/RadFrac/FeatureServer", user, 0);
			RadFracTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			RadFracTable.initialize();
			System.out.println(RadFracTable.getStatus());
			RadFracTable.getInitializationError();
			
			/*GeodatabaseFeatureServiceTable HeaterCoolerTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer", user, 0);
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
			*/
			final CountDownLatch latch = new CountDownLatch(1);                                                                             // ZL-151207 handles one asynchronous processes, only continues  Thread when it reaches 0
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
					
			       latch.await();                                                                                                              // wait until all feature service tables are ready then continue
			       
			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
//				System.out.println("data= " + data);
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[6];

				//the following code is used for updating the flowrate of the FINALPRD to ArcGIS database
				for (int j = 3; j < 6; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoRadF.get(j + 1).equals("T-701")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> RadFracAttributes = RadFracTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						
						if (!data[2].trim().isEmpty()) {
							RadFracAttributes.put("MatOut2Qnt",Float.parseFloat(data[2].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("F_propane="+data[2]);
			 	       
						if (!data[0].trim().isEmpty()) {
							RadFracAttributes.put("MatOut4Qnt",Float.parseFloat(data[0].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("F_DIST="+data[0]);
			 	       
						if (!data[1].trim().isEmpty()) {
							RadFracAttributes.put("MatOut5Qnt",Float.parseFloat(data[1].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("F_NAPHTHA="+data[1]);
						
												
						if (!data[3].trim().isEmpty()) {
							RadFracAttributes.put("MatOut2_T",Float.parseFloat(data[3].trim()));
							RadFracAttributes.put("MatOut4_T",Float.parseFloat(data[3].trim()));
							RadFracAttributes.put("MatOut5_T",Float.parseFloat(data[3].trim()));// upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("T_out="+data[3]);
					    RadFracTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),RadFracAttributes);                          // update feature table locally
						break;
					}
				}										
			}
			RadFracTable.applyEdits(null);                                                                                        // commit local updates onto Server
			
			
			
			
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms");                     // tells how long it took to update
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
	
	
	/**when run AspenPlus for the first biodiesel plant (without waste heat recovery) was called, excute the following code: 
 * 1)get all the input state variables from ArcGIS database for the aspen plus model;
 * 2)call python script to run aspen plus model, and generate a output csv file;
 * 3)read from the csv file and update to ArcGIS database */
	public void PCrunAspenPlus(ArrayList<String[]> editStack) {
		PCgetAPInput(editStack);
		PCrunPyScript(editStack);                                                                  // call python script to run aspen plus 

		
		PCreadAPCSV();
	}
	/**when run AspenPlus for the second biodiesel plant (with waste heat recovery) was called, excute the following code: 
	 * 1)get all the input state variables from ArcGIS database for the aspen plus model;
	 * 2)call python script to run aspen plus model, and generate a output csv file;
	 * 3)read from the csv file and update to ArcGIS database */	
	public void PCrunAspenPlusWithWasteHeatRecovery(ArrayList<String[]> editStack) {
		PCgetAPWHRInput(editStack);
		PCrunPyScript(editStack);                                                                  // PC call python script to run aspen plus model with waste heat recovery
		PCreadAPWHRCSV();		
	}
	/**Collects all the input state variables for the aspen plus mode for biodiesel plant-2 (with waste heat recovery) */
	public ArrayList<Double> PCgetAPWHRInput(ArrayList<String[]> editStack){ 
		//ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); // additional ArrayList for mixer
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		ArrayList<Map<String, Object>> attributeslist_Reactor = new ArrayList<Map<String, Object>>(); // additional ArrayList for Reactor
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		
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
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);                               // arraylist to
//		ArrayList<ArrayList<Double>> yData;                                                    // output of the pr aspenplus model
		
		 		 
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
//							filewriterAPIN.append(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1Qnt")));
//							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_MX.get(i).get("MatIn2_P")));
//							filewriterAPIN.append(",");
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2Qnt")))); // add the feeding mole flowrate of methanol  to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_Reactor.get(i).get("MatIn1_T")))); // add the temperature of the feeding methanol flow to xRow							
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2_P")))); // add the pressure of the feeding methanol flow to xRow
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
	/**Reads the csv file generated by the aspen plus model for the first biodiesel plant (without waste heat recovery), and update some of the out put to ArcGIS database */
	public void PCreadAPCSV() {       // PC (Update outputs obtained by running Aspen Plus model to ArcGIS database)
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
	/**this part of code reads the csv file generated by the aspen plus model for the second biodiesel plant (with waste heat recovery), and update some of the out put to ArcGIS database */
	public void PCreadAPWHRCSV() {   // PC (Update outputs obtained by running Aspen Plus model with waste heat recovery to ArcGIS database)
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
	/**This part of code calls certain python script in order to run the corresponding aspen plus model when a certain service is called from GUI */
	public void PCrunPyScript(ArrayList<String[]> editStack) {
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
	/**this part of code collects all the input state variables for the aspen plus mode for biodiesel plant-2 (without waste heat recovery) */
	public ArrayList<Double> PCgetAPInput(ArrayList<String[]> editStack){  
		//ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); // additional ArrayList for mixer
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		ArrayList<Map<String, Object>> attributeslist_Reactor = new ArrayList<Map<String, Object>>(); // additional ArrayList for Reactor
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");		
		
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
	
	//the function below is designed to call the send_from_NUS.bat file from D drive, in order to upload the .csv input file generated for the OPAL-RT model to the cloud in NTU
	public void uploadCSV(){
		String strcmd = "cmd /c start  D:\\send_from_NUS.bat";
		Runtime rt = Runtime.getRuntime();
		Process ps = null;
		try{
			ps = rt.exec(strcmd);
		}catch (IOException e){
			e.printStackTrace();
			System.out.println(e);
		}
		try{
			ps.waitFor();
		}catch(InterruptedException e1){
			e1.printStackTrace();
		}
		int i = ps.exitValue();
		if(i==0){
			System.out.println("Success!");
		}else {
			System.out.println("failed!");
		}
		ps.destroy();
		ps = null;
	}

	//the function below is designed to call the download_from_NTU.bat file from D drive, in order to download the .csv output file generated by OPALRT model to this folder: C:\ftptest\download_from_NTU
	public void downloadCSV(){
		String strcmd = "cmd /c start  D:\\download_from_NTU.bat";
		Runtime rt = Runtime.getRuntime();
		Process ps = null;
		try{
			ps = rt.exec(strcmd);
		}catch (IOException e){
			e.printStackTrace();
			System.out.println(e);
		}
		
		try{
			ps.waitFor();
		}catch(InterruptedException e1){
			e1.printStackTrace();
		}

		int i = ps.exitValue();
		if(i==0){
			System.out.println("Success!");
		}else {
			System.out.println("failed!");
		}
		ps.destroy();
		ps = null;
	}
	
	/**This method is wrote to collect the input state variable for the RT-lab sim model*/
	public void writeOPALRTCSV(){		
			BufferedReader fileReader = null;
			FileWriter FWOPALRT = null;
						
			try {
				FWOPALRT = new FileWriter(OPALRTINCSV);
				
				String line = null;
				fileReader = new BufferedReader(new FileReader(APPWOUTCSV));
				fileReader.readLine();       // Read the CSV flie header to skip it
				
				while ((line = fileReader.readLine()) != null) {
					String[] data = line.split(",");
					System.out.println("data= " + data);
					
					FWOPALRT.append(data[1]);
					FWOPALRT.append(data[2]);
				}

				FWOPALRT.flush();
				FWOPALRT.close();
				
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
	/**This method sends http request to OPAL-RT machine residing in NTU, in order to run the certain python script and RT-lab sim model*/
	public void callOPALRT () {
		HttpURLConnection urlCon;
		OutputStreamWriter out;
		URL url;
						
		try{
			
//			url = new URL("http://caresremote1.dyndns.org:1700/OPARTServlet/");
//			url = new URL("http://14.100.26.181:1700/OPARTServlet/");
			url = new URL("http://caresremote1.dyndns.org/OPARTServlet/");
			urlCon = (HttpURLConnection) url.openConnection();
			urlCon.setRequestMethod("POST");
			urlCon.setDoOutput(true);
			out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
			
			StringBuilder outputString = new StringBuilder();
			outputString.append(URLEncoder.encode("layers", "UTF-8"));
			outputString.append("=");
			outputString.append(URLEncoder.encode(" ", "UTF-8"));
			outputString.append("&");
			outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
			outputString.append("=");
			outputString.append(URLEncoder.encode(" ", "UTF-8"));
			outputString.append("&");
			outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
			outputString.append("=");
			outputString.append(URLEncoder.encode("OPALRT", "UTF-8"));
			outputString.append("&");
			outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
			outputString.append("=");				
			outputString.append(URLEncoder.encode(" ", "UTF-8"));
			
			DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
			wr.writeBytes(outputString.toString());
			wr.flush();
			wr.close();
			
			if(urlCon.getResponseCode()==200){
				System.out.println("Message received!");
			} else {
				System.out.println( "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
						+ "\nPlease try testing your code again");
			}
			
			out.close();
			System.out.println(wr);
		}catch (IOException equery){
			equery.printStackTrace();
		}
	}
	
	public JSONArray PerformQuery(String queryT) throws IOException{
		final JSONArray arr=new JSONArray();
		String QueryTask = null;
		QueryTask = queryT;
		
		String uri = "File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant.owl";
		try{			
            OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
            System.out.println("QueryTask="+ QueryTask);
            if(QueryTask.equals("Pump")||QueryTask.equals("pump")){            	
            	                
                OWLIndividual Pump10P01Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_10P01");
                String Pump01LatValue = Pump10P01Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Pump10P101Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_10P01");
                String Pump01LogValue = Pump10P101Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
                
                arr.put(Pump01LatValue);
                arr.put(Pump01LogValue);
                
                OWLIndividual Pump002Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_P002");  
                String Pump002LatValue = Pump002Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Pump002Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_P002");
                String Pump002LogValue = Pump002Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Pump002LatValue);
                arr.put(Pump002LogValue);
                
                OWLIndividual Pump003Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_P003");  
                String Pump003LatValue = Pump003Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Pump003Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_P003");
                String Pump003LogValue = Pump003Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Pump003LatValue);
                arr.put(Pump003LogValue);
                
                OWLIndividual Pump004Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_P004");  
                String Pump004LatValue = Pump004Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Pump004Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_P004");
                String Pump004LogValue = Pump004Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Pump004LatValue);
                arr.put(Pump004LogValue);
                
                OWLIndividual Pump005Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_P005");  
                String Pump005LatValue = Pump005Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Pump005Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_P005");
                String Pump005LogValue = Pump005Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Pump005LatValue);
                arr.put(Pump005LogValue);
                
                System.out.println("GISInformation="+arr);        		
            }
            if(QueryTask.equals("Reactor")||QueryTask.equals("reactor")){       
            	               
            	OWLIndividual Reactor_10D01_Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_10D01");  
                String Reactor_10D01_LatValue = Reactor_10D01_Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Reactor_10D01_Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_10D01");
                String Reactor_10D01_LogValue = Reactor_10D01_Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Reactor_10D01_LatValue);
                arr.put(Reactor_10D01_LogValue);
                
                OWLIndividual Reactor_10D03_Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_10D03");
                String Reactor_10D03_LatValue = Reactor_10D03_Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Reactor_10D03_Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_10D03");
                String Reactor_10D03_LogValue = Reactor_10D03_Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
                
                arr.put(Reactor_10D03_LatValue);
                arr.put(Reactor_10D03_LogValue);
                
                OWLIndividual Reactor_combust1_Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_combust1");  
                String Reactor_combust1_LatValue = Reactor_combust1_Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Reactor_combust1_Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_combust1");
                String Reactor_combust1_LogValue = Reactor_combust1_Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Reactor_combust1_LatValue);
                arr.put(Reactor_combust1_LogValue);
                
                OWLIndividual Reactor_combust2_Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_combust2");  
                String Reactor_combust2_LatValue = Reactor_combust2_Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Reactor_combust2_Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_combust2");
                String Reactor_combust2_LogValue = Reactor_combust2_Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Reactor_combust2_LatValue);
                arr.put(Reactor_combust2_LogValue);
                
                OWLIndividual Reactor_combust3_Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_combust3");  
                String Reactor_combust3_LatValue = Reactor_combust3_Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual Reactor_combust3_Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_combust3");
                String Reactor_combust3_LogValue = Reactor_combust3_Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();	
                
                arr.put(Reactor_combust3_LatValue);
                arr.put(Reactor_combust3_LogValue);
                
                System.out.println("GISInformation="+arr);        		
            }
            
            if(QueryTask.equals("CO2 emission")||QueryTask.equals("co2 emission")){            	
                
                OWLIndividual CO2Emissionsite1Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_CO2EmissionPoint1_Temporary");
                String CO2Emissionsite1LatValue = CO2Emissionsite1Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual CO2Emissionsite1Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_CO2EmissionPoint1_Temporary");
                String CO2Emissionsite1LogValue = CO2Emissionsite1Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
                
                arr.put(CO2Emissionsite1LatValue);
                arr.put(CO2Emissionsite1LogValue);
                
                OWLIndividual CO2Emissionsite2Lat = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_x_CO2EmissionPoint2_Temporary");
                String CO2Emissionsite2LatValue = CO2Emissionsite2Lat.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();  
                OWLIndividual CO2Emissionsite2Log = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOf_y_CO2EmissionPoint2_Temporary");
                String CO2Emissionsite2LogValue = CO2Emissionsite2Log.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();
                
                arr.put(CO2Emissionsite2LatValue);
                arr.put(CO2Emissionsite2LogValue);
                
                System.out.println("GISInformation="+arr);        		
            }
            
		}catch (OntologyLoadException ex) {
	           Logger.getLogger(PWServlet.class.getName()).log(Level.SEVERE, null, ex); 
	           System.out.println(ex);
	        } 
		return arr;
	}
	
	public void runPyScript(ArrayList<String[]> editStack) {
		String appCallFlag = null;
		appCallFlag = editStack.get(0)[2];                                               // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)

		try {
			System.out.println(appCallFlag);
			switch (appCallFlag) {

			case ("AP"):                                                          // when appCallFlag=AP indicating that the run Aspenplus button has been pressed, then the following actions are going to be taken
				System.out.println(appCallFlag + " Button was pressed! (runPyScript)");          // for double checking
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
				line = br1.readLine();
				break;
			case ("APPW"):                                                              // when appCallFlag=APPW indicating that the run AspenPuls+PowerWorld button has been pressed, then the following actions are going to be taken
				System.out.println(appCallFlag + " Button was pressed! (runPyScript)");
				Process p2 = Runtime.getRuntime().exec(runPythonCommandAPPW);
				p2.waitFor();
				System.out.println("Exit Value (0 means success): "+ p2.exitValue()); // if console prints 0 it means success
				BufferedReader br2 = new BufferedReader(new InputStreamReader(p2.getInputStream()));
				String line2;                                                         // retrieves console from python script
				System.out.println("Python input:");
				while ((line2 = br2.readLine()) != null) {
					System.out.println(line2);                                        // print input array from Python (see python code for more details)
				}
				line = br2.readLine();
				break;				
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
/**This part of code: 
 * 1) collects all the input data for the aspen plus model of biodieselplant-3 (AP+PW), and writes the data to a csv file
 * 2) run the corresponding python script and thus run the aspen plus model and power world model sequentially, generate the output to a csv file
 * 3) update the output to ArcGIS database*/
	public void runAspenPlusandPowerWorld(ArrayList<String[]> editStack) {
		List<Double> xRow = new ArrayList<>(); 	    		
		xRow=getAPPWInput(editStack);
		
		String[] array = new String[xRow.size()];
		for (int i = 0; i < xRow.size(); i++) {
		    array[i] = Double.toString(xRow.get(i));
		} //convert the collected xRow from list to array
/**write the x-values into a csv file	*/	
        FileWriter filewriterAPIN = null;
        try {
			
			filewriterAPIN = new FileWriter(APINCSV); // to put the input values for the AspenPlus subset model

			filewriterAPIN.append("FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER");
			filewriterAPIN.append("\n");
			for(int i=0; i<array.length; i++){
				filewriterAPIN.append(array[i]);
				filewriterAPIN.append(",");
			}
			
			filewriterAPIN.flush();
			filewriterAPIN.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}			
		runPyScript(editStack);
		readAPPWCSV();	
	}
		
	/**This part of code: 
	 * 1) collects all the input data for the Pr surrogate model of biodieselplant-3 (AP+PW);
	 * 2) calls MoDS API, feeds the input data to the surrogate model, evaluates the model, generates the output to a csv file
	 * 3) update the output to ArcGIS database*/
	public void runPrAspenPlusandPowerWorld(ArrayList<String[]> editStack) {
		String appCallFlag = null;
		appCallFlag = editStack.get(0)[2];                                               // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)
		List<Double> xRow = new ArrayList<>();                                            // extra arraylist to collect the x-value required as input to the pr aspen plus model
		List<List<Double>> xData = new ArrayList<>(1);                                    // arraylist to
		List<List<Double>> yData;                                                         // output of the pr aspenplus model
		
		xRow=getAPPWInput(editStack);
		
// start evaluating the surrogate model		
		xData.add(xRow);                                                                       // pass all the collected input x-value to xData
		                                                             
//		String simDir = "C:/apache-tomcat-8.0.24/webapps/ROOT/APPWSim";                       // pass the directory of the aspenplus sorrogate model to simDir
/**try to get the model IRI from ontology file */	
/**		
//****************************************************************************************************************************		
		String uri = "File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlantSurrogateModelTest.owl";
		try{			
            OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
            
            OWLIndividual Jbiod = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Jbiod");                       
            String Model = Jbiod.getPropertyValueLiteral(owlModel.getOWLProperty("system:isModeldBy")).getString();
            String url = Model.getPropertyValueLiteral(owlModel.getOWLProperty("system:isModeldBy")).getString(); 
            
		}catch (OntologyLoadException ex) {
	           Logger.getLogger(PWServlet.class.getName()).log(Level.SEVERE, null, ex); 
	           System.out.println(ex);
	        }
		
//****************************************************************************************************************************	
**/	
		
		String simDir = APPWSim;	
		String modelName = "Polynomial_Alg_1";
		FileWriter fileWriter = null;
		try {
	
			fileWriter = new FileWriter(PrAPPWOUTCSV);                                        // filewriter for the output of pr aspenplus model
			System.load("C:/apache-tomcat-8.0.24/webapps/ROOT/MoDS_Java_API.dll");            //the MoDS API at use is version 0.1  D:\MoDS_API\MoDS_Java_API_v0.1
			
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
		System.out.println("xData=" + xData);
		System.out.println("yData=" + yData);                                              // print out the output yData to console

		for (int j = 0; j < yData.size(); j++) {
			try {
				fileWriter.append("\n");
				for (int k = 0; k < yData.get(j).size(); k++) {
					fileWriter.append(Double.toString(yData.get(j).get(k)));                        // write the yData to the output CSV file
					fileWriter.append(",");
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
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
				    
		    readPrAPPWCSV();	
		    
	}
	/**this method collects the input data for the AP+PW model */
	public ArrayList<Double> getAPPWInput(ArrayList<String[]> editStack){ 
		ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); // additional ArrayList for mixer
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		ArrayList<Map<String, Object>> attributeslist_ML = new ArrayList<Map<String, Object>>(); // additional ArrayList for material line
		ArrayList<Map<String, Object>> attributeslist_WL = new ArrayList<Map<String, Object>>(); // additional ArrayList for water line
		
		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");	
		
		/*String layer = null, OBJECTID = null, appCallFlag = null;
		layer = editStack.get(0)[0];
		OBJECTID = editStack.get(0)[1];
		appCallFlag = editStack.get(0)[2];
		System.out.println("layer="+layer);
		System.out.println("OBJECTID="+OBJECTID);
		*/
		
		for (Integer key : CPIDtoMatLine.keySet()) {
			try {
				QueryParameters qParameter_ML = new QueryParameters();                       // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
				qParameter_ML.setWhere("CPID='" + key + "'");                            // define FID address of an ArcGIS element
				qParameter_ML.setOutFields(new String[] { "*" });                            // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_ML = null;                                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_ML = null;                                                   // create an instance of Feature to store an ArcGIS element

				qTask_ML = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Material_line/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_ML = qTask_ML.execute(qParameter_ML);                   // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
				graphic_ML = (Feature) fResult_ML.iterator().next();                          // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_ML.add(graphic_ML.getAttributes());                            // append information about the  element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace();                                                            // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}
		for (Integer key : OBJECTIDtoWLB3.keySet()) {
			try {
				QueryParameters qParameter_WL = new QueryParameters();                       // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
				qParameter_WL.setWhere("OBJECTID='" + key + "'");                            // define FID address of an ArcGIS element
				qParameter_WL.setOutFields(new String[] { "*" });                            // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_WL = null;                                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_WL = null;                                                   // create an instance of Feature to store an ArcGIS element

				qTask_WL = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/water_line/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_WL = qTask_WL.execute(qParameter_WL);                   // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
				graphic_WL = (Feature) fResult_WL.iterator().next();                          // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_WL.add(graphic_WL.getAttributes());                            // append information about the  element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace();                                                            // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}
		
		for (Integer key : OBJECTIDtoHXB3.keySet()) {
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
	
		for (Integer key : OBJECTIDtoMXB3.keySet()) {
//			System.out.println(key);
			try {
				QueryParameters qParameter_MX = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
				qParameter_MX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
				qParameter_MX.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
				QueryTask qTask_MX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_MX = null; // create an instance of Feature to store an ArcGIS element

				qTask_MX = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

				FeatureResult fResult_MX = qTask_MX.execute(qParameter_MX); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
				graphic_MX = (Feature) fResult_MX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_MX.add(graphic_MX.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
			}
		}
		
		
		
		ArrayList<Double> xRow = new ArrayList<Double>();                                     // extra arraylist to collect the x-value required as input to the pr aspen plus model
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);                               // arraylist to
//		ArrayList<ArrayList<Double>> yData;                                                    // output of the pr aspenplus model
		
/**																	
		Thread thread_MX = new Thread(new Set_MX());
		 thread_MX.start();
	
		Thread thread_HX = new Thread(new Set_HX());
		 thread_HX.start();	
**/		 		 
		 FileWriter filewriterAPIN = null;

		try {
			
			filewriterAPIN = new FileWriter(APINCSV); // to put the input values for the AspenPlus subset model

			filewriterAPIN.append("FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER");
			filewriterAPIN.append("\n");
			
			for (int i = 0; i < attributeslist_ML.size(); i++) {
				for (String key : attributeslist_ML.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "CPID") {				
//50-56
						if (CPIDtoMatLine.get(i + 60).equals("3-1")) { 
							// "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_ML.get(i).get("Mat_1_qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_ML.get(i).get("Mat_T")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")));
//							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_ML.get(i).get("Mat_1_qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_ML.get(i).get("Mat_T")))); // add the temperature of oil to xRow
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")))); // add the pressure of oil to xRow
						break;
						}
						if (CPIDtoMatLine.get(i + 60).equals("3-2")) { 
							// "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_ML.get(i).get("Mat_1_qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_ML.get(i).get("Mat_T")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")));
//							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_ML.get(i).get("Mat_1_qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_ML.get(i).get("Mat_T")))); // add the temperature of oil to xRow
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")))); // add the pressure of oil to xRow
						break;
						}
					}
				}
			}
			
			for (int i = 0; i < attributeslist_WL.size(); i++) {
				for (String key : attributeslist_WL.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {				
//50-56
						if (OBJECTIDtoWLB3.get(i + 42).equals("FW-302")) { 
							// "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_WL.get(i).get("Mat_qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_WL.get(i).get("Mat_P")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")));
//							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_WL.get(i).get("Mat_qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_WL.get(i).get("Mat_P")))); // add the temperature of oil to xRow
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")))); // add the pressure of oil to xRow
						break;
						}
					}
				}
			}
						
			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {				
//50-56
						if (OBJECTIDtoHXB3.get(i + 48).equals("E-301")) { 
							// "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")));
//							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")))); // add the temperature of oil to xRow
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_P")))); // add the pressure of oil to xRow
						break;
						}
					}
				}
			}

			for (int i = 0; i < attributeslist_MX.size(); i++) {
				for (String key : attributeslist_MX.get(i).keySet()) { // go through all  the mixers in biodiesel plant
					if (key == "OBJECTID") {
//19-21
						if (OBJECTIDtoMXB3.get(i + 18).equals("M-301")) { // "mx01" is the mixer for methanol and the catalyst to be mixed before feeding to the reactor
							System.out.println("formixer="+OBJECTIDtoHXB3.get(i+53));
							filewriterAPIN.append(String.valueOf(attributeslist_MX.get(i).get("MatIn2Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_MX.get(i).get("MatIn2_T")));
							filewriterAPIN.append(",");
//							filewriterAPIN.append(String.valueOf(attributeslist_MX.get(i).get("MatIn2_P")));
//							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2Qnt")))); // add the feeding mole flowrate of methanol  to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2_T")))); // add the temperature of the feeding methanol flow to xRow							
//							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2_P")))); // add the pressure of the feeding methanol flow to xRow
						break;
						}
					}
				}
			}
//getting the flowrate for re-water and operation pressure for the boiler			
			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through all the heat exchanger in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoHXB3.get(i + 48).equals("E-307")) {
							System.out.println(OBJECTIDtoHXB3.get(i+54));
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("Operate_P")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the temperature of the outlet cold stream  to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("Operate_P")))); // add the temperature of the outlet cold stream  to xRow
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
		
	/**This part of code is related to the "run PrAP from OntoCAPE" function on the GUI, 
	 * it collects the un-modified input data and all the parameters for the surrogate model from an owl file, 
	 * evaluate the surrogate model, update the output to ArcGIS database */
	public void runParameterisedAPwithOntoCAPE(ArrayList<String[]> editStack) {
		ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); // additional ArrayList for mixer
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		UserCredentials user = new UserCredentials();
 		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
		String layer = null, OBJECTID = null;
		layer = editStack.get(0)[0]; 
		OBJECTID = editStack.get(0)[1];
		System.out.println("layer= "+ layer);
		System.out.println("OBJECTID= "+ OBJECTID);
		
		int Ai = 6, i=6, j=3;
		double[] Result = new double [j];
		double[] x=new double [i], xc=new double [i];
	    double[][] A = new double [Ai][j];
	    double[] C = new double [j];
	    double x0A=30, x0B=3, x1A=30, x1B=3, x2A=180, x2B=18, x3A=30, x3B=3, x4A=233.135, x4B=23.3135, x5A=4, x5B=0.4;
	    	    					
		String uri = "File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant.owl";
													
			try{			
                OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
                                                                       
//extracting the stateVariables for the surrogate
                if(layer.equals("Heater_cooler")||layer.equals("Mixer")){
                	if (layer.equals("Heater_cooler")&&(OBJECTID.equals("55")||OBJECTID.equals("56"))){
                    	if(OBJECTID.equals("55")){
                   		          		         		         		
                 		for (Integer key : OBJECTIDtoHXB3.keySet()) {
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
                 		for (int k = 0; k < attributeslist_HX.size(); k++) {
            				for (String key : attributeslist_HX.get(k).keySet()) { // go through all  the mixers in biodiesel plant
            					if (key == "OBJECTID") {

            						if (OBJECTIDtoHXB3.get(k + 50).equals("10E01B3")) { // "mx01" is the mixer for methanol and the catalyst to be mixed before feeding to the reactor
            							x[0] = (Double.parseDouble(String.valueOf(attributeslist_HX.get(k).get("MatIn1Qnt"))));
            		           		    xc[0] = (x[0]-x0A)/x0B;   //convert the true number to the space of (-1,1);
            		           		    x[1] = (Double.parseDouble(String.valueOf(attributeslist_HX.get(k).get("MatIn1_T"))));
         		           		        xc[1] = (x[1]-x1A)/x1B;   //convert the true number to the space of (-1,1);
            						}
            					}
            				}
            			}
                    	}else{            	
                    		OWLIndividual x1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_OIL_V");                       
                            String x1value = x1.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();             
                            x[0] = Double.valueOf(x1value)/807.3;              //molar flowrate	
                            xc[0] = (Double.valueOf(x1value)/807.3-x0A)/x0B;   //convert the true number to the space of (-1,1);    
                            
                            OWLIndividual x2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_OIL_V");                
                            String x2value = x2.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();               
                            x[1] = Double.valueOf(x2value);
                            xc[1] = (Double.valueOf(x2value)-x1A)/x1B;
                    }
                    	if(OBJECTID.equals("56")){
                    		for (int k = 0; k < attributeslist_HX.size(); k++) {
                				for (String key : attributeslist_HX.get(k).keySet()) { // go through all the heat exchanger in biodiesel plant
                					if (key == "OBJECTID") {
                						if (OBJECTIDtoHXB3.get(k + 50).equals("BoilerB3")) {
                							x[4] = (Double.parseDouble(String.valueOf(attributeslist_MX.get(k).get("MatOut6Qnt"))))/18.01;
                		           		    xc[4] = (x[4]-x4A)/x4B;              //convert the true number to the space of (-1,1);
                		           		    x[5] = (Double.parseDouble(String.valueOf(attributeslist_MX.get(k).get("Operate_P"))))/100;
             		           		        xc[5] = (x[5]-x5A)/x5B;              //convert the true number to the space of (-1,1);        							
                						}
                					}
                				}
                			}
                    	}else{
                    		OWLIndividual x5 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_RE-WATER_V");           
                            String x5value = x5.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                            x[4] = Double.valueOf(x5value)/18.01527;
                            xc[4] = (Double.valueOf(x5value)/18.01527-x4A)/x4B;
                            
                            OWLIndividual x6 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#P_Boiling_V");           
                            String x6value = x6.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                            x[5] = Double.valueOf(x6value)/100;
                            xc[5] = (Double.valueOf(x6value)/100-x5A)/x5B;
                    	}
                    }else{
                    	OWLIndividual x1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_OIL_V");                       
                        String x1value = x1.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();             
                        x[0] = Double.valueOf(x1value)/807.3;              //molar flowrate	
                        xc[0] = (Double.valueOf(x1value)/807.3-x0A)/x0B;   //convert the true number to the space of (-1,1);    
                        
                        OWLIndividual x2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_OIL_V");                
                        String x2value = x2.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();               
                        x[1] = Double.valueOf(x2value);
                        xc[1] = (Double.valueOf(x2value)-x1A)/x1B;
                        
                        OWLIndividual x5 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_RE-WATER_V");           
                        String x5value = x5.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                        x[4] = Double.valueOf(x5value)/18.01527;
                        xc[4] = (Double.valueOf(x5value)/18.01527-x4A)/x4B;
                        
                        OWLIndividual x6 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#P_Boiling_V");           
                        String x6value = x6.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                        x[5] = Double.valueOf(x6value)/100;
                        xc[5] = (Double.valueOf(x6value)/100-x5A)/x5B;
                    }
                    
                    if(layer.equals("Mixer")&&OBJECTID.equals("19")){
                    	for (Integer key : OBJECTIDtoMXB3.keySet()) {
                			try {
                				QueryParameters qParameter_MX = new QueryParameters();                       // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
                				qParameter_MX.setWhere("OBJECTID='" + key + "'");                            // define FID address of an ArcGIS element
                				qParameter_MX.setOutFields(new String[] { "*" });                            // fetch all  attributes of an ArcGIS element using *
                				QueryTask qTask_MX = null;                                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
                				Feature graphic_MX = null;                                                   // create an instance of Feature to store an ArcGIS element

                				qTask_MX = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
                				FeatureResult fResult_MX = qTask_MX.execute(qParameter_MX);                   // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
                				graphic_MX = (Feature) fResult_MX.iterator().next();                          // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
                				attributeslist_MX.add(graphic_MX.getAttributes());                            // append information about the  element in graphic_LP to ArrayList attributeslist_LP

                			} catch (Exception e) {
                				e.printStackTrace();                                                            // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
                			}
                		}
                    	for (int k = 0; k < attributeslist_MX.size(); k++) {
                    		for (String key : attributeslist_MX.get(k).keySet()) { // go through all  the mixers in biodiesel plant
            					if (key == "OBJECTID") {
            						if (OBJECTIDtoMXB3.get(k + 19).equals("MX01B3")) { // "mx01" is the mixer for methanol and the catalyst to be mixed before feeding to the reactor
            							x[2] = (Double.parseDouble(String.valueOf(attributeslist_MX.get(k).get("MatIn2Qnt"))));
            		           		    xc[2] = (x[2]-x2A)/x2B;   //convert the true number to the space of (-1,1);
            		           		    x[3] = (Double.parseDouble(String.valueOf(attributeslist_MX.get(k).get("MatIn2_T"))));
         		           		        xc[3] = (x[3]-x3A)/x3B;   //convert the true number to the space of (-1,1);
            						}
            					}
            				}
                    	}
            				
                    }else{            	
                    	OWLIndividual x3 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_MEOH_V");           
                        String x3value = x3.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                        x[2] = Double.valueOf(x3value)/32.04;           // molar flowrate
                        xc[2] = (Double.valueOf(x3value)/32.04-x2A)/x2B; 	
                                    
                        OWLIndividual x4 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_MEOH_V");           
                        String x4value = x4.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                        x[3] = Double.valueOf(x4value);
                        xc[3] = (Double.valueOf(x4value)-x3A)/x3B;
                }                                        
                }else{
                	OWLIndividual x1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_OIL_V");                       
                    String x1value = x1.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();             
                    x[0] = Double.valueOf(x1value)/807.3;              //molar flowrate	
                    xc[0] = (Double.valueOf(x1value)/807.3-x0A)/x0B;   //convert the true number to the space of (-1,1);
                                                                    
                    OWLIndividual x2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_OIL_V");                
                    String x2value = x2.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();               
                    x[1] = Double.valueOf(x2value);
                    xc[1] = (Double.valueOf(x2value)-x1A)/x1B;           
                                
                    OWLIndividual x3 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_MEOH_V");           
                    String x3value = x3.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                    x[2] = Double.valueOf(x3value)/32.04;           // molar flowrate
                    xc[2] = (Double.valueOf(x3value)/32.04-x2A)/x2B; 	
                                
                    OWLIndividual x4 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_MEOH_V");           
                    String x4value = x4.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                    x[3] = Double.valueOf(x4value);
                    xc[3] = (Double.valueOf(x4value)-x3A)/x3B;
                    
                    OWLIndividual x5 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_RE-WATER_V");           
                    String x5value = x5.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                    x[4] = Double.valueOf(x5value)/18.01527;
                    xc[4] = (Double.valueOf(x5value)/18.01527-x4A)/x4B;
                    
                    OWLIndividual x6 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#P_Boiling_V");           
                    String x6value = x6.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
                    x[5] = Double.valueOf(x6value)/100;
                    xc[5] = (Double.valueOf(x6value)/100-x5A)/x5B;
                }
            
                 
//extracting the parameters for the first surrogate: calculating the molar flow of the FinalProduct             
            OWLIndividual Af01 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A01_MathM_FinalProduct_F_V");           
            String Af01value = Af01.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[0][0] = Double.valueOf(Af01value);  	
                        
            OWLIndividual Af11 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A11_MathM_FinalProduct_F_V");           
            String Af11value = Af11.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[1][0] = Double.valueOf(Af11value);
            
            OWLIndividual Af21 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A21_MathM_FinalProduct_F_V");           
            String Af21value = Af21.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[2][0] = Double.valueOf(Af21value);
            
            OWLIndividual Af31 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A31_MathM_FinalProduct_F_V");           
            String Af31value = Af31.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[3][0] = Double.valueOf(Af31value);
            
            OWLIndividual Af41 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A41_MathM_FinalProduct_F_V");           
            String Af41value = Af41.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[4][0] = Double.valueOf(Af41value);
            
            OWLIndividual Af51 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A51_MathM_FinalProduct_F_V");           
            String Af51value = Af51.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[5][0] = Double.valueOf(Af51value);
            
            OWLIndividual Cf = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#C_MathM_FinalProduct_F_V");           
            String Cfvalue = Cf.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            C[0] = Double.valueOf(Cfvalue);
            System.out.println("A00="+A[0][0]+", A10="+A[1][0]+", A20="+A[2][0]+", A30="+A[3][0]+", A40="+A[4][0]+", A50="+A[5][0]);
            
//extracting the parameters for the second surrogate: calculating the molar purity of the FinalProduct             
            OWLIndividual Ay01 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A01_MathM_FinalProduct_y_V");           
            String Ay01value = Ay01.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[0][1] = Double.valueOf(Ay01value);  	
                        
            OWLIndividual Ay11 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A11_MathM_FinalProduct_y_V");           
            String Ay11value = Ay11.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[1][1] = Double.valueOf(Ay11value);
            
            OWLIndividual Ay21 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A21_MathM_FinalProduct_y_V");           
            String Ay21value = Ay21.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[2][1] = Double.valueOf(Ay21value);
            
            OWLIndividual Ay31 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A31_MathM_FinalProduct_y_V");           
            String Ay31value = Ay31.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[3][1] = Double.valueOf(Ay31value);
            
            OWLIndividual Ay41 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A41_MathM_FinalProduct_y_V");           
            String Ay41value = Ay41.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[4][1] = Double.valueOf(Ay41value);
            
            OWLIndividual Ay51 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A51_MathM_FinalProduct_y_V");           
            String Ay51value = Ay51.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[5][1] = Double.valueOf(Ay51value);
            
            OWLIndividual Cy = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#C_MathM_FinalProduct_y_V");           
            String Cyvalue = Cy.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            C[1] = Double.valueOf(Cyvalue);
            System.out.println("A01="+A[0][1]+", A11="+A[1][0]+", 0A21="+A[2][1]+", 0A31="+A[3][1]+", A41="+A[4][1]+", A51="+A[5][1]);
 
//extracting the parameters for the third surrogate: calculating the molar purity of the FinalProduct             
            OWLIndividual At01 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A01_MathM_FinalProduct_T_V");           
            String At01value = At01.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[0][2] = Double.valueOf(At01value);  	
                        
            OWLIndividual At11 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A11_MathM_FinalProduct_T_V");           
            String At11value = At11.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[1][2] = Double.valueOf(At11value);
            
            OWLIndividual At21 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A21_MathM_FinalProduct_T_V");           
            String At21value = At21.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[2][2] = Double.valueOf(At21value);
            
            OWLIndividual At31 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A31_MathM_FinalProduct_T_V");           
            String At31value = At31.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[3][2] = Double.valueOf(At31value);
            
            OWLIndividual At41 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A41_MathM_FinalProduct_T_V");           
            String At41value = At41.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[4][2] = Double.valueOf(At41value);
            
            OWLIndividual At51 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#A51_MathM_FinalProduct_T_V");           
            String At51value = At51.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            A[5][2] = Double.valueOf(At51value);
            
            OWLIndividual Ct = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#C_MathM_FinalProduct_T_V");           
            String Ctvalue = Ct.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();    
             
            
            
            C[2] = Double.valueOf(Ctvalue);
            System.out.println("A02="+A[0][2]+", A12="+A[1][2]+", A22="+A[2][2]+", A32="+A[3][2]+", A42="+A[4][2]+", A52="+A[5][2]);	    		
		    System.out.println("C0="+C[0]+", C1="+C[1]+", C2="+C[2]);
            
		}catch (OntologyLoadException ex) {
           Logger.getLogger(PWServlet.class.getName()).log(Level.SEVERE, null, ex); 
           System.out.println(ex);
        }
        			
			System.out.println("x0="+x[0]+", x1="+x[1]+", x2="+x[2]+", x3="+x[3]+", x4="+x[4]+", x5="+x[5]);
			System.out.println("xc0="+xc[0]+", xc1="+xc[1]+", xc2="+xc[2]+", xc3="+xc[3]+", xc4="+xc[4]+", xc5="+xc[5]);
			for(j=0; j<3; j++){
    			Result[j]=C[j]+A[0][j]*xc[0]+A[1][j]*xc[1]+A[2][j]*xc[2]+A[3][j]*xc[3]+A[4][j]*xc[4]+A[5][j]*xc[5];    			
    			System.out.println("Result"+j+"= "+Result[j]);
    		}
			updateAPfromOntology(Result);
	}
	/**this method update the output generated by this function "runParameterisedAPwithOntoCAPE" */
	public void updateAPfromOntology(double[] output){
		int i=3;
		double[] result = new double [i];
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		result[0]=output[0];
		result[1]=output[1];
		result[2]=output[2];

		try {
			long start = System.currentTimeMillis(); // start a timer
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			GeodatabaseFeatureServiceTable RadFracTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/RadFrac/FeatureServer",user, 0);
			RadFracTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			RadFracTable.initialize();

			final CountDownLatch latch = new CountDownLatch(1);                                                                  // ZL-151207 handles one asynchronous processes, only continuesThread when it reaches 0
			RadFracTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {                                                                  // Asynchronous callback: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown();                                                                                 // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			latch.await(); // wait until all feature service tables are ready then continue

				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[7];

				for (int j = 0; j < 2; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoRadF.get(j + 1).equals("10D08")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> RadFracAttributes = RadFracTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
							RadFracAttributes.put("MatOut3Qnt",result[0]);                                                               // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
							System.out.println("F="+result[0]);
						
							RadFracAttributes.put("MatOut3_T",result[2]);                                                                // upgrade the new temperature of ester3 that calculated by the pr aspen plus model to ArcGIS databse
							System.out.println("T="+result[2]);
						RadFracTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),RadFracAttributes);                                 // update feature table locally
						break;
					}
				}

			RadFracTable.applyEdits(null); // commit local updates onto server
//			RadFracTable.dispose();
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms");                    // tells how long it took to update
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
		
    /**This function runs only the aspen plus model for biodieslplant-3 (without running the PowerWorld model)*/
	public void runAspenPlus(ArrayList<String[]> editStack) {
		getAPPWInput(editStack);          //collects the input data
		runPyScript(editStack);           // call python script to run aspen plus model
		readAPCSV();                      //update the output 
/*		
		ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); // additional ArrayList for mixer
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		ArrayList<Map<String, Object>> attributeslist_CR = new ArrayList<Map<String, Object>>(); // additional ArrayList for chemical reactor
		ArrayList<Map<String, Object>> attributeslist_SP = new ArrayList<Map<String, Object>>(); // additional ArrayList for separator
		ArrayList<Map<String, Object>> attributeslist_DC = new ArrayList<Map<String, Object>>(); // additional ArrayList for Decanter
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		List<Double> xRow = new ArrayList<>(); // extra arraylist to collect the x-value required as input to the pr aspen plus model
		
		for (int key : OBJECTIDtoMXNum.keySet()) {
			System.out.println(key);
			try {
				QueryParameters qParameter_MX = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
				qParameter_MX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
				qParameter_MX.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
				QueryTask qTask_MX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_MX = null; // create an instance of Feature to store an ArcGIS element

				qTask_MX = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

				FeatureResult fResult_MX = qTask_MX.execute(qParameter_MX); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
				graphic_MX = (Feature) fResult_MX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_MX.add(graphic_MX.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
			}
		}
		
		for (Integer key : OBJECTIDtoHXNum.keySet()) {
			try {
				QueryParameters qParameter_HX = new QueryParameters(); // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
				qParameter_HX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
				qParameter_HX.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_HX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_HX = null; // create an instance of Feature to store an ArcGIS element

				qTask_HX = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_HX = qTask_HX.execute(qParameter_HX); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
				graphic_HX = (Feature) fResult_HX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_HX.add(graphic_HX.getAttributes()); // append information about the  element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}
		
		for (Integer key : OBJECTIDtoCRNum.keySet()) {
			try {
				QueryParameters qParameter_CR = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
				qParameter_CR.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
				qParameter_CR.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_CR = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_CR = null; // create an instance of Feature to store an ArcGIS element

				qTask_CR = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Reactor/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_CR = qTask_CR.execute(qParameter_CR); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and  qTask_LP
				graphic_CR = (Feature) fResult_CR.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP  and stores it in graphic_LP; qParameter_LP requests information about a single element only
				attributeslist_CR.add(graphic_CR.getAttributes()); // append information  about the  element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}
		
		for (Integer key : OBJECTIDtoSPNum.keySet()) {
			try {
				QueryParameters qParameter_SP = new QueryParameters(); // create an instance of QueryParameters to be  used for querying ArcGIS database for predefined data
				qParameter_SP.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
				qParameter_SP.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
				QueryTask qTask_SP = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_SP = null; // create an instance of Feature to store an ArcGIS element

				qTask_SP = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Flashdrum/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_SP = qTask_SP.execute(qParameter_SP); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and qTask_LP
				graphic_SP = (Feature) fResult_SP.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element  only
				attributeslist_SP.add(graphic_SP.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}
		
		for (Integer key : OBJECTIDtoDCNum.keySet()) {
			try {
				QueryParameters qParameter_DC = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database  for  predefined data
				qParameter_DC.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
				qParameter_DC.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
				QueryTask qTask_DC = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_DC = null; // create an instance of Feature to store an ArcGIS element

				qTask_DC = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Decanter/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_DC = qTask_DC.execute(qParameter_DC); // FeatureResult is used to store information from ArcGIS database  requested  using qParameter_LP and qTask_LP
				graphic_DC = (Feature) fResult_DC.iterator().next(); // queryResult.iterator() iterates over the elements  in fResult_LP and  stores it in graphic_LP; qParameter_LP requests information  about a  single  element only
				attributeslist_DC.add(graphic_DC.getAttributes()); // append information about the element in graphic_LP  to ArrayList attributeslist_LP

			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the  Exception to System.err. It's a very  simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
			}
		}
		
//****************************************************															
		Thread thread_MX = new Thread(new Set_MX());
		 thread_MX.start();
		 
		Thread thread_HX = new Thread(new Set_HX());
		 thread_HX.start();	

		 Thread thread_CR = new Thread(new Set_CR());
		 thread_CR.start();

		 Thread thread_SP = new Thread(new Set_SP());
		 thread_SP.start();

		 Thread thread_DC = new Thread(new Set_DC());
		 thread_DC.start();
//****************************************************
	
		FileWriter filewriterAPIN = null;

		try {			
			filewriterAPIN = new FileWriter(APINCSV); // to put the input values for the AspenPlus subset model

			filewriterAPIN.append("FOIL, TOIL, FMEOH, TMEOH, TCR, VCR, TSP, TDC, T10E01, T10E02, T10E03");
//			filewriterAPIN.append("FOIL, TOIL, FMEOH, TMEOH, FRe-water, PBoiler");
			filewriterAPIN.append("\n");

			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoHXNum.get(i + 1).equals("10E01")) { // "10E01" is the heat exchanger for oil to be heated before feeding to the reactor
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
			System.out.println("xRow=" + xRow);
			for (int i = 0; i < attributeslist_MX.size(); i++) {
				for (String key : attributeslist_MX.get(i).keySet()) { // go through all  the mixers in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoMXNum.get(i + 1).equals("mx01")) { // "mx01" is the mixer for methanol and the catalyst to be mixed before feeding to the reactor
							filewriterAPIN.append(String.valueOf(attributeslist_MX.get(i).get("MatIn2Qnt")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_MX.get(i).get("MatIn2_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2Qnt")))); // add the feeding mole flowrate of methanol  to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2_T")))); // add the temperature of the feeding methanol flow to xRow
						}
					}
				}
			}

			System.out.println("xRow=" + xRow);
			for (int i = 0; i < attributeslist_CR.size(); i++) {
				for (String key : attributeslist_CR.get(i).keySet()) { // go through all the reactors in biodiesel plant
					if (key == "OBJECTID") {
						
						if (OBJECTIDtoCRNum.get(i + 1).equals("10D01")) { // "10D01" is the first reactor
							filewriterAPIN.append(String.valueOf(attributeslist_CR.get(i).get("operate_T")));
							filewriterAPIN.append(",");
							filewriterAPIN.append(String.valueOf(attributeslist_CR.get(i).get("Volume")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_CR.get(i).get("operate_T")))); // add the operation temperature of reactor to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_CR.get(i).get("Volume")))); // add the reaction volume of reactor to xRow
						}
					}
				}
			}
			System.out.println("xRow=" + xRow);						
			for (int i = 0; i < attributeslist_SP.size(); i++) {
				for (String key : attributeslist_SP.get(i).keySet()) { // go through all the separators in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoSPNum.get(i + 1).equals("10D02")) {
							filewriterAPIN.append(String.valueOf(attributeslist_SP.get(i).get("Operate_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_SP.get(i).get("Operate_T")))); // add the operation temperature to xRow
						}
					}
				}
			}
			System.out.println("xRow=" + xRow);
			for (int i = 0; i < attributeslist_DC.size(); i++) {
				for (String key : attributeslist_DC.get(i).keySet()) { // go through all the decanter in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoDCNum.get(i + 1).equals("10D02D")) {
							filewriterAPIN.append(String.valueOf(attributeslist_DC.get(i).get("Operate_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_DC.get(i).get("Operate_T")))); // add the operation temperature to xRow
						}
					}
				}
			}
			System.out.println("xRow=" + xRow);
			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through all the heat exchanger in biodiesel plant
					if (key == "OBJECTID") {

						if (OBJECTIDtoHXNum.get(i + 1).equals("10E01")|| OBJECTIDtoHXNum.get(i + 1).equals("10E02")|| OBJECTIDtoHXNum.get(i + 1).equals("10E03")) {
							filewriterAPIN.append(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")));
							filewriterAPIN.append(",");
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatOut1_T")))); // add the temperature of the outlet cold stream  to xRow
						}
					}
				}
			}
			System.out.println("xRow=" + xRow); // print out all the x-data that has been collected to console
			
			filewriterAPIN.flush();
			filewriterAPIN.close();

		} catch (Exception e) {
			e.printStackTrace();
		}
*/
		
/*		
		for (int i = 0; i < editStack.size(); i++) {                                                      // for each feature in editStack, append something to skeleton, attributeslist and layers
			String appCallFlag = (String) editStack.get(i)[2];
			switch (appCallFlag) {
			case "AP":
				System.out.println(appCallFlag + "was pressed");
				runPyScript(editStack);                                                                  // call python script to run aspen plus model
				readAPCSV();
				break;
			case "PrAP":
				System.out.println(appCallFlag + "was pressed");
				List<List<Double>> xData = new ArrayList<>(1);                                          // arraylist to
				List<List<Double>> yData;                                                               // output of the pr aspenplus model
				xData.add(xRow);                                                                        // pass all the collected input x-value to xData
				System.out.println("xData=" + xData);
				String simDir = "C:/apache-tomcat-8.0.24/webapps/ROOT/APSim1";                          // pass the directory of the aspenplus sorrogate model to simDir
				String modelName = "Polynomial_surrogate_Alg_1";
				FileWriter fileWriter = null;
				try {			
					fileWriter = new FileWriter(PrAPOUTCSV);                                            // filewriter for the output of pr aspenplus model
					System.load("C:/apache-tomcat-8.0.24/webapps/MoDS_Java_API.dll");                   //load the dll      
					ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);				
					System.out.println("xNames= " + xNames);
					
					ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);
					System.out.println("yNames= " + yNames);
					for (int j = 0; j < yNames.size(); j++) {
						fileWriter.append(yNames.get(j));                                              // write the yNames to the output CSV file
						fileWriter.append(",");
					}
				} catch (Error e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}

				yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData);                                // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam"  and  the input xData that was collected before
				System.out.println("yData=" + yData);                                                       // print out the output yData to console

				for (int j = 0; j < yData.size(); j++) {
					try {
						fileWriter.append("\n");
						for (int k = 0; k < yData.get(j).size(); k++) {
							fileWriter.append(Double.toString(yData.get(j).get(k)));                        // write the yData to the output CSV file
							fileWriter.append(",");
						}
					} catch (IOException e) {
						// TODO Auto-generated catch block
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
				readPrAPCSV();
				break;
				
			}
		}
*/		
	}
    /**This method:
     * 1) collects all the input data for the Pr PowerWorld model of the whole Jurong Island;
     * 2) calls MoDS API, evaluate the surrogate model, generate output csv file
     * 3) update the output to ArcGIS database (the updating function is still requires debug)*/
	public void runPrPowerWorld(ArrayList<String[]> editStack) {
				
		end_time = System.currentTimeMillis();
		ArrayList<Map<String, Object>> attributeslist_LP = new ArrayList<Map<String, Object>>(); // additional ArrayList for loadpoints
		ArrayList<Map<String, Object>> attributeslist_PG = new ArrayList<Map<String, Object>>(); // additional ArrayList for powergen
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		int counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
		for (String key : ArcGISFIDtoPWBusNum.keySet()) {
			try {				
				String[] temp = new String[178];	
			    temp[counter]= key; 
			   				
				QueryParameters qParameter_LP = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
				qParameter_LP.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
				qParameter_LP.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_LP = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_LP = null; // create an instance of Feature to store an ArcGIS element
				
//				System.out.println("We are here");
								
				qTask_LP = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Load_points/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_LP = qTask_LP.execute(qParameter_LP); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and qTask_LP
				graphic_LP = (Feature) fResult_LP.iterator().next(); // queryResult.iterator() iterates over the elements  in fResult_LP and stores it in  graphic_LP; qParameter_LP  requests information about a single element only
				attributeslist_LP.add(graphic_LP.getAttributes());  // append information about the element in graphic_LP to ArrayList attributeslist_LP
				
//				System.out.println("Loading No."+counter+ " The key is: "+key );				
//				System.out.println("temp["+counter+"] is "+ temp[counter]);
				
				counter++;

                if (counter == 178) {		
					System.out.print("Done loading 178");
					break;
				}
			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
			}
		}

		counter = 0; // the following loop iterates over FID numbers of load points stored within ArcGISFIDtoPWBusNum ArrayList and appends the attributes of each load point to ArrayList attributeslist_LP
		for (String key : ArcGISFIDtoPGBusNum.keySet()) {
			try {

				QueryParameters qParameter_PG = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for  predefined data
				qParameter_PG.setWhere("OBJECTID='" + key + "'"); // define FID address  of an ArcGIS element
				qParameter_PG.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
				QueryTask qTask_PG = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
				Feature graphic_PG = null; // create an instance of Feature to store an ArcGIS element

				qTask_PG = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Generators/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials
				FeatureResult fResult_PG = qTask_PG.execute(qParameter_PG); // FeatureResult is used to store information from ArcGIS database requested using qParameter_PG and qTask_PG
				graphic_PG = (Feature) fResult_PG.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_PG and stores it in graphic_PG; qParameter_PG requests information about a single element only
				attributeslist_PG.add(graphic_PG.getAttributes()); // append information about the element in graphic_PG to ArrayList attributeslist_LP

				counter++;
				if (counter == 7) {
					break;
				}
			} catch (Exception e) {
				e.printStackTrace(); // It prints the stack trace of the Exception to System.err.
			}
		}

		writePrPWCSV(attributeslist_LP, attributeslist_PG);

// the following code extract the required input data set from arcgis database
//		ArrayList<ArrayList<Double>> xData = new ArrayList<>(1);
		List<List<Double>> xData = new ArrayList<>(1);				
		List<Double> xRow = new ArrayList<>();
		List<List<Double>> yData;

		String[] ArcGISFID = null;
		ArcGISFID = new String[108];                        // for the simplified parameterised PW model, 6 inputs from 3 of the BusNum are required

		for (int j = 0; j < 108; j++) {
			String BusNum = XPointtoBusNum.get(j);          // get the BusNum so that we can look for the ArcGIS FID, then extract the input x-value
			System.out.println(BusNum);
			ArcGISFID[j] = PWBusNumtoArcGISFID.get(BusNum);  // get the ArcGIS FID for the input x-values
		}

		BufferedReader fileReader = null;
		BufferedReader fileReader1 = null;
		BufferedReader fileReader2 = null;
		BufferedReader fileReader3 = null;
		FileWriter XValue = null;

		try {
			String line2 = null, line3 = null;
			;
			fileReader2 = new BufferedReader(new FileReader(PGPIN)); // read the pwr_p data for powerGen from CSV file
			fileReader3 = new BufferedReader(new FileReader(PGQIN)); // read the pwr_Q data for  powerGen from CSV file

			XValue = new FileWriter(XVALUE);

			for (int k = 0; k < 108; k++) {
				String line = null;
				fileReader = new BufferedReader(new FileReader(LPPIN)); // read the pwr_p data from CSV file
				fileReader.readLine();                                  // Read the CSV flie header to skip it
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

			for (int k = 0; k < 108; k++) {
				String line1 = null;
				fileReader1 = new BufferedReader(new FileReader(LPQIN)); // read the  pwr_Q data from CSV file
				while ((line1 = fileReader1.readLine()) != null) {
					String[] data = line1.split(",");
					if (ArcGISFID[k].equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
						XValue.append(data[2].trim());
						XValue.append("\n");
						System.out.println("Xvalue1" + k + "=" + data[2]);
						xRow.add(Double.parseDouble(data[2].trim()));                            // convert string to double and add to xRow
						break;
					}
				}
			}

			// to get the data for the powerGen
			while ((line2 = fileReader2.readLine()) != null) {
				String[] data = line2.split(",");
				XValue.append(data[2].trim());
				XValue.append("\n");
				System.out.println("Xvalue2" + "=" + data[2]);
				xRow.add(Double.parseDouble(data[2].trim()));                                   // convert string to double and add to xRow
			}
			// }
			while ((line3 = fileReader3.readLine()) != null) {
				String[] data = line3.split(",");
				XValue.append(data[2].trim());
				XValue.append("\n");
				System.out.println("Xvalue3" + "=" + data[2]);
				xRow.add(Double.parseDouble(data[2].trim()));                                    // convert string to double and add to xRow
			}
			// }
			XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
			XValue.close();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
				fileReader1.close();
				fileReader2.close();
				fileReader3.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		// end of extracting the input data set from arcgis database

	xData.add(xRow);                                                                                  // pass all the collected input x-value to xData
				
		System.out.println("xData=" + xData);

		String simDir = Sim1;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_1";
		FileWriter fileWriter = null;
		try {			

			fileWriter = new FileWriter(PrPWOUTCSV); // filewriter for the
			System.load("C:/apache-tomcat-8.0.24/webapps/ROOT/MoDS_Java_API.dll");                     //the MoDS API at use is version 0.1		
			
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

	}
	/**This method:
     * 1) collects all the input data for the PowerWorld model of the whole Jurong Island;
     * 2) calls python script, run the PowerWorld model, generate output csv file
     * 3) update the output to ArcGIS database (the updating function is still requires debug)*/
	public void runPowerWorld(ArrayList<String[]> editStack) {
		ArrayList<String[]> skeleton = new ArrayList<String[]>(); // array of strings containing PW fields, ArcGIS fields corresponding to PW fields and PW object type
		ArrayList<Map<String, Object>> attributeslist = new ArrayList<Map<String, Object>>();
		ArrayList<String> layers = new ArrayList<String>();
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username  and password

		for (int i = 0; i < editStack.size(); i++) { // for each feature in editStack, append something to skeleton, attributeslist and layers
			String layer = (String) editStack.get(i)[0];
			String graphicFID = (String) editStack.get(i)[1];
			// String appCallFlag = (String) editStack.get(i)[3];

			QueryParameters qParameter = new QueryParameters();
			qParameter.setWhere("OBJECTID='" + graphicFID + "'"); // find graphic using FID
			qParameter.setOutFields(new String[] { "*" }); // fetch all attributes using *
			QueryTask qTask = null;
			Feature graphic = null;

			if (layer.equals("UHT_Substation_(230_66kV)") || layer.equals("UHT_Lines_(230kV)") || layer.equals("EHT_Substation_(66_22kV)") || layer.equals("EHT_Lines") // check if feature in editStack is part of power grid
					|| layer.equals("HT_Lines") || layer.equals("PowerGen") || layer.equals("Load_Points") || layer.equals("Bus_Coupler")) { // PW variable names can be found in Case Object Fields.xslx

				if (layer.equals("Load_Points")) {                                                                                           // variable names specific to load points (e.g. LoadMW=pwr_P,  LoadMVR=pwr_Q)
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,volt_nom", "Bus" });
					skeleton.add(new String[] { "BusNum,LoadID,LoadMW,LoadMVR", "OBJECTID,LoadID,pwr_P,pwr_Q", "Load" });                    // can  only  modify MW and MVR at load, not bus
					try {
						qTask = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Load_points/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				} else if (layer.equals("UHT_Substation_(230_66kV)")) {
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,HV_kV", "Bus" });                                                 // high voltage bus
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,LV_kV", "Bus" });                                                // low voltage bus
					try {
						qTask = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/UHT_substations/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				} else if (layer.equals("EHT_Substation_(66_22kV)")) {
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,HV_kV", "Bus" });
					skeleton.add(new String[] { "BusNum,BusNomVolt", "OBJECTID,LV_kV", "Bus" });
					try {
						qTask = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/EHT_substation/FeatureServer/0", user);
					} catch (EsriSecurityException e) {
						e.printStackTrace();
					}
				} else if (layer.equals("PowerGen")) { skeleton.add(new String[] { "BusNum,GenID,GenMW,GenMVR", "OBJECTID,GenID,PowerGenMW,PowerGenMVR", "Gen" });
					skeleton.add(new String[] { "BusNum,GenID,GenMW,GenMVR", "OBJECTID,GenID,PowerGenMW,PowerGenMVR", "Gen" });                   // duplicate because generator only needs to update one  bus
					try {
						qTask = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Generators/FeatureServer/0", user);
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
	/**write input csv file for aspen plus model */ 	
	public void writeAPCSV(ArrayList<String[]> skeleton, ArrayList<Map<String, Object>> attributeslist, ArrayList<String> layers) {
		FileWriter fileWriter = null;

		try {
			fileWriter = new FileWriter(APINCSV);
			for (int i = 0; i < skeleton.size(); i++) {
				fileWriter.append(skeleton.get(i)[0]); // write headers-Aspen Plus input names
				fileWriter.append("\n"); // start a New line
				String[] ArcGISfields = skeleton.get(i)[1].split(",");
				Map<String, Object> attributes = attributeslist.get(i); // pulls all date fields available from ArcGIS

				for (int j = 0; j < ArcGISfields.length; j++) {
					if (ArcGISfields[j].equals("FOIL")) {
						String ArcGISOILF = String.valueOf(attributes.get("MatIn1Qnt")); // get the mole flowrate of oil from ArcGIS database
						fileWriter.append(ArcGISOILF); // write the mole flowrate of oil to APINCSV
						fileWriter.append(",");
					} else if (ArcGISfields[j].equals("FMEOH")) {
						String ArcGISMEOHF = String.valueOf(attributes.get("MatIn2Qnt")); // get the mole flowrate of methanol from ArcGIS database
						fileWriter.append(ArcGISMEOHF); // write the mole flowrate of methanol to APINCSV
					}
				}
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


// ZL-20160328 change the readAPCSV method to update the output of the full scale modle to GUI
	public void readPrAPCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrAPPWOUTCSV));
			fileReader.readLine();     // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
//			GeodatabaseFeatureServiceTable HeatexchangerTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer", user, 0);
			GeodatabaseFeatureServiceTable MaterialLineTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/RadFrac/FeatureServer", user, 0);
			MaterialLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			MaterialLineTable.initialize();
			System.out.println(MaterialLineTable.getStatus());
			MaterialLineTable.getInitializationError();

			final CountDownLatch latch = new CountDownLatch(1);                                                                             // ZL-151207 handles one asynchronous processes, only continues  Thread when it reaches 0
			MaterialLineTable.populateFromService(loadAllFeatures, false, new CallbackListener<Boolean>() {
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
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[7];

				for (int j = 0; j < 2; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoRadF.get(j + 1).equals("10D08")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> MaterialLineAttributes = MaterialLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[0].trim().isEmpty()) {
							MaterialLineAttributes.put("MatOut3Qnt",Float.parseFloat(data[0].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("F="+data[0]);
						if (!data[2].trim().isEmpty()) {
							MaterialLineAttributes.put("MatOut3_T",Float.parseFloat(data[2].trim()));                                       // upgrade the new temperature of ester3 that calculated by the pr aspen plus model to ArcGIS databse
						}
						MaterialLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),MaterialLineAttributes);                          // update feature table locally
						break;
					}
				}
			}
			MaterialLineTable.applyEdits(null);    // commit local updates onto Server
//			MaterialLineTable.dispose();
			System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis() - start) + "ms");                     // tells how long it took to update
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

	// update the ArcGIS database according to the output of the PrPW model ZL-20160111
	public void readPrPWCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrPWOUTCSV));
			fileReader.readLine();     // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable LoadPointTable = new GeodatabaseFeatureServiceTable( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Load_points/FeatureServer", user, 0);
			LoadPointTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			LoadPointTable.initialize();
			System.out.println(LoadPointTable.getStatus());
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
//				System.out.println("data= " + data);
				
//				String[] ArcGISFID = null;
//				ArcGISFID = new String[209]; 
				

				
				for (int j = 0; j < 209; j++) {
					String PWBusNum = String.valueOf(j+1);
					String ArcGISFID = PWBusNumtoArcGISFID.get(PWBusNum);
					
//					String BusNum = XPointtoBusNum.get(j);
//					System.out.println(BusNum);
//					ArcGISFID = PWBusNumtoArcGISFID.get(BusNum);
					System.out.println(ArcGISFID);

					if (ArcGISFID != null) {
						Map<String, Object> LoadPointAttributes = LoadPointTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
						if (!data[1 + 5 * j].trim().isEmpty()) {
							LoadPointAttributes .put("theta_act", Float.parseFloat(data[1 + 5 * j] .trim()) * 2 * 3.146 / 360); // convert the bus angle to radian and upgrade it to the corressponding BusNum attributes
						}
						if (!data[2 + 5 * j].trim().isEmpty()) { 
							LoadPointAttributes.put("volt_act", Float.parseFloat(data[2 + 5 * j].trim()));
						}
						if (!data[3 + 5 * j].trim().isEmpty()) {
							LoadPointAttributes.put("pwr_P_act",Float.parseFloat(data[3 + 5 * j].trim()));
						}
						if (!data[4 + 5 * j].trim().isEmpty()) {
							LoadPointAttributes.put("pwr_Q_act",Float.parseFloat(data[4 + 5 * j].trim()));
						}
						
						LoadPointTable.updateFeature( Long.parseLong(ArcGISFID), LoadPointAttributes); // update feature table locally
					}
				}
			}
			LoadPointTable.applyEdits(null); // commit local updates onto server
//			LoadPointTable.dispose();
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
// update the ArcGIS database according to the output of the PrPW model
	/**read the aspen plus output csv file and update to ArcGIS database*/
	public void readAPCSV() {
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

			final CountDownLatch latch = new CountDownLatch(1); // ZL-151207 handles one asynchronous processes, only continuesThread when it reaches 0
			RadFracTable.populateFromService(loadAllFeatures, false,
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
				//System.out.println("data= " + data);
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[7];

				for (int j = 0; j < 2; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoRadF.get(j + 1).equals("10D08")) {                                                                     // heat  exchanger  10E03 is  for now where the output data should be upgraded to
						Map<String, Object> RadFracAttributes = RadFracTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[1].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3Qnt",Float.parseFloat(data[1].trim()));   // upgrade the new mole  flowrate of ester3 that calculated  by the pr aspen  plus model to ArcGIS  databse
						}
						System.out.println("F="+data[0]);
						if (!data[2].trim().isEmpty()) {
							RadFracAttributes.put("MatOut3_T",Float.parseFloat(data[2].trim()));                                       // upgrade the new temperature of ester3 that calculated by the pr aspen plus model to ArcGIS databse
						}
						RadFracTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),RadFracAttributes);                          // update feature table locally
						break;
					}
				}
			}
			RadFracTable.applyEdits(null); // commit local updates onto server
//			RadFracTable.dispose();
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

//	to update the output of the AP+PW model
	/**read the AP+PW output csv file and update to ArcGIS database*/
	public void readAPPWCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(APPWOUTCSV));
			fileReader.readLine();       // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			GeodatabaseFeatureServiceTable ReactorTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Reactor/FeatureServer",user, 0);
			ReactorTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			ReactorTable.initialize();

			final CountDownLatch latch = new CountDownLatch(1);                                                 // ZL-151207 handles one asynchronous processes, only continuesThread when it reaches 0
			ReactorTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {                                               // Asynchronous callback: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown();                                                             // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			latch.await();                                                                                       // wait until all feature service tables are ready then continue

			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				//System.out.println("data= " + data);
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[7];

				for (int j = 0; j < 2; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					System.out.println(ArcGISOBJECTID);

					if (OBJECTIDtoReactor.get(j + 1).equals("10D01")) {                                                                     
						Map<String, Object> ReactorAttributes = ReactorTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[26].trim().isEmpty()) {
							ReactorAttributes.put("Heat_Duty",Float.parseFloat(data[26].trim())*1000);                                     // upgrade the heat duty of reactor 10D01 to ArcGIS  databse
						}
						System.out.println("HeatDuty="+Float.parseFloat(data[26].trim())*1000);
//updating the PW aspect of the APPW model						
						if (!data[112].trim().isEmpty()) {
							ReactorAttributes.put("V_pu",Float.parseFloat(data[112].trim()));                                             // upgrade the new per unit voltage of load point reactor 10D01 to ArcGIS  databse
						}
						System.out.println("V_pu="+ Float.parseFloat(data[112].trim()));						
						if (!data[113].trim().isEmpty()) {
							ReactorAttributes.put("theta_act",Float.parseFloat(data[113].trim()));                                       // upgrade the new bus angle of load point reactor 10D01 to ArcGIS  databse
						}
						System.out.println("theta_act="+ Float.parseFloat(data[113].trim()));
						if (!data[114].trim().isEmpty()) {
							ReactorAttributes.put("V_act_kv",Float.parseFloat(data[114].trim()));                                       // upgrade the new actual voltage of load point reactor 10D01 to ArcGIS  databse
						}
						System.out.println("V_act_kv="+ Float.parseFloat(data[114].trim()));
						ReactorTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),ReactorAttributes);                                // update feature table locally
					}
					if (OBJECTIDtoReactor.get(j + 1).equals("10D03")) {                                                                     
						Map<String, Object> ReactorAttributes = ReactorTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[23].trim().isEmpty()) {
							ReactorAttributes.put("Heat_Duty",Float.parseFloat(data[23].trim())*1000);                                // upgrade the heat duty of reactor 10D03 to ArcGIS  databse
						}
						System.out.println("HeatDuty="+Float.parseFloat(data[23].trim())*1000);
						if (!data[102].trim().isEmpty()) {
							ReactorAttributes.put("V_pu",Float.parseFloat(data[102].trim()));                                        // upgrade the new per unit voltage of load point reactor 10D03 to ArcGIS  databse
						}
						System.out.println("V_pu="+ Float.parseFloat(data[102].trim()));						
						if (!data[103].trim().isEmpty()) {
							ReactorAttributes.put("theta_act",Float.parseFloat(data[103].trim()));                                   // upgrade the new bus angle of load point reactor 10D03 to ArcGIS  databse
						}
						System.out.println("theta_act="+ Float.parseFloat(data[103].trim()));
						if (!data[104].trim().isEmpty()) {
							ReactorAttributes.put("V_act_kv",Float.parseFloat(data[104].trim()));                                    // upgrade the new actual voltage of load point reactor 10D03 to ArcGIS  databse
						}
						System.out.println("V_act_kv="+ Float.parseFloat(data[104].trim()));
						ReactorTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),ReactorAttributes);                             // update feature table locally
					}
				}
			}
			ReactorTable.applyEdits(null); // commit local updates onto server
//			ReactorTable.dispose();
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

//to update the output of the Pr AP+PW model
	/**read the Pr AP+PW output csv file and update to ArcGIS database*/
	public void readPrAPPWCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(PrAPPWOUTCSV));
			fileReader.readLine();       // Read the CSV flie header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL");
			
			GeodatabaseFeatureServiceTable MatLineTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Material_line/FeatureServer",user, 0);
			MatLineTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			/*System.out.println(1);
			System.out.println(MatLineTable.getFields());
			System.out.println(2);
			System.out.println(MatLineTable.getStatus());
			System.out.println(3);*/
			System.out.println(MatLineTable.initialize());
			//System.out.println(4);
			System.out.println(MatLineTable.getStatus());
			//System.out.println(5);
			MatLineTable.getInitializationError();
			//System.out.println(6);
			
			
			GeodatabaseFeatureServiceTable ReactorTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Reactor/FeatureServer",user, 0);
			ReactorTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
			System.out.println(ReactorTable.initialize());
			System.out.println(ReactorTable.getStatus());
			ReactorTable.getInitializationError();
			

			final CountDownLatch latch = new CountDownLatch(2);                                                 // ZL-151207 handles one asynchronous processes, only continuesThread when it reaches 0
			ReactorTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {                                               // Asynchronous callback: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown();                                                             // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			
			MatLineTable.populateFromService(loadAllFeatures, false,
					new CallbackListener<Boolean>() {
						@Override
						public void onCallback(Boolean status) {                                               // Asynchronous callback: code must wait for populate from service to finish loading features
							if (status == true) {
								latch.countDown();                                                             // latch decrement if feature service table is ready
							}
						}

						@Override
						public void onError(Throwable e) {
							e.printStackTrace();
						}
					});
			latch.await();                                                                                       // wait until all feature service tables are ready then continue

			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				//System.out.println("data= " + data);
				String[] ArcGISOBJECTID = null;
				ArcGISOBJECTID = new String[50];

				for (int j = 0; j < 2; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 197); //still use indication of object id to write value into the feature object; 197 number still follow the old "material line" feature.
					System.out.println(ArcGISOBJECTID[j]);
					
					if (CPIDtoMatLine.get(j + 61).equals("3-23")) {                                                                     
						Map<String, Object> MatLineAttributes = MatLineTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[0].trim().isEmpty()) {
							MatLineAttributes.put("Mat_4_qnt",Float.parseFloat(data[0].trim()));                                     // upgrade the heat duty of reactor 10D01 to ArcGIS  databse
						
						System.out.println("ok1");
						}
						System.out.println("FlowOut="+Float.parseFloat(data[0].trim()));
						if (!data[2].trim().isEmpty()) {
							MatLineAttributes.put("Mat_T",Float.parseFloat(data[2].trim()));                                     // upgrade the heat duty of reactor 10D01 to ArcGIS  databse
							System.out.println("ok2");
						}
						System.out.println("Flow_Temperature="+Float.parseFloat(data[2].trim()));
						MatLineTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),MatLineAttributes);                                // update feature table locally
						System.out.println(Long.parseLong(ArcGISOBJECTID[j]));
					}
				}
				
				for (int j = 0; j < 20; j++) {
					ArcGISOBJECTID[j] = String.valueOf(j + 1);
					
					if (OBJECTIDtoReactor.get(j + 1).equals("R-301")) {                                                                     
						Map<String, Object> ReactorAttributes = ReactorTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[25].trim().isEmpty()) {
							ReactorAttributes.put("Heat_Duty",Float.parseFloat(data[25].trim())*1000);                                     // upgrade the heat duty of reactor 10D01 to ArcGIS  databse
						}
						System.out.println("HeatDuty="+Float.parseFloat(data[25].trim())*1000);
//updating the PW aspect of the APPW model						
						if (!data[111].trim().isEmpty()) {
							ReactorAttributes.put("V_pu",Float.parseFloat(data[111].trim()));                                             // upgrade the new per unit voltage of load point reactor 10D01 to ArcGIS  databse
						}
						System.out.println("V_pu="+ Float.parseFloat(data[111].trim()));						
						if (!data[112].trim().isEmpty()) {
							ReactorAttributes.put("theta_act",Float.parseFloat(data[112].trim()));                                       // upgrade the new bus angle of load point reactor 10D01 to ArcGIS  databse
						}
						System.out.println("theta_act="+ Float.parseFloat(data[112].trim()));
						if (!data[113].trim().isEmpty()) {
							ReactorAttributes.put("V_act_kv",Float.parseFloat(data[113].trim()));                                       // upgrade the new actual voltage of load point reactor 10D01 to ArcGIS  databse
						}
						System.out.println("V_act_kv="+ Float.parseFloat(data[113].trim()));
						ReactorTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),ReactorAttributes);                                // update feature table locally
						
					}
					
					
					
					
					
					if (OBJECTIDtoReactor.get(j + 1).equals("R-302")) {                                                                     
						Map<String, Object> ReactorAttributes = ReactorTable.getFeature(Long.parseLong(ArcGISOBJECTID[j])).getAttributes();
						if (!data[23].trim().isEmpty()) {
							ReactorAttributes.put("Heat_Duty",Float.parseFloat(data[23].trim())*1000);                                // upgrade the heat duty of reactor 10D03 to ArcGIS  databse
						}
						System.out.println("HeatDuty="+Float.parseFloat(data[23].trim())*1000);
						if (!data[101].trim().isEmpty()) {
							ReactorAttributes.put("V_pu",Float.parseFloat(data[101].trim()));                                        // upgrade the new per unit voltage of load point reactor 10D03 to ArcGIS  databse
						}
						System.out.println("V_pu="+ Float.parseFloat(data[101].trim()));						
						if (!data[102].trim().isEmpty()) {
							ReactorAttributes.put("theta_act",Float.parseFloat(data[102].trim()));                                   // upgrade the new bus angle of load point reactor 10D03 to ArcGIS  databse
						}
						System.out.println("theta_act="+ Float.parseFloat(data[102].trim()));
						if (!data[103].trim().isEmpty()) {
							ReactorAttributes.put("V_act_kv",Float.parseFloat(data[103].trim()));                                    // upgrade the new actual voltage of load point reactor 10D03 to ArcGIS  databse
						}
						System.out.println("V_act_kv="+ Float.parseFloat(data[103].trim()));
						ReactorTable.updateFeature(Long.parseLong(ArcGISOBJECTID[j]),ReactorAttributes);                             // update feature table locally
					}
				}
			}
			ReactorTable.applyEdits(null); // commit local updates onto server
			MatLineTable.applyEdits(null);
			
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

//end of updating the output of the Pr AP+PW model		
	
// For this function, you can also do the modification but it is a little bit tricky. For the first for loop, it runs for 178 times, 
// if you can break the 178 iterations into several (3-5) Threads, it will increase the efficiency
    /**this method collects all the input data for the Pr PowerWorld model and write to a csv file*/
	public void writePrPWCSV(ArrayList<Map<String, Object>> attributeslist_LP, ArrayList<Map<String, Object>> attributeslist_PG) {

		FileWriter fW_LP_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from loadpoint layer.
		FileWriter fW_LP_Q = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from loadpoint layer.
		FileWriter fW_PG_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
		FileWriter fW_PG_Q = null; // create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

		try {
			fW_LP_P = new FileWriter(LPPIN);
			fW_LP_Q = new FileWriter(LPQIN);
			fW_PG_P = new FileWriter(PGPIN);
			fW_PG_Q = new FileWriter(PGQIN);

			fW_LP_P.append("   ,FID, pwr_P, pwr_Q, pwr_P_act, pwr_Q_act");
			fW_LP_P.append("\n");
			for (int i = 0; i < attributeslist_LP.size(); i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
		
			if (i == 178) {				
					break;
				}
				for (String key : attributeslist_LP.get(i).keySet()) { // access all feature fields ("key") of the Load_Point  ("LP") layer
					if (key == "OBJECTID") {
						fW_LP_P.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
						fW_LP_P.append(","); // ZL-151218
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
						fW_LP_P.append(", ");
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get("pwr_P"))); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
						fW_LP_P.append(", ");
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get("pwr_Q")));
						fW_LP_P.append(", ");
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get("pwr_P_act")));
						fW_LP_P.append(", ");
						fW_LP_P.append(String.valueOf(attributeslist_LP.get(i).get("pwr_Q_act")));
						fW_LP_P.append("\n");

						fW_LP_Q.append(key); // add the header "FID" to the data stream of the file that will contain the pwr_Q value.
						fW_LP_Q.append(","); // ZL-151218
						fW_LP_Q.append(String.valueOf(attributeslist_LP.get(i).get(key))); // capture the FID value, convert it to a string and add it to the data stream
						fW_LP_Q.append(", ");
						fW_LP_Q.append(String.valueOf(attributeslist_LP.get(i).get("pwr_Q"))); // capture the pwr_Q value that corresponds to the FID, convert it to a string and add it to the data  stream
						fW_LP_Q.append("\n");
					}
				}
			}

			for (int i = 0; i < attributeslist_PG.size(); i++) { // iterate over all members of the PowerGen attributeslist (total of 5), which contains information on multiple graphic elements
				if (i == 5) {
					break;
				}
				for (String key : attributeslist_PG.get(i).keySet()) { // access all feature fields ("key") of the PowerGen ("PG") layer
					if (key == "OBJECTID") {
						fW_PG_P.append(key); // add key to the data stream
						fW_PG_P.append(",");
						fW_PG_P.append(String.valueOf(attributeslist_PG.get(i).get(key))); // convert Object to a string and add to the data stream
						fW_PG_P.append(", ");
						fW_PG_P.append(String.valueOf(attributeslist_PG.get(i).get("PowerGenMW"))); // convert Object to a string and add to the data stream
						fW_PG_P.append("\n");
						fW_PG_Q.append(key); // add key to the data stream
						fW_PG_Q.append(",");
						fW_PG_Q.append(String.valueOf(attributeslist_PG.get(i).get(key))); // convert Object to a string and add to the data stream
						fW_PG_Q.append(", ");
						fW_PG_Q.append(String.valueOf(attributeslist_PG.get(i).get("PowerGenMV"))); // convert Object to a string and add to the data stream
						fW_PG_Q.append("\n");
					}
				}
			}
			fW_LP_P.flush(); // passes the data from fW_LP to LPIN.csv
			fW_LP_Q.flush(); // passes the data from fW_LP to LPIN.csv
			fW_PG_P.flush(); // passes the data from fW_PG to PGIN.csv
			fW_PG_Q.flush(); // passes the data from fW_PG to PGIN.csv
			fW_LP_P.close();
			fW_LP_Q.close();
			fW_PG_P.close();
			fW_PG_Q.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**this method collects all the input data for the PowerWorld model and write to a csv file*/
	public void writeCSV(ArrayList<String[]> skeleton, ArrayList<Map<String, Object>> attributeslist, ArrayList<String> layers) { // write input file to python

		FileWriter fW_PWIN = null; // create an object later used for writing a .csv file for PW

		try {
			fW_PWIN = new FileWriter(INCSV);
			for (int i = 0; i < skeleton.size(); i++) { // for each entry in editStack
				fW_PWIN.append(skeleton.get(i)[0]); // write headers (PowerWorld field names)
				fW_PWIN.append("\n"); // new line
				String[] ArcGISfields = skeleton.get(i)[1].split(","); // produce iterable list from comma separated string (e.g. ["FID", "volt_nom"])
				Map<String, Object> attributes = attributeslist.get(i); // pulls all data fields available about all (i) modified ArcGIS objects (e.g. (i=0) loadpoints, (i=1) buildings) from ArrayList "attributeslist" and stores them in the Map "attributes", where the Map represents a value-pair  consisting of <String, Object>.
				// System.out.println("attributes(" + i + ")=" + attributes);
				String layer = layers.get(i);

				for (int j = 0; j < ArcGISfields.length; j++) { // for each ArcGIS field, append corresponding  values
					if (layer.equals("Load_Points")) { // specific to load point
						if (ArcGISfields[j].equals("OBJECTID")) { // ArcGIS element is FID
							String ArcGISFID = String.valueOf(attributes.get("OBJECTID")); // String.valueOf() converts any data type (in this case an integer) to string
							fW_PWIN.append(ArcGISFIDtoPWBusNum.get(ArcGISFID)); // converts FID to BusNum using the hard coded map
						} else if (ArcGISfields[j].equals("LoadID")) {
							fW_PWIN.append("1"); // LoadID is 1 by default in the jparksimulator.pwb
						} else {
							fW_PWIN.append(String.valueOf(Float.parseFloat(String.valueOf(attributes.get(ArcGISfields[j]))) * 1.0)); // float values from ArcGIS converted to string
						}
					} else if (layer.equals("UHT_Substation_(230_66kV)")) {
						String substationID = "UHT"+ String.valueOf(attributes.get("OBJECTID"));
						String HVNomVolt = SubstationtoPWBusNum.get(substationID + "HV");
						String LVNomVolt = SubstationtoPWBusNum.get(substationID + "LV");
						if (ArcGISfields[j].equals("OBJECTID")) {
							if (ArcGISfields[j + 1].equals("HV_kV")) { // check if current entry is looking for HV or LV bus
								fW_PWIN.append(HVNomVolt);
							} else {
								fW_PWIN.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")
								|| ArcGISfields[j].equals("LV_kV")) { // input value from ArcGIS
							fW_PWIN.append(String.valueOf(attributes
									.get(ArcGISfields[j])));
						}
					} else if (layer.equals("EHT_Substation_(66_22kV)")) { // essentially same as UHT substation
						String substationID = "EHT" + String.valueOf(attributes.get("OBJECTID"));
						String HVNomVolt = SubstationtoPWBusNum .get(substationID + "HV");
						String LVNomVolt = SubstationtoPWBusNum .get(substationID + "LV");
						if (ArcGISfields[j].equals("OBJECTID")) {
							if (ArcGISfields[j + 1].equals("HV_kV")) {
								fW_PWIN.append(HVNomVolt);
							} else {
								fW_PWIN.append(LVNomVolt);
							}
						} else if (ArcGISfields[j].equals("HV_kV")
								|| ArcGISfields[j].equals("LV_kV")) {
							fW_PWIN.append(String.valueOf(attributes .get(ArcGISfields[j])));
						}
					} else if (layer.equals("PowerGen")) {
						if (ArcGISfields[j].equals("OBJECTID")) {
							int BusNum = (int) attributes.get("OBJECTID") + 4; // special case where PowerWorld bus number is ArcGIS FID + 4
							fW_PWIN.append(String.valueOf(BusNum));
						} else if (ArcGISfields[j].equals("GenID")) {
							fW_PWIN.append("1"); // GenID is 1 by default in jparksimulator.pwb
						} else {
							fW_PWIN.append(String.valueOf(attributes.get(ArcGISfields[j]))); // GenMW
						}
					}
					//
					// CONTINUE ELSE IF FOR OTHER LAYERS
					//
					fW_PWIN.append(","); // separate values with a comma
				}
				fW_PWIN.append("\n");
				fW_PWIN.append(skeleton.get(i)[2]); // PW object type (e.g. BUS, GEN, LOAD)
				fW_PWIN.append("\n"); // each item is given three rows: headers (name of attributes), corresponding  values and type of object (e.g. BUS, GEN, etc)

			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fW_PWIN.flush();
				fW_PWIN.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
/**this method reads the csv file generated by the powerworld model and update to ArcGIS database */
	public void readCSV() {
		BufferedReader fileReader = null;
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
		try {
			long start = System.currentTimeMillis(); // start a timer
			String line = null;
			fileReader = new BufferedReader(new FileReader(BUSCSV));
			fileReader.readLine(); // Read the CSV file header to skip it
			QueryParameters loadAllFeatures = new QueryParameters();
			loadAllFeatures.setWhere("OBJECTID IS NOT NULL"); // Load all features using SQL command

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
			//
			// EXPAND CODE HERE
			//

			latch.await(); // wait until all feature service tables are ready then continue
			while ((line = fileReader.readLine()) != null) { // Continue reading lines until none left
				String[] data = line.split(","); // split string by comma

				// data[0]=BUSNUM,
				// data[1]=BUSNAME,
				// data[2]=BUSLOADMW,
				// data[3]=BUSLOADMVR,
				// data[4]=BUSNOMVOLT,
				// data[5]=BUSKVVOLT,
				// data[6]=BUSANGLE,
				// data[7]=BUSGENMW,
				// data[8]=BUSGENMVR

				int PWBusNum = Integer.valueOf(data[0].trim()); // data[0] is the bus number
				String ArcGISFID = PWBusNumtoArcGISFID.get(data[0].trim()); // .trim() removes trailing white spaces

				if (ArcGISFID != null) { // if PowerWorld bus number can map to an ArcGIS LoadPoint FID
					Map<String, Object> LoadPointsAttributes = LoadPointsTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
					if (!data[2].trim().isEmpty()) { // if not an empty string. Overwrite values using attributes.put()

						LoadPointsAttributes.put("pwr_P_act",Float.parseFloat(data[2].trim())); // BUSLOADMW
					}
					if (!data[3].trim().isEmpty()) {
						LoadPointsAttributes.put("pwr_Q_act",Float.parseFloat(data[3].trim())); // BUSLOADMVR
					}
					if (!data[4].trim().isEmpty()) {
						LoadPointsAttributes.put("volt_nom",Float.parseFloat(data[4].trim())); // BUSNOMVOLT
					}
					if (!data[5].trim().isEmpty()) {
						LoadPointsAttributes.put("volt_act",Float.parseFloat(data[5].trim())); // BUSKVVOLT
					}
					if (!data[6].trim().isEmpty()) {
						LoadPointsAttributes.put("theta_act",Float.parseFloat(data[6].trim())); // BUSANGLE
					}
					LoadPointsTable.updateFeature(Long.parseLong(ArcGISFID),LoadPointsAttributes); // update feature table locally
				}
				if (PWBusNum >= 5 && PWBusNum <= 9) { // PowerGen buses
					Map<String, Object> PowerGenAttributes = PowerGenTable.getFeature((long) (PWBusNum - 4)).getAttributes(); // subtract 4 from  PWBusNum to get  FID
					if (!data[4].trim().isEmpty()) {
						PowerGenAttributes.put("volt_nom", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
					} 
					if (!data[5].trim().isEmpty()) {
						PowerGenAttributes.put("volt_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT
					}
					if (!data[6].trim().isEmpty()) {
						PowerGenAttributes.put("theta", Float.parseFloat(data[6].trim())); // BUSANGLE
					}
					if (!data[7].trim().isEmpty()) {
						PowerGenAttributes.put("PowerGenMW", Float.parseFloat(data[7].trim())); // BUSGENMW
					}
					if (!data[8].trim().isEmpty()) {
						PowerGenAttributes.put("PowerGenMVR", Float.parseFloat(data[8].trim())); // BUSGENMVR
					}
					// PowerGenTable.updateFeature((long) (PWBusNum-4),
					// PowerGenAttributes);
				}
				if ((PWBusNum >= 2 && PWBusNum <= 4) || (PWBusNum >= 10 && PWBusNum <= 12)) { // UHTSubstation
					String SubstationFID = PWBusNumtoSubstation.get(String.valueOf(PWBusNum));
					Map<String, Object> UHTSubstationAttributes = UHTSubstationTable.getFeature(Long.parseLong(SubstationFID)).getAttributes();
					if (!data[4].trim().isEmpty()) {
						if (PWBusNum >= 2 && PWBusNum <= 4) { // high voltage bus
							UHTSubstationAttributes.put("HV_kV", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
						} else { // low voltage bus
							UHTSubstationAttributes.put("LV_kV", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
						}
					}
					if (!data[5].trim().isEmpty()) {
						if (PWBusNum >= 2 && PWBusNum <= 4) {
							UHTSubstationAttributes.put("HV_kV_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT
						} else {
							UHTSubstationAttributes.put("LV_kV_act",
									Float.parseFloat(data[5].trim())); // BUSKVVOLT
						}
					}
					if (!data[6].trim().isEmpty()) {
						if (PWBusNum >= 2 && PWBusNum <= 4) {
							UHTSubstationAttributes.put("HV_theta", Float.parseFloat(data[6].trim())); // BUSANGLE
						} else {
							UHTSubstationAttributes.put("LV_theta", Float.parseFloat(data[6].trim())); // BUSANGLE
						}
					}
					UHTSubstationTable.updateFeature( Long.parseLong(SubstationFID), UHTSubstationAttributes);
				}
				if (PWBusNum >= 13 && PWBusNum <= 30) { // EHT Substation
					String SubstationFID = PWBusNumtoSubstation.get(String.valueOf(PWBusNum));
					Map<String, Object> EHTSubstationAttributes = EHTSubstationTable.getFeature(Long.parseLong(SubstationFID)).getAttributes();
					if (!data[4].trim().isEmpty()) {
						if (PWBusNum >= 13 && PWBusNum <= 21) { // high voltage bus
							EHTSubstationAttributes.put("HV_kV", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
						} else { // low voltage bus
							EHTSubstationAttributes.put("LV_kV", Float.parseFloat(data[4].trim())); // BUSNOMVOLT
						}
					}
					if (!data[5].trim().isEmpty()) {
						if (PWBusNum >= 13 && PWBusNum <= 21) {
							EHTSubstationAttributes.put("HV_kV_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT
						} else {
							EHTSubstationAttributes.put("LV_kV_act", Float.parseFloat(data[5].trim())); // BUSKVVOLT
						}
					}
					if (!data[6].trim().isEmpty()) {
						if (PWBusNum >= 13 && PWBusNum <= 21) {
							EHTSubstationAttributes.put("HV_theta", Float.parseFloat(data[6].trim())); // BUSANGLE
						} else {
							EHTSubstationAttributes.put("LV_theta", Float.parseFloat(data[6].trim())); // BUSANGLE
						}
					}
					EHTSubstationTable.updateFeature(
							Long.parseLong(SubstationFID),
							EHTSubstationAttributes);
				}
			}
			LoadPointsTable.applyEdits(null); // commit local updates onto server
			PowerGenTable.applyEdits(null);
			UHTSubstationTable.applyEdits(null);
			EHTSubstationTable.applyEdits(null);
//			LoadPointsTable.dispose(); // commit local updates onto server
//			PowerGenTable.dispose();
//			UHTSubstationTable.dispose();
//			EHTSubstationTable.dispose();
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
	
	

	
//multi-thread 	
	class Set_MX extends Thread{
		@Override 
		public void run()
		{
			UserCredentials user = new UserCredentials();
			user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
			ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); 
			for (int key : OBJECTIDtoMXNum.keySet()) {
				System.out.println(key);
				try {
					QueryParameters qParameter_MX = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
					qParameter_MX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
					qParameter_MX.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
					QueryTask qTask_MX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_MX = null; // create an instance of Feature to store an ArcGIS element

					qTask_MX = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

					FeatureResult fResult_MX = qTask_MX.execute(qParameter_MX); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
					graphic_MX = (Feature) fResult_MX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
					attributeslist_MX.add(graphic_MX.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
				}
			}

		}
	
	}
	
	
	class Set_HX extends Thread
	{
		@Override 
		public void run()
		{
			UserCredentials user = new UserCredentials();
			user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
			ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); 
			for (Integer key : OBJECTIDtoHXNum.keySet()) {
				try {
					QueryParameters qParameter_HX = new QueryParameters(); // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
					qParameter_HX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
					qParameter_HX.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
					QueryTask qTask_HX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_HX = null; // create an instance of Feature to store an ArcGIS element

					qTask_HX = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
					FeatureResult fResult_HX = qTask_HX.execute(qParameter_HX); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
					graphic_HX = (Feature) fResult_HX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
					attributeslist_HX.add(graphic_HX.getAttributes()); // append information about the  element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
				}
			}
	
		}
	
	}
	
	
	class Set_CR extends Thread
	{
		@Override 
		public void run()
		{
			UserCredentials user = new UserCredentials();
			user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
			ArrayList<Map<String, Object>> attributeslist_CR = new ArrayList<Map<String, Object>>(); 
			for (Integer key : OBJECTIDtoCRNum.keySet()) {
				try {
					QueryParameters qParameter_CR = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
					qParameter_CR.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
					qParameter_CR.setOutFields(new String[] { "*" }); // fetch all  attributes of an ArcGIS element using *
					QueryTask qTask_CR = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_CR = null; // create an instance of Feature to store an ArcGIS element

					qTask_CR = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Reactor/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
					FeatureResult fResult_CR = qTask_CR.execute(qParameter_CR); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and  qTask_LP
					graphic_CR = (Feature) fResult_CR.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP  and stores it in graphic_LP; qParameter_LP requests information about a single element only
					attributeslist_CR.add(graphic_CR.getAttributes()); // append information  about the  element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
				}
			}
		}
		}

			class Set_SP extends Thread
			{
				@Override 
				public void run()
				{
					UserCredentials user = new UserCredentials();
					user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
					ArrayList<Map<String, Object>> attributeslist_SP = new ArrayList<Map<String, Object>>(); 
			for (Integer key : OBJECTIDtoSPNum.keySet()) {
				try {
					QueryParameters qParameter_SP = new QueryParameters(); // create an instance of QueryParameters to be  used for querying ArcGIS database for predefined data
					qParameter_SP.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
					qParameter_SP.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
					QueryTask qTask_SP = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_SP = null; // create an instance of Feature to store an ArcGIS element

					qTask_SP = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Flashdrum/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
					FeatureResult fResult_SP = qTask_SP.execute(qParameter_SP); // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP and qTask_LP
					graphic_SP = (Feature) fResult_SP.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element  only
					attributeslist_SP.add(graphic_SP.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
				}
			}

		}

	}
	
			class Set_DC extends Thread
			{
				@Override 
				public void run()
				{
					UserCredentials user = new UserCredentials();
					user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
					ArrayList<Map<String, Object>> attributeslist_DC = new ArrayList<Map<String, Object>>(); 
					for (Integer key : OBJECTIDtoDCNum.keySet()) {
						try {
							QueryParameters qParameter_DC = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database  for  predefined data
							qParameter_DC.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
							qParameter_DC.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
							QueryTask qTask_DC = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
							Feature graphic_DC = null; // create an instance of Feature to store an ArcGIS element

							qTask_DC = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Decanter/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
							FeatureResult fResult_DC = qTask_DC.execute(qParameter_DC); // FeatureResult is used to store information from ArcGIS database  requested  using qParameter_LP and qTask_LP
							graphic_DC = (Feature) fResult_DC.iterator().next(); // queryResult.iterator() iterates over the elements  in fResult_LP and  stores it in graphic_LP; qParameter_LP requests information  about a  single  element only
							attributeslist_DC.add(graphic_DC.getAttributes()); // append information about the element in graphic_LP  to ArrayList attributeslist_LP

						} catch (Exception e) {
							e.printStackTrace(); // It prints the stack trace of the  Exception to System.err. It's a very  simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
						}
					}
		}
	}	
		
}

