package uk.ac.cam.cares.jps.arbitrage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

public class Arbitrage {
	private static Logger logger = LoggerFactory.getLogger(Arbitrage.class);

	/**
	 * this function uses MoDS-Java API to evaluate the
	 * pre-generated MoDS surrogate model stored a location
	 * defined below, converts MoDS output values into a
	 * string, which is passed to a Python script, calls
	 * DataDownloadAgent via Tomcat server to retrieve
	 * market prices of crude palm oil (CPO) and biodiesel
	 * (FAME), utility prices and exchange rates from JPS
	 * knowledge base, which are passed to the Python script
	 * as a string, and calls cmd to execute the Python
	 * script which conducts the financial analysis, result
	 * of which is printed to the eclipse console and
	 * returned as a string
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	public static String runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent(
			String input) throws Exception {

		String[] sim_address = {
				AgentLocator.getCurrentJpsAppDirectory(new Arbitrage())	+ "/MoDS/HDMR_50_001",
				"HDMR_Alg_1" };
		
		Gson g = new Gson();
		Double[] raw_materials = g.fromJson(input, Double[].class);
		List<Double> MoDS_data = MoDS(raw_materials, sim_address);
		
		//parameter1
		String result = raw_materials[0].toString();
		for (int i = 0; i < MoDS_data.size(); i++) {
			result += "," + MoDS_data.get(i).toString();
		}
		
		String path = "/JPS_ARBITRAGE/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_Price_MediumPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";
		String actual = g.fromJson(AgentCaller.executeGet(path, key, value), String.class);
		logger.info(actual);
		logger.info(result);
		
		//parameters
		String[] arrayActual = g.fromJson(actual, String[].class);
		String miscCosts = arrayActual[0];
		String cpoPrices = arrayActual[1];
		String famePrices = arrayActual[2];
		
		JSONObject misccostjobj= new JSONObject(miscCosts);
		Iterator<String> keys = misccostjobj.keys();
		while(keys.hasNext()) {
		    String keyofjson = keys.next();
		    System.out.println("valuesnotif= "+keyofjson);
		    System.out.println("values= "+misccostjobj.getString(keyofjson));
		    if (misccostjobj.getString(keyofjson).contains("^^")) {

		    	String extracted=misccostjobj.getString(keyofjson).replace("^^","-");
		    	System.out.println("now= "+extracted.split("-")[0]);
		    	misccostjobj.put(keyofjson, extracted.split("-")[0]);
		              
		    }
		}
		
		//path of the pythonscript
		String CPO_to_FAME_analysis = new String("caresjpsarbitrage/CPO_to_FAME_MoDS2.py");
		String result1 = PythonHelper.callPython(CPO_to_FAME_analysis, result, g.toJson(misccostjobj.toString()), g.toJson(cpoPrices), g.toJson(famePrices), new Arbitrage());

		logger.info(result1);

		return result1;

	}

	/**
	 * this function uses MoDS-Java API to evaluate the
	 * pre-generated MoDS surrogate model stored a location
	 * defined below, converts MoDS output values into a
	 * string, which is passed to a Python script, calls
	 * DataDownloadAgent via Tomcat server to retrieve
	 * market prices of natural gas (HNG) and methanol
	 * (ZCE), utility prices and exchange rates from JPS
	 * knowledge base, which are passed to the Python script
	 * as a string, and calls cmd to execute the Python
	 * script which conducts the financial analysis, result
	 * of which is printed to the eclipse console and
	 * returned as a string
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	public static String runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2(
			String input) throws Exception {

		String[] sim_address = {
				AgentLocator.getCurrentJpsAppDirectory(new Arbitrage())	+ "/MoDS/HDMR_0%2E01_001",
				"HDMR_Alg_1" };
		
		Gson g = new Gson();
		Double[] raw_materials = g.fromJson(input, Double[].class);
		List<Double> MoDS_data = MoDS(raw_materials, sim_address);

		String result = raw_materials[0].toString();
		for (int i = 0; i < MoDS_data.size(); i++) {
			result += "," + MoDS_data.get(i).toString();
		}
		
		String path = "/JPS_ARBITRAGE/retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_CoolingWater_001,V_Price_Storage_NaturalGas_001,V_Price_Storage_Methanol_001,V_Price_Transport_USGC-NEA_NaturalGas_001,V_Price_Electricity_001,V_USD_to_SGD,V_USD_to_CNY,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_Price_MediumPressureSteam_001,V_Price_Transport_SG-SC_Methanol_001,V_Price_FuelGas_001";
		String actual = g.fromJson(AgentCaller.executeGet(path, key, value), String.class);
		logger.info(actual);
		logger.info(result);
		
		String[] arrayActual = g.fromJson(actual, String[].class);
		String miscCosts = arrayActual[0];
		String hngPrices = arrayActual[1];
		String zcePrices = arrayActual[2];
		
		JSONObject misccostjobj= new JSONObject(miscCosts);
		Iterator<String> keys = misccostjobj.keys();
		while(keys.hasNext()) {
		    String keyofjson = keys.next();
		    if (misccostjobj.getString(keyofjson).contains("^^")) {
		    	String extracted=misccostjobj.getString(keyofjson).replace("^^","-");
		    	misccostjobj.put(keyofjson, extracted.split("-")[0]);          
		    }
		}
		
		String NG_to_MeOH_analysis = new String("caresjpsarbitrage/NG_to_MeOH_MoDS.py");
		String result1 = PythonHelper.callPython(NG_to_MeOH_analysis, result, g.toJson(misccostjobj.toString()), g.toJson(hngPrices), g.toJson(zcePrices), new Arbitrage());
		logger.info(result1);
		
		return result1;

	}
	
	/**
	 * this function was based on an example provided by
	 * cmcl showing how to evaluate MoDS surrogate models
	 * using their MoDS-Java API; it is required to provide
	 * the API with location and name of the surrogate model
	 * and a List<Double> of input values; List<Double> of
	 * outputs is returned
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	public static List<Double> MoDS(final Double[] args,
			final String[] args1) {

		final String simDir = args1[0];
		final String surrogateAlgName = args1[1];

		// final String simDir =
		// "C:\\Users\\Janusz\\Desktop\\JParkSimulator-git\\JPS_ARBITRAGE\\MoDS\\HDMR_50_001";
		// final String surrogateAlgName = "HDMR_Alg_1";

		// Note that IndexOutOfBoundsException is thrown if
		// you supply too many inputs, but *no exception is
		// thrown if you supply too few!*.
		final List<Double> raw_materials = new ArrayList<>(
				Arrays.asList(args));

		// Evaluate the surrogate for a single set of
		// inputs. The result is a List<Double> of size
		// Noutputs
		List<Double> outputs1 = MoDSAPI.evaluateSurrogate(
				simDir, surrogateAlgName, raw_materials);
		return outputs1;
	}

}
