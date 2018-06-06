package uk.ac.cam.cares.jps.arbitrage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import com.cmclinnovations.mods.api.MoDSAPI;
import uk.ac.cam.cares.jps.arbitragetest.TestMoDSAnalysis;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

public class Arbitrage {

	/**
	 * this function calls cmd to execute a Python script
	 * which uses market prices of crude palm oil (CPO) and
	 * biodiesel (FAME) stored in CSV files and hardcoded
	 * utility prices and exchange rates to conduct the
	 * financial analysis, result of which is captured from
	 * cmd and printed to the eclipse console; the script
	 * also prints a plot of market data to the location
	 * defined below
	 * 
	 * this function is more or less obsolete, but was
	 * retained in case anyone would like to use Aspen for
	 * the arbitrage analysis
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unused")
	private static void runningAnalysisAspen()
			throws Exception {

		String CPO_to_FAME_analysis = new String(
				"caresjpsarbitrage/CPO_to_FAME.py");

		String market_data_plot = new String(
				"C:\\Users\\Janusz\\Desktop\\JParkSimulator-git\\JPS_Arbitrage\\workingdir\\arbitrage_CPO.png");

		String result = PythonHelper.callPython(
				CPO_to_FAME_analysis, market_data_plot,
				new Arbitrage());
		System.out.println(result);

	}

	/**
	 * this function uses MoDS-Java API to evaluate the
	 * pre-generated MoDS surrogate model stored a location
	 * defined below, converts MoDS output values into a
	 * string, which is passed to a Python script, and calls
	 * cmd to execute the Python script which uses market
	 * prices of crude palm oil (CPO) and biodiesel (FAME)
	 * stored in CSV files and hardcoded utility prices and
	 * exchange rates to conduct the financial analysis,
	 * result of which is printed to the eclipse console and
	 * returned as a string
	 * 
	 * 
	 * @param input
	 * @return
	 * @throws Exception
	 */
	public static String runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles(
			String input) throws Exception {
		
	
		String[] sim_address = {
				AgentLocator.getCurrentJpsAppDirectory(new Arbitrage()) + "/MoDS/HDMR_50_001",
				"HDMR_Alg_1" };	
//		String[] sim_address = {
//				"C:\\Users\\Janusz\\Desktop\\JParkSimulator-git\\JPS_Arbitrage\\MoDS\\HDMR_50_001",
//				"HDMR_Alg_1" };
		// Double[] inputs = {24220.0656};
		Double[] raw_materials = {
				Double.parseDouble(input) };
		List<Double> data = MoDS(raw_materials,
				sim_address);

		String result = raw_materials[0].toString();
		for (int i = 0; i < data.size(); i++) {
			result += "," + data.get(i).toString();
		}

		String CPO_to_FAME_analysis = new String(
				"caresjpsarbitrage/CPO_to_FAME_MoDS.py");
		String result1 = PythonHelper.callPython(
				CPO_to_FAME_analysis, result,
				new Arbitrage());
		System.out.println(result1);
		return result1;

	}

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
				AgentLocator.getCurrentJpsAppDirectory(new Arbitrage()) + "/MoDS/HDMR_50_001",
				"HDMR_Alg_1" };	
//		String[] sim_address = {
//				"C:\\Users\\Janusz\\Desktop\\JParkSimulator-git\\JPS_Arbitrage\\MoDS\\HDMR_50_001",
//				"HDMR_Alg_1" };
		// Double[] inputs = {24220.0656};
		Double[] raw_materials = {
				Double.parseDouble(input) };
		List<Double> MoDS_data = MoDS(raw_materials,
				sim_address);

		String result = raw_materials[0].toString();
		for (int i = 0; i < MoDS_data.size(); i++) {
			result += "," + MoDS_data.get(i).toString();
		}

		String path = "/JPS_Arbitrage/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_Price_MediumPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";
		String actual = TestMoDSAnalysis.executeGet(path,
				key, value);
		System.out.println(actual);
		System.out.println(result);

		String CPO_to_FAME_analysis = new String(
				"caresjpsarbitrage/CPO_to_FAME_MoDS2.py");
		String result1 = PythonHelper.callPython(
				CPO_to_FAME_analysis, result, actual,
				new Arbitrage());
		System.out.println(result1);

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
		// "C:\\Users\\Janusz\\Desktop\\JParkSimulator-git\\JPS_Arbitrage\\MoDS\\HDMR_50_001";
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

	public static void main(String[] args)
			throws Exception {
	}
}
