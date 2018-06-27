package uk.ac.cam.cares.jps.arbitragetest;

import java.io.IOException;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.util.Map;

import org.apache.http.client.ClientProtocolException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestMarketPackage extends TestCase {
	private static Logger logger = LoggerFactory
			.getLogger(TestMarketPackage.class);
	
	class HeaderValues {
		String[] arrayHeader;
		String[] arrayMonths;
		String[] arrayDatetime;
		String[] arrayPrices;
	}
	
	/**
	 * this function calls DataDownload to call
	 * DataDownload.Downloading_market_data and check that
	 * the returned string contains a predefined substring
	 * 
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public void testDownloadingAndSavingMarketDataInTheKnowledgeBaseBiodiesel()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/downloadingAndSavingMarketDataInTheKnowledgeBase";
		String actual = AgentCaller.executeGet(path, "choicePlant", "Biodiesel");
		logger.info(actual);
		
		Gson g = new Gson();
		
		String marketDataCpoFame = g.fromJson(actual, String.class);		
		String[] marketDataCpoFameArray = g.fromJson(marketDataCpoFame, String[].class);
		
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listType = new TypeToken<Map<String, String[]>>(){}.getType();
		
		Map<String, String[]> cpo = objGson.fromJson(marketDataCpoFameArray[0], listType);
		Map<String, String[]> fame = objGson.fromJson(marketDataCpoFameArray[1], listType);
		
		assertEquals(cpo.get("arrayHeader")[0], "CPO");
		assertEquals(fame.get("arrayHeader")[0], "FAME");
	}
	
	public void testDownloadingAndSavingMarketDataInTheKnowledgeBaseMethanol()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/downloadingAndSavingMarketDataInTheKnowledgeBase";
		String actual = AgentCaller.executeGet(path, "choicePlant", "Methanol");
		logger.info(actual);
		
		Gson g = new Gson();
		
		String marketDataMeohNg = g.fromJson(actual, String.class);		
		String[] marketDataMeohNgArray = g.fromJson(marketDataMeohNg, String[].class);
		
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listType = new TypeToken<Map<String, String[]>>(){}.getType();
		
		Map<String, String[]> meoh = objGson.fromJson(marketDataMeohNgArray[0], listType);
		Map<String, String[]> ng = objGson.fromJson(marketDataMeohNgArray[1], listType);
		
		assertEquals(meoh.get("arrayHeader")[0], "MeOH");
		assertEquals(ng.get("arrayHeader")[0], "NG");
	}

	/**
	 * this function calls DataDownload to call
	 * DataDownload.Downloading_currencies and check that
	 * the returned string contains a predefined substring
	 * 
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public void testDownloadingAndSavingExchangeRatesInTheKnowledgeBase()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/downloadingAndSavingExchangeRatesInTheKnowledgeBase";
		String actual = AgentCaller.executeGet(path,
				"whatver", "whatever2");
		logger.info(actual);
		assertTrue(actual.contains("USD"));

	}

	/**
	 * this function calls DataDownload to call
	 * DataDownload.Call_data and check that the returned
	 * string contains a predefined substring
	 * 
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public void testRetrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Costs_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";
		String actual = AgentCaller.executeGet(path, key,
				value);
		logger.info(actual);
		assertTrue(actual.contains(
				"JAN 2021,FEB 2021,MAR 2021,APR 2021"));

	}

	/**
	 * this function calls DataDownload to call
	 * DataDownload.Call_data and check that the returned
	 * string contains a predefined substring
	 * 
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public void testRetrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_Storage_NaturalGas_001,V_Price_CoolingWater_001,V_Price_Storage_Methanol_001,V_Price_Electricity_001,V_Price_Transport_SG-SC_Methanol_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_Transport_USGC-NEA_NaturalGas_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_FuelGas_001";
		String actual = AgentCaller.executeGet(path, key,
				value);
		logger.info(actual);
		assertTrue(actual.contains("MA902,MA903,MA904"));

	}

}
