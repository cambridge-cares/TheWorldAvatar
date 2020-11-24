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
import uk.ac.cam.cares.jps.market.DataDownload;


public class TestMarketPackage extends TestCase {
	private static Logger logger = LoggerFactory
			.getLogger(TestMarketPackage.class);
	
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

		String path = "/JPS_ARBITRAGE/downloadingAndSavingMarketDataInTheKnowledgeBase";
		System.out.println(path);
		String actual = AgentCaller.executeGet(path, "choicePlant", "Biodiesel");
		logger.info(actual);
		System.out.println("actual result= "+actual);
		
		// Send HTTP get request to endpoint
//		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost:8080")
//				.setPath("/JPS_ARBITRAGE/downloadingAndSavingMarketDataInTheKnowledgeBase")
//				.setParameter("choicePlant", "Biodiesel");
//		
//		URI uri = builder.build();
//		HttpGet getRequest = new HttpGet(uri);
//		HttpClient httpClient = HttpClientBuilder.create().build();
//		HttpResponse httpResponse = httpClient.execute(getRequest);
//		
//		// Parse response into string
//		BufferedReader rd = new BufferedReader(new InputStreamReader(
//			    httpResponse.getEntity().getContent()));
//		
//		StringBuilder total = new StringBuilder();
//		String line = null;
//
//		while ((line = rd.readLine()) != null) {
//			total.append(line);
//		}
//		rd.close();
//		String actual = total.toString();	
//		
//		System.out.println(actual);
		
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
	
	public void testBio() throws Exception {
		
		DataDownload.downloadingAndSavingMarketDataInTheKnowledgeBase("Biodiesel");
		DataDownload.downloadingAndSavingMarketDataInTheKnowledgeBase("Methanol");
		
	}
	
	public void testRetrieve() throws Exception {
		
		System.out.println("MY TEST");
		
		Gson g = new Gson();
		
		String jsonString = "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_Price_MediumPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";

		try {
			String result = g.toJson(DataDownload
					.retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase(
							jsonString.split(",")));
			System.out.println(result);
		} catch (Exception e) {
			e.printStackTrace();
			throw e;
		}
	}
	
	public void testDownloadingAndSavingMarketDataInTheKnowledgeBaseMethanol()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_ARBITRAGE/downloadingAndSavingMarketDataInTheKnowledgeBase";
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

		String path = "/JPS_ARBITRAGE/downloadingAndSavingExchangeRatesInTheKnowledgeBase";
		String actual = AgentCaller.executeGet(path, "whatver", "whatever2");
		logger.info(actual);
		
		Gson g = new Gson();
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listType = new TypeToken<Map<String, String>>(){}.getType();
		
		Map<String, String> mapHeadersRates = objGson.fromJson(g.fromJson(actual, String.class), listType);
		String[] headers = mapHeadersRates.keySet().toArray(new String[0]);
		
		assertTrue(headers[0].contains("USD"));
		assertTrue(headers[1].contains("USD"));
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

		String path = "/JPS_ARBITRAGE/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Costs_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";
		String actual = AgentCaller.executeGet(path, key, value);
		logger.info(actual);
				
		Gson g = new Gson();
		
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listTypeStringString = new TypeToken<Map<String, String>>(){}.getType();
		Type listTypeStringArray = new TypeToken<Map<String, String[]>>(){}.getType();
				
		String[] utilitiesMarketdataArray = g.fromJson(g.fromJson(actual, String.class), String[].class);
		
		Map<String, String> utilities = objGson.fromJson(utilitiesMarketdataArray[0], listTypeStringString);
		Map<String, String[]> marketdataCpo = objGson.fromJson(utilitiesMarketdataArray[1], listTypeStringArray);
		Map<String, String[]> marketdataFame = objGson.fromJson(utilitiesMarketdataArray[2], listTypeStringArray);
		
		assertTrue(utilities.containsKey("V_Price_Storage_Biodiesel_001"));
		assertNotNull(utilities.get("V_Price_Storage_Biodiesel_001"));
				
		assertEquals(marketdataCpo.get("arrayHeader")[0], "CPO");
		assertEquals(marketdataFame.get("arrayHeader")[0], "FAME");
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

		String path = "/JPS_ARBITRAGE/retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase";
		String key = "individuals";
		String value = "V_Price_Storage_NaturalGas_001,V_Price_CoolingWater_001,V_Price_Storage_Methanol_001,V_Price_Electricity_001,V_Price_Transport_SG-SC_Methanol_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_Transport_USGC-NEA_NaturalGas_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_FuelGas_001";
		String actual = AgentCaller.executeGet(path, key, value);
		logger.info(actual);
		
		Gson g = new Gson();
		
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listTypeStringString = new TypeToken<Map<String, String>>(){}.getType();
		Type listTypeStringArray = new TypeToken<Map<String, String[]>>(){}.getType();
				
		String[] utilitiesMarketdataArray = g.fromJson(g.fromJson(actual, String.class), String[].class);
		
		Map<String, String> utilities = objGson.fromJson(utilitiesMarketdataArray[0], listTypeStringString);
		Map<String, String[]> marketdataNg = objGson.fromJson(utilitiesMarketdataArray[1], listTypeStringArray);
		Map<String, String[]> marketdataMeoh = objGson.fromJson(utilitiesMarketdataArray[2], listTypeStringArray);
		
		assertTrue(utilities.containsKey("V_Price_Storage_Methanol_001"));
		assertNotNull(utilities.get("V_Price_Storage_Methanol_001"));
				
		assertEquals(marketdataNg.get("arrayHeader")[0], "NG");
		assertEquals(marketdataMeoh.get("arrayHeader")[0], "MeOH");

	}
	public void testretrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase() {
		String value = "V_Price_Storage_NaturalGas_001,V_Price_CoolingWater_001,V_Price_Storage_Methanol_001,V_Price_Electricity_001,V_Price_Transport_SG-SC_Methanol_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_Transport_USGC-NEA_NaturalGas_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_FuelGas_001";
		Gson g = new Gson();
		try {
			String result = g.toJson(DataDownload
					.retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase(
							value.split(",")));
			
			
			Gson objGson = new GsonBuilder().setPrettyPrinting().create();
			Type listTypeStringString = new TypeToken<Map<String, String>>(){}.getType();
			Type listTypeStringArray = new TypeToken<Map<String, String[]>>(){}.getType();
					
			String[] utilitiesMarketdataArray = g.fromJson(g.fromJson(result, String.class), String[].class);
			
			Map<String, String> utilities = objGson.fromJson(utilitiesMarketdataArray[0], listTypeStringString);
			Map<String, String[]> marketdataNg = objGson.fromJson(utilitiesMarketdataArray[1], listTypeStringArray);
			Map<String, String[]> marketdataMeoh = objGson.fromJson(utilitiesMarketdataArray[2], listTypeStringArray);
			assertTrue(utilities.containsKey("V_Price_Storage_Methanol_001"));
			assertNotNull(utilities.get("V_Price_Storage_Methanol_001"));
					
			assertEquals(marketdataNg.get("arrayHeader")[0], "NG");
			assertEquals(marketdataMeoh.get("arrayHeader")[0], "MeOH");

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	public void testretrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase() {
		String value = "V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Costs_Storage_CrudePalmOil_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";
		Gson g = new Gson();
		try {
			String actual = g.toJson(DataDownload
					.retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase(
							value.split(",")));
			
			
			Gson objGson = new GsonBuilder().setPrettyPrinting().create();
			Type listTypeStringString = new TypeToken<Map<String, String>>(){}.getType();
			Type listTypeStringArray = new TypeToken<Map<String, String[]>>(){}.getType();
					
			String[] utilitiesMarketdataArray = g.fromJson(g.fromJson(actual, String.class), String[].class);
			
			Map<String, String> utilities = objGson.fromJson(utilitiesMarketdataArray[0], listTypeStringString);
			Map<String, String[]> marketdataCpo = objGson.fromJson(utilitiesMarketdataArray[1], listTypeStringArray);
			Map<String, String[]> marketdataFame = objGson.fromJson(utilitiesMarketdataArray[2], listTypeStringArray);
			
			assertTrue(utilities.containsKey("V_Price_Storage_Biodiesel_001"));
			assertNotNull(utilities.get("V_Price_Storage_Biodiesel_001"));
					
			assertEquals(marketdataCpo.get("arrayHeader")[0], "CPO");
			assertEquals(marketdataFame.get("arrayHeader")[0], "FAME");

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	public void testdownloadingAndSavingExchangeRatesInTheKnowledgeBaselocal() throws Exception {
		Gson g = new Gson();
		String result = g.toJson(DataDownload
				.downloadingAndSavingExchangeRatesInTheKnowledgeBase());
		System.out.println("result= "+result);
	}
}
