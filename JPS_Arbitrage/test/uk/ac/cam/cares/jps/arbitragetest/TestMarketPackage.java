package uk.ac.cam.cares.jps.arbitragetest;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import junit.framework.TestCase;


public class TestMarketPackage extends TestCase{

	
	public static String executeGet(String path, String key, String value)
			throws ParseException, IOException, URISyntaxException {
		// TODO-AE change localhost, maybe use directly class java.net.URI, maybe move this class to JPS_BASE
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost:8080")
				.setPath(path)
				.setParameter(key, value);
		return executeGet(builder);
	}	
		
	private static String executeGet(URIBuilder builder)
			throws ParseException, IOException, URISyntaxException {
		
		HttpGet request = new HttpGet(builder.build());
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		return EntityUtils.toString(httpResponse.getEntity());
	}

	public void testdatadownload() throws URISyntaxException, ClientProtocolException, IOException {
		String path = "/JPS_Arbitrage/download";
		String actual = executeGet(path,"whatver", "whatever2");
		System.out.println(actual);
		assertTrue(actual.contains("JAN 2021,FEB 2021,MAR 2021,APR 2021"));

	}
	
	public void testreadingdata() throws URISyntaxException, ClientProtocolException, IOException {
		String path = "/JPS_Arbitrage/read";
		String key = "individuals";
		String value = 	"V_Price_Storage_NaturalGas_001,V_Price_CoolingWater_001,V_Price_Storage_Biodiesel_001,V_Price_Storage_CrudePalmOil_001,V_Costs_Storage_CrudePalmOil_001,V_Price_Storage_Methanol_001,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,V_Price_Electricity_001,V_Price_Transport_SG-SC_Methanol_001,V_USD_to_SGD,V_Price_ProcessWater_001,V_Price_Transport_USGC-NEA_NaturalGas_001,V_Price_HighPressureSteam_001,V_USD_to_CNY,V_Price_MediumPressureSteam_001,V_Price_LowPressureSteam_001,V_Price_Transport_SEA-SC_Biodiesel_001,V_Price_FuelGas_001";
		String actual = executeGet(path,key,value);
		System.out.println(actual);
		assertTrue(actual.contains("JAN 2021,FEB 2021,MAR 2021,APR 2021"));

	}
}
