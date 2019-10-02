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

import uk.ac.cam.cares.jps.arbitrage.Arbitrage;

public class TestMoDSAnalysis extends TestCase {
	private static Logger logger = LoggerFactory
			.getLogger(TestMoDSAnalysis.class);
	

	/**
	 * this function calls ArbitrageAgent to call
	 * Arbitrage.Running_analysis_MoDS2 and check that the
	 * returned string contains a predefined substring
	 * 
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public void testRunningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent()
			throws URISyntaxException, Exception,
			ClientProtocolException, IOException {

		String input = "[24220.0656]";
		String result = Arbitrage.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent(input);
		logger.info(result);
		
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listType = new TypeToken<Map<String, String>>(){}.getType();
		
		Map<String, String> resultMap = objGson.fromJson(result, listType);
		
		assertTrue(resultMap.containsKey("marginal profit per tonne of biodiesel FAME (in USD)"));
		assertNotNull(resultMap.get("marginal profit per tonne of biodiesel FAME (in USD)"));
	}
	
	public void testRunningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2()
			throws URISyntaxException, Exception,
			ClientProtocolException, IOException {

		String input = "[0.5527777778]";
		String result = Arbitrage.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2(input);
		logger.info(result);

		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listType = new TypeToken<Map<String, String>>(){}.getType();
		
		Map<String, String> resultMap = objGson.fromJson(result, listType);
		
		assertTrue(resultMap.containsKey("marginal profit per tonne of methanol (in USD)"));
		assertNotNull(resultMap.get("marginal profit per tonne of methanol (in USD)"));
	}

}
