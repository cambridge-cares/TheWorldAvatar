package uk.ac.cam.cares.jps.arbitragetest;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.ClientProtocolException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestMoDSAnalysis extends TestCase {
	private static Logger logger = LoggerFactory
			.getLogger(TestMoDSAnalysis.class);
	
	/**
	 * this function calls ArbitrageAgent to call
	 * Arbitrage.Running_analysis_MoDS and check that the
	 * returned string contains a predefined substring
	 * 
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public void testRunningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles()
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles";
		String key = "MoDS_input";
		String value = "24220.0656";

		String actual = AgentCaller.executeGet(path, key, value);
		logger.info(actual);
		assertTrue(actual.contains(
				"The highest marginal profit per tonne"));
	}

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
			throws URISyntaxException,
			ClientProtocolException, IOException {

		String path = "/JPS_Arbitrage/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent";
		String key = "MoDS_input";
		String value = "24220.0656";

		String actual = AgentCaller.executeGet(path, key, value);
		logger.info(actual);
		assertTrue(actual.contains(
				"The highest marginal profit per tonne"));
	}

}
