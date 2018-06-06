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

public class TestMoDSAnalysis extends TestCase {

	public static String executeGet(String path, String key,
			String value) throws ParseException,
			IOException, URISyntaxException {
		/**
		 * this function was provided by Andreas and so I
		 * don't really know how it works
		 */

		// TODO-AE change localhost, maybe use directly
		// class java.net.URI, maybe move this class to
		// JPS_BASE
		URIBuilder builder = new URIBuilder()
				.setScheme("http").setHost("localhost:8080")
				.setPath(path).setParameter(key, value);
		return executeGet(builder);
	}

	private static String executeGet(URIBuilder builder)
			throws ParseException, IOException,
			URISyntaxException {

		HttpGet request = new HttpGet(builder.build());
		HttpResponse httpResponse = HttpClientBuilder
				.create().build().execute(request);
		return EntityUtils
				.toString(httpResponse.getEntity());
	}

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

		String actual = executeGet(path, key, value);
		System.out.println(actual);
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

		String actual = executeGet(path, key, value);
		System.out.println(actual);
		assertTrue(actual.contains(
				"The highest marginal profit per tonne"));
	}

}
