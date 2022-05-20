package uk.ac.cam.cares.jps.agent.flood;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.HttpEntity;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class APIConnector {
    private URIBuilder builder;
    private static final Logger LOGGER = LogManager.getLogger(APIConnector.class);	
	
	public APIConnector(String URL) {
		try {
			this.builder = new URIBuilder(URL);
		} catch (URISyntaxException e) {
			LOGGER.error(e.getMessage());
		}
	}
	
	public void setParameter(String param, String value) {
		builder.setParameter(param, value);
	}
	
	public HttpEntity getData() throws ClientProtocolException, IOException, URISyntaxException {
		HttpGet request = new HttpGet(builder.build());
		LOGGER.info("Downloading from " + request);
		CloseableHttpClient httpclient = HttpClients.createDefault();
        CloseableHttpResponse response = httpclient.execute(request);
        LOGGER.info("Download complete");
        return response.getEntity();
	}
}
