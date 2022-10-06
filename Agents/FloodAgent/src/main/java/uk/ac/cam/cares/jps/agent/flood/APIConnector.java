package uk.ac.cam.cares.jps.agent.flood;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class APIConnector {
    private URIBuilder builder;
    private static final Logger LOGGER = LogManager.getLogger(APIConnector.class);	
	
	public APIConnector(String url) throws URISyntaxException {
		this.builder = new URIBuilder(url);
	}
	
	public void setParameter(String param, String value) {
		builder.setParameter(param, value);
	}
	
	public HttpEntity getData(CloseableHttpClient httpclient) throws IOException, URISyntaxException {
		CloseableHttpResponse response;
		HttpGet request = new HttpGet(builder.build());
		LOGGER.debug("Downloading from {}", request);
		response = httpclient.execute(request);
		LOGGER.debug("Download complete");
		return response.getEntity();	
	}
}
