package uk.ac.cares.jps.composition.utils;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

public class Request {

	public static String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build()
					.execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				return "";
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			return "";
		}
	}

}
