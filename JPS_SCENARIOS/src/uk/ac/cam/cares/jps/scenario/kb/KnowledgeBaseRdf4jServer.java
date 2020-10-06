package uk.ac.cam.cares.jps.scenario.kb;

import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class KnowledgeBaseRdf4jServer extends KnowledgeBaseAbstract {

	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseRdf4jServer.class);
	
	public KnowledgeBaseRdf4jServer(String datasetUrl, String datasetName, String endpointUrl) {
		super(datasetUrl, datasetName, endpointUrl);
	}
	
	private String getGraphStoreUrl() {
		return getEndpointUrl() + "/rdf-graphs/service";
	}
	
	private String getEndpointUrl() {
		return endpointUrl;
	}
	
	@Override
	public void put(String resourceUrl, String content, String contentType) {
		logger.info("put resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getGraphStoreUrl();
		if (contentType == null) {
			contentType = MediaType.APPLICATION_RDF_XML.type; //"application/rdf+xml;charset=UTF-8";
		}
		if (resourceUrl == null) {
			
			// does not work without changing JPS_BASE_LIB
			//Http.execute(Http.put(url, content, contentType, null, "graph", "default"));
			
			HttpPut put = Http.put(url, content, contentType, null);
			try {
				URI extended = new URI(put.getURI() + "?default");
				put.setURI(extended);
			} catch (URISyntaxException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
			Http.execute(put);
			
		} else {
			Http.execute(Http.put(url, content, contentType, null, "graph", resourceUrl));			
		}
	}

	@Override
	public void update(String resourceUrl, String sparql) {
		logger.info("update resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");

		try {
			String sparqlUpdateInBody = "update=" + URLEncoder.encode(sparql, StandardCharsets.UTF_8.displayName());
			String url = getEndpointUrl() + "/statements";
			if (resourceUrl == null) {
				Http.execute(Http.post(url, sparqlUpdateInBody, MediaType.APPLICATION_X_WWW_FORM_URLENCODED.type, null));
			} else {
				Http.execute(Http.post(url, sparqlUpdateInBody, MediaType.APPLICATION_X_WWW_FORM_URLENCODED.type, null,
						"insert-graph-uri", resourceUrl));
			}
		} catch (UnsupportedEncodingException e) {
			throw new JPSRuntimeException(e);
		}
	}

	@Override
	public String get(String resourceUrl, String accept) {
		logger.info("get resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getGraphStoreUrl();
		if (resourceUrl == null) {
			
			// does not work without changing JPS_BASE_LIB
			//return Http.execute(Http.get(url, accept, "default"));	
			
			HttpGet get = Http.get(url, accept);			
			try {
				URI extended = new URI(get.getURI() + "?default");
				get.setURI(extended);
			} catch (URISyntaxException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}

			return Http.execute(get);
				
		} else {
			return Http.execute(Http.get(url, accept, "graph", resourceUrl));		
		}
	}

	@Override
	public String query(String resourceUrl, String sparql) {
		logger.info("query resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getEndpointUrl();
		String result = null;
		if (resourceUrl == null) {
			// no special treatment of default graph necessary
			
			result = Http.execute(Http.get(url, MediaType.APPLICATION_SPARQL.type, "query", sparql));
		} else {
			RDFFormat format = getRDFFormatFromFileType(resourceUrl);
			if (format == null) {
				format = RDFFormat.RDFXML;
			}
			String content = get(resourceUrl, format.getDefaultMIMEType());
			InputStream inputStream = FileUtil.stringToInputStream(content);
			result = query(inputStream, format, sparql);
		}
		return result;
	}

	@Override
	public boolean exists(String resourceUrl) {
		logger.info("exists resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		throw new UnsupportedOperationException();
	}
}
