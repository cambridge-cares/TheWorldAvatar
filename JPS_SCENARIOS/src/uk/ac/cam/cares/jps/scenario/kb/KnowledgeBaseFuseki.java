package uk.ac.cam.cares.jps.scenario.kb;

import java.io.InputStream;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * Helpful links concerning the Web API of Fuseki: <br>
 * - https://jena.apache.org/documentation/fuseki2/ <br>
 * - https://jena.apache.org/documentation/fuseki2/soh.html
 * - https://jena.apache.org/documentation/fuseki2/fuseki-data-services.html <br>
 * - https://jena.apache.org/documentation/fuseki2/fuseki-server-protocol.html <br>
 * - http://blog.mynarz.net/2015/05/curling-sparql-http-graph-store-protocol.html<br>
 * 
 * @author Andreas
 *
 */
public class KnowledgeBaseFuseki extends KnowledgeBaseAbstract {

	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseFuseki.class);
	

	
	public KnowledgeBaseFuseki(String datasetUrl, String datasetName, String endpointUrl) {
		super(datasetUrl, datasetName, endpointUrl);
	}
	
	private String getEndpointUrl() {
		return endpointUrl;
	}
	
	private String getGraphStoreUrl() {
		return getEndpointUrl() + "/data";
	}
	
	@Override
	public void put(String resourceUrl, String content, String contentType) {
		logger.info("put resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getGraphStoreUrl();
		if (contentType == null) {
			contentType = MediaType.APPLICATION_RDF_XML.type; 
		}
		if (resourceUrl == null) {
			HttpPut put = Http.put(url, content, contentType, null);
			Http.execute(put);	
		} else {
			Http.execute(Http.put(url, content, contentType, null, "graph", resourceUrl));			
		}
	}

	@Override
	public void update(String resourceUrl, String sparql) {
		logger.info("update resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getEndpointUrl()  + "/update";
		if (resourceUrl == null) {
			Http.execute(Http.post(url, sparql, MediaType.APPLICATION_SPARQL_UPDATE.type, null));
		} else {
			throw new UnsupportedOperationException();
		}
	}

	@Override
	public String get(String resourceUrl, String accept) {
		logger.info("get resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getGraphStoreUrl();
		if (resourceUrl == null) {	
			HttpGet get = Http.get(url, accept);			
			return Http.execute(get);
				
		} else {
			return Http.execute(Http.get(url, accept, "graph", resourceUrl));		
		}
	}

	@Override
	public String query(String resourceUrl, String sparql) {
		logger.info("query resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		String url = getEndpointUrl() + "/query";
		if (resourceUrl == null) {
			return Http.execute(Http.get(url, MediaType.APPLICATION_SPARQL.type, "query", sparql));
		} else {
			RDFFormat format = getRDFFormatFromFileType(resourceUrl);
			if (format == null) {
				format = RDFFormat.RDFXML;
			}
			String content = get(resourceUrl, format.getDefaultMIMEType());
			InputStream inputStream = FileUtil.stringToInputStream(content);
			return query(inputStream, format, sparql);
		}
	}

	@Override
	public boolean exists(String resourceUrl) {
		logger.info("exists resourceUrl=" + resourceUrl + " (kb url=" + datasetUrl + ")");
		throw new UnsupportedOperationException();
	}
}
