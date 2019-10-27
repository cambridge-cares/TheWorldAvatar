package uk.ac.cam.cares.jps.scenario.kb;

import java.io.InputStream;

import org.apache.http.client.methods.HttpGet;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class KnowledgeBaseBlazegraph extends KnowledgeBaseAbstract {

	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseBlazegraph.class);
	
	private String datasetName = null;
	
	public KnowledgeBaseBlazegraph(String datasetUrl, String datasetName) {
		this.datasetUrl = datasetUrl;
		this.datasetName = datasetName;
	}
	
	private String getEndpointUrl() {		
		return "http://localhost:9999//blazegraph/namespace/" + datasetName + "/sparql";
	}
	
	@Override
	public void put(String resourceUrl, String content, String contentType) {
		logger.info("put resourceUrl=" + resourceUrl);
		String url = getEndpointUrl();
		if (contentType == null) {
			contentType = MediaType.APPLICATION_RDF_XML.type; 
		}
		
		if (resourceUrl == null) {			
			throw new UnsupportedOperationException();
		} else {
			// Blazegraph server allow HTTP PUT in combination with an SPARQL CONSTRUCT query for deletion
			// see https://wiki.blazegraph.com/wiki/index.php/REST_API#UPDATE_.28DELETE_statements_selected_by_a_QUERY_plus_INSERT_statements_from_Request_Body_using_PUT.29
			// here, we use to consecutive requests DELETE and POST
			Http.execute(Http.delete(url, "c", "<" + resourceUrl + ">"));
			Http.execute(Http.post(url, content, contentType, null, "context-uri", resourceUrl));			
		}
	}

	@Override
	public void update(String resourceUrl, String sparql) {
		logger.info("update resourceUrl=" + resourceUrl);
		
		String url = getEndpointUrl();
		if (resourceUrl == null) {
			Http.execute(Http.post(url, null, null, null, "update", sparql));
		} else {
			// neither context-iri nor using-graph-uri nor using-named-graph-uri works here
			//Http.execute(Http.post(url, null, null, null, "update", sparql,
			//		"using-named-graph-uri", resourceUrl));
			// must be solved within the sparql statement: INSERT DATA { GRAPH <resourceurl> { ... } }
			//Http.execute(Http.post(url, null, null, null, "update", sparql));
			throw new UnsupportedOperationException();
		}
	}

	@Override
	public String get(String resourceUrl, String accept) {
		logger.info("get resourceUrl=" + resourceUrl);
		String url = getEndpointUrl();
		if (resourceUrl == null) {
			
			// does not work without changing JPS_BASE_LIB
			//return Http.execute(Http.get(url, accept, "default"));	
			
			HttpGet get = Http.get(url, accept);			
//			try {
//				URI extended = new URI(get.getURI() + "?default");
//				get.setURI(extended);
//			} catch (URISyntaxException e) {
//				throw new JPSRuntimeException(e.getMessage(), e);
//			}

			return Http.execute(get);

		} else {
			//HttpGet get = Http.get(url, accept); //, "c", resourceUrl);
			//HttpGet get = Http.get(url, accept, "default-graph-uri", resourceUrl);
			HttpGet get = Http.get(url, accept, "query", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }",
					"default-graph-uri", resourceUrl);
			
			
			
//			try {
//				URI extended = new URI(get.getURI() + "?GETSTMTS");
//				get.setURI(extended);
//			} catch (URISyntaxException e) {
//				throw new JPSRuntimeException(e.getMessage(), e);
//			}				

			return Http.execute(get);		
		}
	}

	@Override
	public String query(String resourceUrl, String sparql) {
		logger.info("query resourceUrl=" + resourceUrl);
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
		logger.info("exists resourceUrl=" + resourceUrl);
		throw new UnsupportedOperationException();
	}
}
