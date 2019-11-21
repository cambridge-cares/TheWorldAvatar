package uk.ac.cam.cares.jps.base.query;

import java.io.UnsupportedEncodingException;
import java.net.URI;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class SparqlOverHttpService {
	
	public enum RDFStoreType {	
		FUSEKI,
		RDF4J,
		BLAZEGRAPH,
		OBDA
	}
	
	private RDFStoreType type = RDFStoreType.FUSEKI;
	private String sparqlServiceURIForQuery = null;
	private String sparqlServiceURIForUpdate = null;
	
	public SparqlOverHttpService(String datasetUrl) {
		
		if (datasetUrl.contains("rdf4j")) {
			init(RDFStoreType.RDF4J, datasetUrl);
		} else {
			init(RDFStoreType.FUSEKI, datasetUrl);
		}
	}
	
	public SparqlOverHttpService(RDFStoreType rdfStoreType, String datasetUrl) {
		init(rdfStoreType, datasetUrl);
	}
	
	public SparqlOverHttpService(RDFStoreType rdfStoreType, String sparqlServiceURIForQuery, String sparqlServiceURIForUpdate) {
		this.type = rdfStoreType;
		this.sparqlServiceURIForQuery = sparqlServiceURIForQuery;
		this.sparqlServiceURIForUpdate = sparqlServiceURIForUpdate;
	}
	
	private void init(RDFStoreType rdfStoreType, String datasetUrl) {
		if (RDFStoreType.RDF4J == rdfStoreType) {
			this.type = RDFStoreType.RDF4J;
			this.sparqlServiceURIForQuery = datasetUrl;
			this.sparqlServiceURIForUpdate = datasetUrl + "/statements";
		} else if (RDFStoreType.FUSEKI == rdfStoreType){
			this.type = RDFStoreType.FUSEKI;
			this.sparqlServiceURIForQuery = datasetUrl + "/query";
			this.sparqlServiceURIForUpdate = datasetUrl + "/update";
		} else if (RDFStoreType.BLAZEGRAPH == rdfStoreType){
			this.type = RDFStoreType.BLAZEGRAPH;
			this.sparqlServiceURIForQuery = datasetUrl;
			this.sparqlServiceURIForUpdate = datasetUrl + "/update"; // update will not work yet
		} else {
			throw new JPSRuntimeException("unsupported RDF store type = " + rdfStoreType);
		}
	}
	
	public String toString() {
		StringBuffer b = new StringBuffer("SparqlOverHttpService[type=").append(type);
		b.append(", query url=").append(sparqlServiceURIForQuery);
		b.append(", update url=").append(sparqlServiceURIForUpdate);
		return b.toString();
	}
	
	public String executePost(String messageBody) {

		URI uri = AgentCaller.createURI(sparqlServiceURIForUpdate);
		HttpPost request = new HttpPost(uri);
		//request.setHeader(HttpHeaders.ACCEPT, AgentCaller.MediaType.TEXT_CSV.type);
		
		if (RDFStoreType.RDF4J.equals(type)) {
			request.setHeader(HttpHeaders.CONTENT_TYPE, "application/x-www-form-urlencoded");
			messageBody = "update=" + messageBody;
			//request.setHeader(HttpHeaders.CONTENT_TYPE, "application/sparql-update");
		} else {
			request.setHeader(HttpHeaders.CONTENT_TYPE, "application/sparql-update");
		}
		
		
		HttpEntity entity;
		try {
			entity = new StringEntity(messageBody);
			request.setEntity(entity);
			
			System.out.println(request);
			
		} catch (UnsupportedEncodingException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		HttpResponse httpResponse;
		try {
			httpResponse = HttpClientBuilder.create().build().execute(request);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		String responseBody = null;	
		if (httpResponse.getEntity() != null) {
			try {
				responseBody = EntityUtils.toString(httpResponse.getEntity());
			} catch (Exception e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		} 

		int statusCode = httpResponse.getStatusLine().getStatusCode();
		// https://httpstatuses.com/204 : The server has successfully fulfilled the request and that there is no additional content to send in the response payload body.
		// this is the case for POST for SPARQL endpoints
		if ((statusCode != 200) && (statusCode != 204)) {
			
			Header[] headers = httpResponse.getAllHeaders();
			for (Header current : headers) {
				System.out.println(current.getName() + "=" + current.getValue());
			}
			
			throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine() + "\n" + responseBody);
		}

		return responseBody;
	}
	
	public String executeGet(String sparqlQuery) {

		URI uri = null;
		if (RDFStoreType.RDF4J.equals(type)) {
			uri = AgentCaller.createURI(sparqlServiceURIForQuery, "query", sparqlQuery, "Accept", MediaType.TEXT_CSV.type);
		} else {
			uri = AgentCaller.createURI(sparqlServiceURIForQuery, "query", sparqlQuery);
		}
		HttpGet request = new HttpGet(uri);
		request.setHeader(HttpHeaders.ACCEPT, MediaType.TEXT_CSV.type);
		//request.setHeader(HttpHeaders.ACCEPT, "text/plain");
		//request.setHeader(HttpHeaders.ACCEPT, "application/sparql-results+json");

		HttpResponse httpResponse;
		try {
			httpResponse = HttpClientBuilder.create().build().execute(request);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		String responseBody = null;	
		if (httpResponse.getEntity() != null) {
			try {
				responseBody = EntityUtils.toString(httpResponse.getEntity());
			} catch (Exception e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		} 
		
		if (httpResponse.getStatusLine().getStatusCode() != 200) {
			
			Header[] headers = httpResponse.getAllHeaders();
			for (Header current : headers) {
				System.out.println(current.getName() + "=" + current.getValue());
			}
			
			throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine() + "\n" + responseBody);
		}

		return responseBody;
	}
}