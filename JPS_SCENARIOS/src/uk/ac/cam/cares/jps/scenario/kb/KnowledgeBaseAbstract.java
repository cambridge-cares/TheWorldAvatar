package uk.ac.cam.cares.jps.scenario.kb;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.Charset;

import org.apache.jena.query.ResultSet;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFParser;
import org.eclipse.rdf4j.rio.RDFWriter;
import org.eclipse.rdf4j.rio.Rio;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

public abstract class KnowledgeBaseAbstract {

	public static final RDFFormat[] SUPPORTED_RDF_FORMATS = new RDFFormat[] {
			RDFFormat.RDFXML, RDFFormat.TURTLE, RDFFormat.JSONLD, RDFFormat.NQUADS};
	
	protected String datasetUrl = null;
	protected String datasetName = null;
	protected String endpointUrl = null;
	
	public abstract void put(String resourceUrl, String content, String contentType);
	
	public abstract void update(String resourceUrl, String sparql);
	
	public abstract String get(String resourceUrl, String accept);
	
	public abstract String query(String resourceUrl, String sparql);
	
	public abstract boolean exists(String resourceUrl);
	
	public String getDatasetUrl() {
		return datasetUrl;
	}
	
	public KnowledgeBaseAbstract(String datasetUrl, String datasetName, String endpointUrl) {
		this.datasetUrl = datasetUrl;
		this.datasetName = datasetName;
		this.endpointUrl = endpointUrl;
	}
	
	public String convert(URL resourceUrl, RDFFormat outputFormat) throws IOException {
		
		String result = null;
		
		InputStream inputStream = resourceUrl.openStream();
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		
		RDFParser rdfParser = Rio.createParser(RDFFormat.RDFXML);
		RDFWriter rdfWriter = Rio.createWriter(outputFormat, outputStream);
		// link the parser to the writer for direct conversion
		rdfParser.setRDFHandler(rdfWriter);

		try {
		   rdfParser.parse(inputStream, resourceUrl.toString());
		   result = new String(outputStream.toByteArray(), Charset.defaultCharset());
		} finally {
		  inputStream.close();
		  outputStream.close();
		}
		
		return result;
	}
	
	/**
	 * getDefaultFileExtension(), getFileExtensions(), current.getDefaultMIMEType(), current.getMIMETypes(), supportsContexts());
	 * 
	 * rdf, [rdf, rdfs, owl, xml], application/rdf+xml, [application/rdf+xml, application/xml, text/xml], false
	 * ttl, [ttl], text/turtle, [text/turtle, application/x-turtle], false
	 * jsonld, [jsonld], application/ld+json, [application/ld+json], true
	 * nq, [nq], application/n-quads, [application/n-quads, text/x-nquads, text/nquads], true
	 * 
	 * @param mediaType
	 * @return
	 */
	public static RDFFormat getRDFFormatFromMediaType(String mediaType) {
		for (RDFFormat current : KnowledgeBaseAbstract.SUPPORTED_RDF_FORMATS) {
			if (current.getDefaultMIMEType().equals(mediaType)) {
				return current;
			}
		}
		
		return null;
	}
	
	public static RDFFormat getRDFFormatFromFileType(String fileName) {
		for (RDFFormat current : KnowledgeBaseAbstract.SUPPORTED_RDF_FORMATS) {
			for (String currentExt : current.getFileExtensions()) {
				if (fileName.endsWith("." + currentExt)) {
					return current;
				}
			}
		}

		return RDFFormat.RDFXML;
	}
	
	public static String query(InputStream inputStream, RDFFormat inputFormat, String sparql) {
		
		// this is the solution with Jena that was used originally in QueryBroker.queryFile 
		// and that is still used in KnowledgeBaseClient when querying a file on the client-side.
		ResultSet resultSet = JenaHelper.queryInputStream(inputStream, sparql);
		return JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		
		
		// the following implementation with RDF4j will not always return the queried information 
		// since - contrary to Jena - RDF4j does not load OWL imports automatically
//		MemoryStore memStore = new MemoryStore();
//		Repository repo = new SailRepository(memStore);
//		repo.init();
//		
//		try (RepositoryConnection conn = repo.getConnection()) {
//			conn.add(inputStream, "", inputFormat);
//			TupleQuery tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, sparql);	
//
//			ByteArrayOutputStream outputStream = null;
//			try {
//				outputStream = new ByteArrayOutputStream();
//				SPARQLResultsJSONWriter writer = new SPARQLResultsJSONWriter(outputStream);
//				tupleQuery.evaluate(writer);
//				return new String(outputStream.toByteArray(), Charset.defaultCharset());
//			} finally {
//				outputStream.close();
//			}
//		} catch(RDFParseException | RepositoryException | IOException e) {
//			throw new JPSRuntimeException(e.getMessage(), e);
//		}
	}
}
