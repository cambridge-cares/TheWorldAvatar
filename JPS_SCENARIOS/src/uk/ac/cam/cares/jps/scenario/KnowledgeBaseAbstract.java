package uk.ac.cam.cares.jps.scenario;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.Charset;

import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.resultio.sparqljson.SPARQLResultsJSONWriter;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFParseException;
import org.eclipse.rdf4j.rio.RDFParser;
import org.eclipse.rdf4j.rio.RDFWriter;
import org.eclipse.rdf4j.rio.Rio;
import org.eclipse.rdf4j.sail.memory.MemoryStore;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public abstract class KnowledgeBaseAbstract {

	public static final RDFFormat[] SUPPORTED_RDF_FORMATS = new RDFFormat[] {
			RDFFormat.RDFXML, RDFFormat.TURTLE, RDFFormat.JSONLD, RDFFormat.NQUADS};
	
	public abstract void put(String resourceUrl, String content);
	
	public abstract void update(String resourceUrl, String sparql);
	
	public abstract String get(String resourceUrl, String accept);
	
	public abstract String query(String resourceUrl, String sparql);
	
	public abstract boolean exists(String resourceUrl);
	
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
	
	public RDFFormat getRDFFormat(String contentType) {
		for (RDFFormat current : KnowledgeBaseAbstract.SUPPORTED_RDF_FORMATS) {
			if (current.getDefaultMIMEType().equals(contentType)) {
				return current;
			}
		}
		
		return null;
	}
	
	public static String query(InputStream inputStream, String sparql) {
		MemoryStore memStore = new MemoryStore();
		Repository repo = new SailRepository(memStore);
		repo.init();
		
		try (RepositoryConnection conn = repo.getConnection()) {
			conn.add(inputStream, "", RDFFormat.RDFXML);
			TupleQuery tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, sparql);	

			ByteArrayOutputStream outputStream = null;
			try {
				outputStream = new ByteArrayOutputStream();
				SPARQLResultsJSONWriter writer = new SPARQLResultsJSONWriter(outputStream);
				tupleQuery.evaluate(writer);
				return new String(outputStream.toByteArray(), Charset.defaultCharset());
			} finally {
				outputStream.close();
			}
		} catch(RDFParseException | RepositoryException | IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
}
