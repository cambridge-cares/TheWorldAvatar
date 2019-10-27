package uk.ac.cam.cares.jps.scenario.kb;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
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
import org.eclipse.rdf4j.sail.memory.MemoryStore;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class KnowledgeBaseRdf4jLocalInMemory extends KnowledgeBaseAbstract {

	private static KnowledgeBaseRdf4jLocalInMemory instance = null;
	
	private MemoryStore memStore = null;
	private Repository repo = null;
	
	public static synchronized KnowledgeBaseRdf4jLocalInMemory getInstance() {
		if (instance == null) {
			instance = new KnowledgeBaseRdf4jLocalInMemory();
		}
		return instance;
	}
	
	private KnowledgeBaseRdf4jLocalInMemory() {
		
		//File dir = new File(datasetDir);
		//memStore = new MemoryStore(dir);
		memStore = new MemoryStore();
		repo = new SailRepository(memStore);
		repo.init();
	}
	
	private RepositoryConnection getConnection() {
		return repo.getConnection();
	}
	
	@Override
	public void put(String resourceUrl, String content, String contentType) {
		InputStream stream = FileUtil.stringToInputStream(content);
		
		RepositoryConnection conn = getConnection();
		try {
			conn.begin();
			conn.add(stream, "", RDFFormat.RDFXML);
			conn.commit();
		} catch (RDFParseException | RepositoryException | IOException e) {
			conn.rollback();
			throw new JPSRuntimeException(e.getMessage(), e);
		} finally {
			conn.close();
			//repo.shutDown();
			//memStore.shutDown();
		}
	}

	@Override
	public void update(String resourceUrl, String sparql) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String get(String resourceUrl, String accept) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String query(String resourceUrl, String sparql) {
		
		TupleQuery tupleQuery = getConnection().prepareTupleQuery(QueryLanguage.SPARQL, sparql);	
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		SPARQLResultsJSONWriter writer = new SPARQLResultsJSONWriter(outputStream);
		tupleQuery.evaluate(writer);
		
		try {
			return new String(outputStream.toByteArray(), Charset.defaultCharset());
		} finally {
			try {
				outputStream.close();
			} catch (IOException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}	
		}
	}

	@Override
	public boolean exists(String resourceUrl) {
		// TODO Auto-generated method stub
		return false;
	}
	

	@Override
	public String getDatasetUrl() {
		// TODO Auto-generated method stub
		return null;
	}

}
