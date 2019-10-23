package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.eclipse.rdf4j.query.GraphQuery;
import org.eclipse.rdf4j.query.Update;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFParseException;
import org.eclipse.rdf4j.rio.RDFWriter;
import org.eclipse.rdf4j.rio.Rio;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class KnowledgeBaseFileBased extends KnowledgeBaseAbstract {

	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseFileBased.class);
	
	@Override
	public void put(String resourceUrl, String content) {
		logger.info("put resourceUrl=" + resourceUrl);
		String filePath = BucketHelper.getLocalPath(resourceUrl);
		FileUtil.writeFileLocally(filePath, content);
	}

	@Override
	public void update(String resourceUrl, String sparql) {
		logger.info("update resourceUrl=" + resourceUrl);
		String filePath = BucketHelper.getLocalPath(resourceUrl);
		File file = new File(filePath);
		
		MemoryStore memStore = new MemoryStore();
		Repository repo = new SailRepository(memStore);
		repo.init();

		try (RepositoryConnection conn = repo.getConnection()) {
			
			// read the file
			conn.add(file, null, RDFFormat.RDFXML);
			
			// update the file in memory
			Update update = conn.prepareUpdate(sparql);
			update.execute();
			
			// write the update back to file
			String queryString = "CONSTRUCT WHERE { ?s ?p ?o } ";
			FileOutputStream outputstream = new FileOutputStream(file);
			RDFWriter writer = Rio.createWriter(RDFFormat.RDFXML, outputstream);
			GraphQuery graphQuery = conn.prepareGraphQuery(queryString);
			graphQuery.evaluate(writer);
		} catch (RDFParseException | RepositoryException | IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	@Override
	public boolean exists(String resourceUrl) {
		String filePath = BucketHelper.getLocalPath(resourceUrl);
		return new File(filePath).exists();
	}
	
	@Override
	public String get(String resourceUrl, String accept) {
		logger.info("get resourceUrl=" + resourceUrl);
		String result = null;
		String filePath = BucketHelper.getLocalPath(resourceUrl);
		
		RDFFormat format = null;
		if (accept != null) {
			format = getRDFFormat(accept);
		}
		
		if (format == null) {
			result = FileUtil.readFileLocally(filePath);
		} else {
			for (String current : format.getFileExtensions()) {
				if (filePath.endsWith(current)) {
					// no conversion is required
					result = FileUtil.readFileLocally(filePath);
					break;
				}
			}
			
			
			try {
				if (result == null) {
						File file = new File(filePath);
						URL fileUrl = file.toURI().toURL();
						result = convert(fileUrl, format);
				}
			} catch(IOException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}	
		
		return result;
	}

	@Override
	public String query(String resourceUrl, String sparql) {
		logger.info("query resourceUrl=" + resourceUrl);
		String filePath = BucketHelper.getLocalPath(resourceUrl);
		File file = new File(filePath);
		InputStream inputStream = null;
		try {
			inputStream = new FileInputStream(file);
		} catch (FileNotFoundException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return query(inputStream, sparql);
	}
}
