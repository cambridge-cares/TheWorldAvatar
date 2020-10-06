package uk.ac.cam.cares.jps.kg;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.Rio;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;

public class RepositoryManager implements IRepositoryManager {
	static Logger logger = Logger.getLogger(RepositoryManager.class);
	private static String ONTOKIN_TBOX_IRI;
	private static String SERVER_URL;
	private static String REPOSITORY_ID;
	private static String ONTOKIN_KB_URL;
	private static String ONTOKIN_KB_ABOX_FILE_PATH;
	public static final String RDF = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n";
	public static final String RDFS = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n";

	private static String ONTOCHEMEXP_TBOX_IRI;
	private static String ONTOCHEMEXP_KB_URL = "http://www.theworldavatar.com/kb/ontokin/";
	private static String ONTOCHEMEXP_ABOX_FILE_PATH;

	public static ApplicationContext applicationContext;
	// public static Onto...

	/**
	 * Loads an abox to the ontology KB repository. It also creates</br>
	 * a context, which is a necessary feature to delete the abox</br>
	 * if user wants.
	 * 
	 * @param serverURL
	 * @param aboxFileName
	 * @param aboxFilePath
	 * @param baseURI
	 * @param repositoryID
	 * @throws OntoException
	 */
	public void loadOntology(String serverURL, String aboxFileName, String aboxFilePath, String baseURI,
			String repositoryID) throws OntoException {
		checkUploadParameterValidity(serverURL, aboxFileName, aboxFilePath, baseURI, repositoryID);
		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
			RepositoryConnection con = repo.getConnection();
			ValueFactory f = repo.getValueFactory();
			org.eclipse.rdf4j.model.IRI context = f.createIRI(ONTOCHEMEXP_KB_URL.concat(aboxFileName));
			try {
				URL url = new URL("file:/".concat(aboxFilePath).concat(aboxFileName));
				con.add(url, url.toString(), RDFFormat.RDFXML, context);
			} finally {
				con.close();
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
		} catch (IOException e) {
			System.out.println("IOException occurred.");
			e.printStackTrace();
		}
	}

	/**
	 * Deletes an abox from the ontology KB repository using its context.
	 * 
	 * @param serverURL
	 * @param aboxFileName
	 * @param contextURL
	 * @param repositoryID
	 */
	public void deleteOntology(String serverURL, String mechanismName, String contextURL, String repositoryID)
			throws OntoException {
		checkDeleteParameterValidity(serverURL, mechanismName, contextURL, repositoryID);
		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
			RepositoryConnection con = repo.getConnection();
			ValueFactory f = repo.getValueFactory();
			try {
				org.eclipse.rdf4j.model.IRI context = f.createIRI(contextURL);
				// Deletes or clears the ontology
				con.clear(context);
			} catch (Exception e) {
			} finally {
				logger.info("Successfully deleted the following mechanism:" + mechanismName);
				con.close();
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
		}
	}

	/**
	 * Downloads an ontology using its context.
	 * 
	 * @param serverURL
	 * @param aboxFileName
	 * @param contextURL
	 * @param repositoryID
	 * @param aboxFilePath
	 */
	public void downloadOntology(String serverURL, String aboxFileName, String contextURL, String repositoryID,
			String aboxFilePath) throws OntoException {
		// Checks the validity of all parameters of this method.
		checkDownloadParameterValidity(serverURL, aboxFileName, contextURL, repositoryID, aboxFilePath);
		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
			RepositoryConnection con = repo.getConnection();
			ValueFactory f = repo.getValueFactory();
			try {
				org.eclipse.rdf4j.model.IRI context = f.createIRI(contextURL);
				// Export all statements in the context to System.out, in RDF/XML format
				RDFHandler writer = Rio.createWriter(RDFFormat.RDFXML,
						new BufferedWriter(new OutputStreamWriter(new FileOutputStream(aboxFilePath), "UTF-8")));
				con.export(writer, context);
			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				con.close();
				logger.info("Executed the command to close the connection to the repository");
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
		}
	}

	/**
	 * Queries a given repository using SPARQL.
	 * 
	 * @param serverURL
	 * @param repositoryID
	 * @param queryString
	 * @return Set<String>
	 */

	public List<String> queryRepositoryExperimentalData(String serverURL, String repositoryID, String queryString)
			throws OntoException, AutoMechCalibAgentException {
		List<String> results = new ArrayList<>();
		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
			RepositoryConnection con = repo.getConnection();
			try {
				System.out.println("Query String:\n" + queryString);
				// Export all statements in the context to System.out, in RDF/XML format
				TupleQuery queryResult = con.prepareTupleQuery(queryString);
				// A QueryResult is also an AutoCloseable resource, so make sure it gets
				// closed when done.
				try (TupleQueryResult result = queryResult.evaluate()) {
					// we just iterate over all solutions in the result...
					while (result.hasNext()) {
						BindingSet solution = result.next();
						// ... and print out the value of the variable bindings
						// for ?s and ?n
						results.add(solution.getValue("x").stringValue());
					}
				} finally {
					// Before our program exits, make sure the database is properly shut down.
					repo.shutDown();
				}
			} catch (Exception e) {
				logger.error("Exception occurred.");
				e.printStackTrace();
				logger.info(e.getMessage());
				throw new OntoException("Exception occurred.");
			} finally {
				con.close();
				logger.info("Executed the command to close the connection to the repository");
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
			throw new OntoException("RDF4JException occurred.");
		}
		return results;
	}

	/**
	 * Queries a given repository using SPARQL.
	 * 
	 * @param serverURL
	 * @param repositoryID
	 * @param queryString
	 * @return Set<String>
	 */
	public String queryRepositoryReturnJson(String serverURL, String repositoryID, String queryString)
			throws OntoException {
//		init();
//		StringBuilder json = new StringBuilder();
//		try {
//			Repository repo = new HTTPRepository(serverURL, repositoryID);
//			repo.initialize();
//			RepositoryConnection con = repo.getConnection();
//			try {
//				System.out.println("Query String:\n" + queryString);
//				// Export all statements in the context to System.out, in RDF/XML format
//				TupleQuery queryResult = con.prepareTupleQuery(queryString);
//				// A QueryResult is also an AutoCloseable resource, so make sure it gets
//				// closed when done.
//				try (TupleQueryResult result = queryResult.evaluate()) {
//					json = getResultInJson(json, result);
//				} finally {
//					// Before our program exits, make sure the database is properly shut down.
//					repo.shutDown();
//				}
//			} catch (Exception e) {
//				logger.error("Exception occurred.");
//				e.printStackTrace();
//				throw new OntoException("Exception occurred.");
//			} finally {
//				logger.info("Executed the command to close the connection to the repository");
//				con.close();
//			}
//		} catch (RDF4JException e) {
//			logger.error("RDF4JException occurred.");
//			e.printStackTrace();
//			throw new OntoException("RDF4JException occurred.");
//		}
//		return json.toString();
		return null;
	}

	public List<List<String>> queryRepository(String serverURL, String repositoryID, String queryString)
			throws AutoMechCalibAgentException {
		List<List<String>> processedResultList = new ArrayList<List<String>>();

		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
			RepositoryConnection con = repo.getConnection();

			try {
				System.out.println("QueryString:\n" + queryString);
				TupleQuery queryResult = con.prepareTupleQuery(queryString);
				try (TupleQueryResult result = queryResult.evaluate()) {
					processResult(result, processedResultList);
				} finally {
					repo.shutDown();
				}
			} catch (Exception e) {
				logger.error("Exception occurred.");
				e.printStackTrace();
				throw new AutoMechCalibAgentException("Exception occurred.");
			} finally {
				logger.info("Executed the command to close the connection to the repository.");
				con.close();
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
			throw new AutoMechCalibAgentException("RDF4JException occurred.");
		}
		return processedResultList;
	}

//	/**
//	 * Initialises property values.
//	 * 
//	 * @throws OntoException
//	 */
//	private static void init() throws OntoException {
//		if (applicationContext == null) {
//			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
//		}
		// need to modify this part
//		if (ontochemexpKB == null) {
//			ontochemexpKB = applicationContext.getBean(OntoChemExp.class);
//		}
		// get the property values
//		ONTOCHEM_KB_URL = ontoChemKB.getOntoKinKbURL();
//		ONTOCHEM_TBOX_IRI = ontoChemKB.getOntoKinKbTBoxIri();
//		ONTOCHEM = "PREFIX ontochem: <"+ONTOCHEM_TBOX_IRI+"#> \n";
//		SERVER_URL = ontoChemKB.getOntoChemKBRDF4JServerUrl();
//		REPOSITORY_ID = ontoChemKB.getOntoChemKBRDF4JRepositoryId();
//		ONTOCHEM_KB_ABOX_FILE_PATH = ontoChemKB.getOntoChemKBABoxFilePath();
//		if(ONTOCHEM_KB_URL==null || ONTOCHEM_KB_URL.isEmpty()){
//			logger.info("The value of the property ontochem.kb.url in the jps-project.properties file is null or empty.");
//			throw new OntoException("The value of the property ontochem.kb.url in the jps-project.properties file is null or empty.");
//		}
//		if(ONTOCHEM_TBOX_IRI==null || ONTOCHEM_TBOX_IRI.isEmpty()){
//			logger.info("The value of the property ontochem.kb.tbox.iri in the jps-project.properties file is null or empty.");
//			throw new OntoException("The value of the property ontochem.kb.tbox.iri in the jps-project.properties file is null or empty.");
//		}
//		if(SERVER_URL==null || SERVER_URL.isEmpty()){
//			logger.info("The value of the property ontochem.kb.rdf4j.server.url in the jps-project.properties file is null or empty.");
//			throw new OntoException("The value of the property ontochem.kb.rdf4j.server.url in the jps-project.properties file is null or empty.");
//		}
//		if(REPOSITORY_ID==null || REPOSITORY_ID.isEmpty()){
//			logger.info("The value of the property ontochem.kb.rdf4j.repository.id in the jps-project.properties file is null or empty.");
//			throw new OntoException("The value of the property ontochem.kb.rdf4j.repository.id in the jps-project.properties file is null or empty.");
//		}
//	}

	/**
	 * Checks the validity of the following parameters:</br>
	 * 1. The Server URL.</br>
	 * 2. The abox file name.</br>
	 * 3. The abox file path.</br>
	 * 4. The base URL.</br>
	 * 5. The Knowledge Base repository ID.
	 * 
	 * @param serverURL
	 * @param aboxFileName
	 * @param aboxFilePath
	 * @param baseURI
	 * @param repositoryID
	 * @throws OntoException
	 */
	private void checkUploadParameterValidity(String serverURL, String aboxFileName, String aboxFilePath,
			String baseURI, String repositoryID) throws OntoException {
		checkURLValidity(serverURL, "The server URL");
		checkStringValidity(aboxFileName, "The abox file name");
		checkFilePathValidity(aboxFilePath, "The abox file path");
		checkURLValidity(baseURI, "The base IRI");
		checkStringValidity(repositoryID, "The repository ID");
	}

	/**
	 * Checks the validity of the following parameters:</br>
	 * 1. The Server URL.</br>
	 * 2. The abox file name.</br>
	 * 3. The context URL.</br>
	 * 4. The repository ID.</br>
	 * 5. The abox file path.
	 * 
	 * @param serverURL
	 * @param aboxFileName
	 * @param contextURL
	 * @param repositoryID
	 * @param aboxFilePath
	 * @throws OntoException
	 */
	private void checkDownloadParameterValidity(String serverURL, String aboxFileName, String contextURL,
			String repositoryID, String aboxFilePath) throws OntoException {
		checkURLValidity(serverURL, "The server URL");
		checkStringValidity(aboxFileName, "The mechanism name");
		checkURLValidity(contextURL, "The context URL");
		checkStringValidity(repositoryID, "The repository id");
		checkFilePathValidity(aboxFilePath, "The file path");
	}

	/**
	 * Checks the validity of the following parameters:</br>
	 * 1. The Server URL.</br>
	 * 2. The abox file name.</br>
	 * 3. The context URL.</br>
	 * 4. The repository ID.</br>
	 * 
	 * @param serverURL
	 * @param aboxFileName
	 * @param contextURL
	 * @param repositoryID
	 * @throws OntoException
	 */
	private void checkDeleteParameterValidity(String serverURL, String aboxFileName, String contextURL,
			String repositoryID) throws OntoException {
		checkURLValidity(serverURL, "The server URL");
		checkStringValidity(aboxFileName, "The mechanism name");
		checkURLValidity(contextURL, "The context URL");
		checkStringValidity(repositoryID, "The repository id");
	}

	/**
	 * Checks the validity of a URL.
	 * 
	 * @param url
	 * @param message
	 * @throws OntoException
	 */
	private void checkURLValidity(String url, String message) throws OntoException {
		if (url == null) {
			if (message != null) {
				throw new OntoException(message.concat("is null."));
			}
		}
		if (url.isEmpty()) {
			throw new OntoException(message.concat(" is empty."));
		}
		if (!IRI.create(url).isIRI()) {
			throw new OntoException(message.concat(" is not valid."));
		}
	}

	/**
	 * Checks the validity of a string value.</br>
	 * It checks whether the string value is null or empty.
	 * 
	 * @param string
	 * @param message
	 * @throws OntoException
	 */
	private void checkStringValidity(String string, String message) throws OntoException {
		if (string == null) {
			if (message != null) {
				throw new OntoException(message.concat(" is null."));
			}
		}
		if (string.isEmpty()) {
			throw new OntoException(message.concat(" is empty."));
		}
	}

	/**
	 * Checks the validity of a file system file path.</br>
	 * It checks whether the path is valid file path, null or empty.
	 * 
	 * @param path
	 * @param message
	 * @throws OntoException
	 */
	private void checkFilePathValidity(String path, String message) throws OntoException {
		File file = new File(path);
		if (path == null) {
			if (message != null) {
				throw new OntoException(message.concat(" is null."));
			}
		}
		if (path.isEmpty()) {
			throw new OntoException(message.concat(" is empty."));
		}
		if (!file.exists()) {
//			throw new OntoException("The following file does not exist:"+path);
		}
	}

	/**
	 * Produces and returns the given result in JSON format.
	 * 
	 * @param json
	 * @param result
	 * @return
	 */
	private StringBuilder getResultInJson(StringBuilder json, TupleQueryResult result) {
		json.append("[\n");
		// we just iterate over all solutions in the result...
		while (result.hasNext()) {
			BindingSet solution = result.next();
			json.append("  {\n");
			int size = solution.getBindingNames().size();
			int count = 0;
			for (String bindingName : solution.getBindingNames()) {
				json.append("    \"");
				json.append(bindingName);
				json.append("\"");
				json.append(":");
				json.append(" ");
				json.append(removeDataType(solution.getValue(bindingName).toString()));
				if (++count < size) {
					json.append(",");
				}
				json.append("\n");
			}
			json.append("  }\n");
		}
		json.append("]");
		return json;
	}

	/**
	 * Removes the OWL API dependent extra characters from the result.
	 * 
	 * @param result
	 * @param processedResult
	 */
	private void processResult(TupleQueryResult result, List<List<String>> processedResultList) {
		List<String> columnTitles = new ArrayList<>();
		for (String bindingName : result.getBindingNames()) {
			columnTitles.add(bindingName);
		}
		processedResultList.add(columnTitles);
		// we just iterate over all solutions in the result...
		while (result.hasNext()) {
			BindingSet solution = result.next();

			List<String> processedResult = new ArrayList<>();
			for (String bindingName : solution.getBindingNames()) {
				processedResult.add(removeDataType(solution.getValue(bindingName).toString()));
			}
			processedResultList.add(processedResult);
		}
	}

	/**
	 * Removes the following XML Schema data types from a string:</br>
	 * 1. string</br>
	 * 2. integer</br>
	 * 3. float</br>
	 * 4. double.
	 * 
	 * @param value
	 * @return
	 */
	private String removeDataType(String value) {
		String stringType = "^^<http://www.w3.org/2001/XMLSchema#string>";
		String integerType = "^^<http://www.w3.org/2001/XMLSchema#integer>";
		String floatType = "^^<http://www.w3.org/2001/XMLSchema#float>";
		String doubleType = "^^<http://www.w3.org/2001/XMLSchema#double>";
		if (value.contains(stringType)) {
			value = value.replace(stringType, "");
		} else if (value.contains(integerType)) {
			value = value.replace(integerType, "");
			value = replaceInvertedComma(value);
		} else if (value.contains(floatType)) {
			value = value.replace(floatType, "");
			value = replaceInvertedComma(value);
		} else if (value.contains(doubleType)) {
			value = value.replace(doubleType, "");
			value = replaceInvertedComma(value);
		} else if (value.startsWith("\"") || value.endsWith("\"")) {
			value = value.replace("\"", "");
		}
		return value;
	}

	/**
	 * Removes inverted commas from a string.
	 * 
	 * @param value
	 * @return
	 */
	private String replaceInvertedComma(String value) {
		if (value.contains("\"")) {
			value = value.replace("\"", "");
		}
		return value;
	}
}
