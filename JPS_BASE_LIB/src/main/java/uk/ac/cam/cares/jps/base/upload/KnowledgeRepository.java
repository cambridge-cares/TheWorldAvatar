package uk.ac.cam.cares.jps.base.upload;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOError;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.GraphQueryResult;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.rio.RDFFormat;

import com.bigdata.rdf.sail.webapp.SD;
import com.bigdata.rdf.sail.webapp.client.IPreparedTupleQuery;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository;
import com.bigdata.rdf.sail.webapp.client.RemoteRepository.AddOp;
import com.bigdata.rdf.sail.webapp.client.RemoteRepositoryManager;

import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * This class has been designed and developed to enable performing upload<br>
 * operations on any knowledge-graph built using Blazegraph.
 * 
 * @author msff2
 *
 */
public class KnowledgeRepository {
	Logger log = Logger.getLogger(KnowledgeRepository.class.getName());
	
	private String endPointURL;
	private String repositoryName;
	private RDFStoreType storeType;
	private String ontologyFilePath;
	private String ontologyDirectory;
	private String query;
	
	/**
	 * Declared to return the list of files produced by the getDirectoryFiles()<p>
	 * method.
	 */
	private List<File> files = new ArrayList<>();
	
	/**
	 * The default constructor.
	 * 
	 */
	public KnowledgeRepository(){
		
	}
	
	/**
	 * Constructor defined to be able to assign data to properties that are<br> 
	 * needed to operate on corresponding methods.
	 * 
	 * @param endPointURL
	 * @param repositoryName
	 * @param storeType
	 */
	public KnowledgeRepository(String endPointURL, String repositoryName, RDFStoreType storeType){
		this.endPointURL = endPointURL;
		this.repositoryName = repositoryName;
		this.storeType = storeType;
	}
	
	/**
	 * Constructor defined to be able to assign data to the following properties<br> 
	 * to operate on corresponding methods.
	 * 
	 * @param endPointURL
	 * @param repositoryName
	 * @param ontologyFilePath it is set to upload a single ontology. In the<br>
	 * case of uploading ontologies from a directory, this can be left empty or null.
	 * @param ontologyDirectory it is set to upload all ontologies available<br>
	 * under a directory. In the case of uploading a single ontology from a<br>
	 * given path, this can be left empty or null.
	 */
	public KnowledgeRepository(String endPointURL, String repositoryName, String ontologyFilePath, String ontologyDirectory){
		this.endPointURL = endPointURL;
		this.repositoryName = repositoryName;
		this.ontologyFilePath = ontologyFilePath;
		this.ontologyDirectory = ontologyDirectory;
	}
	
	/**
	 * Constructor defined to be able to assign data to the following properties<br> 
	 * to operate on corresponding methods.
	 * 
	 * @param endPointURL
	 * @param repositoryName
	 * @param storeType
	 * @param query
	 */
	public KnowledgeRepository(String endPointURL, String repositoryName, RDFStoreType storeType, String query){
		this.endPointURL = endPointURL;
		this.repositoryName = repositoryName;
		this.storeType = storeType;
		this.query = query;
	}
	
	/**
	 * Creates the instance of the current repository (knowledge base) if it<br>
	 * exists and returns it.
	 * 
	 * @return
	 * @throws Exception
	 */
	public RemoteRepository getRepository() throws Exception{
		try{
			checkRepositoryDataAvailability(this.endPointURL, this.repositoryName);
			checkStoreTypeDataAvailability(this.storeType);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		RemoteRepository repository = null;
		if(this.storeType.toString().equals(RDFStoreType.BLAZEGRAPH.toString())){
			RemoteRepositoryManager repositoryManager = new RemoteRepositoryManager(endPointURL, false);
			if(repositoryExists(endPointURL, repositoryName, repositoryManager)){
				repository = repositoryManager.getRepositoryForNamespace(repositoryName);
				repositoryManager.close();
				return repository; 
			}
		}
		return repository;
	}

	/**
	 * Uploads an ontology file to the current repository.
	 * 
	 * @throws Exception
	 */
	public void uploadAnOntology() throws Exception {
		try{
			checkRepositoryDataAvailability(this.endPointURL, this.repositoryName);
			checkOntologyUploadDataAvailability(this.ontologyFilePath);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		RemoteRepository repository = getRepository(this.endPointURL, this.repositoryName, RDFStoreType.BLAZEGRAPH);
		
		if (repository != null) {
			final InputStream is = new FileInputStream(new File(this.ontologyFilePath));
			try {
				repository.add(new AddOp(is, RDFFormat.forMIMEType("application/xml")));
			} finally {
				is.close();
			}
		} else {
			log.info("The following repository does not exist: " + endPointURL + repositoryName);
			log.info("Create a repository with this name and try again.");
		}
	}
	
	/**
	 * Uploads a single ontology file to the current repository.
	 * 
	 * @throws Exception
	 */
	public void uploadOntology() throws Exception {
		try{
			checkRepositoryDataAvailability(this.endPointURL, this.repositoryName);
			checkOntologyUploadDataAvailability(this.ontologyFilePath);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		RemoteRepository repository = getRepository(this.endPointURL, this.repositoryName, RDFStoreType.BLAZEGRAPH);
		
		if (repository != null) {
			final InputStream is = new FileInputStream(new File(this.ontologyFilePath));
			try {
				repository.add(new AddOp(is, RDFFormat.forMIMEType("application/xml")));
			} finally {
				is.close();
			}
		} else {
			log.info("The following repository does not exist: " + endPointURL + repositoryName);
			log.info("Create a repository with this name and try again.");
		}
	}
	
	/**
	 * Uploads all ontology files available under the given folder to<br>
	 * the current repository.
	 * 
	 * @throws Exception
	 */
	public void uploadOntologies() throws Exception{
		String importedFileLog = "imported-file.log";
		String importErrorDetailedLog = "import-error-detailed.log";
		String nonRdfOrOwlFileLog = "non-rdf-or-owl-file.log";
		FileWriter fWDetailedLog = new FileWriter(importErrorDetailedLog, true);
		BufferedWriter bWDetailedLog = new BufferedWriter(fWDetailedLog);
		FileWriter fW = new FileWriter(importedFileLog, true);
		BufferedWriter bWImported = new BufferedWriter(fW);
		FileWriter fWNonRdfOrOwlFile = new FileWriter(nonRdfOrOwlFileLog, true);
		BufferedWriter bWNonRdfOrOwlFile = new BufferedWriter(fWNonRdfOrOwlFile);
		// Reading the list of already imported files to avoid re-importing them.
		BufferedReader br = FileUtil.openSourceFile(importedFileLog);
		List<String> listOfImportedFiles = new ArrayList<>();
		String line = "";
		while((line=br.readLine())!=null){
			listOfImportedFiles.add(line.trim());
		}
		br.close();
		System.out.println("No of already imported files: "+listOfImportedFiles.size());
		int n_of_problematic_abox = 0;
		int non_ref_owl_file = 0;
		try{
			checkRepositoryDataAvailability(this.endPointURL, this.repositoryName);
			checkOntologyDiretoryAvailability(this.ontologyDirectory);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		File dir = new File(this.ontologyDirectory);
		if(dir.isDirectory()){
			int i = 0;
			FileUtil fileUtil = new FileUtil();
			List<File> files = fileUtil.getDirectoryFiles(dir, Arrays.asList(".owl",".rdf"));
			System.out.println("Total number of files to upload: "+files.size());
			if(files==null){
				return;
			}
			for(File file:files){
				if(file.isFile()){
					if(listOfImportedFiles.contains(file.getAbsolutePath())){
						System.out.println("Already imported file: "+file.getAbsolutePath()+", so skipping.");
						continue;
					}
					try{
						if(isOwlOrRdf(file.getAbsolutePath())){
							uploadOntology(endPointURL, repositoryName, file.getAbsolutePath());
							bWImported.write(file.getAbsolutePath()+"\n");
						}else{
							bWNonRdfOrOwlFile.write("["+ ++non_ref_owl_file + "] File "+file.getName()+" is not imported as it is not in OWL or RDF format.");
							System.out.println("["+ non_ref_owl_file + "] File "+file.getName()+" is not imported as it is not in OWL or RDF format.");
							continue;
						}
					log.info("["+ ++i+"] Uploaded "+file.getAbsolutePath());
					if(i%1000 == 0){
						TimeUnit.SECONDS.sleep(5);
					}
					}catch(Exception e){
						bWDetailedLog.write("["+ ++n_of_problematic_abox + "] File "+file.getName()+" could not be imported due to " + e.getMessage());
						System.out.println("["+ n_of_problematic_abox + "] File "+file.getName()+" could not be imported due to " + e.getMessage());
						System.out.println("Now the tool will stop. Run it again to finish the import.");
						TimeUnit.MILLISECONDS.sleep(500);
						bWDetailedLog.close();
						bWImported.close();
						bWNonRdfOrOwlFile.close();
						System.exit(0);
					}
				}
			}
		}
		bWImported.close();
		bWNonRdfOrOwlFile.close();
	}
	
	/**
	 * Verifies if the current file is an OWL or RDF file.
	 * 
	 * @param path the path to an OWL or RDF file
	 * @return
	 */
	public boolean isOwlOrRdf(String path) throws IOException{
		BufferedReader br = FileUtil.openSourceFile(path);
		String line;
		String xmlDeclaration = "^<\\?xml.*?\\?>";
		String rdfStartingTag = "^<rdf:RDF.*?";
		String rdfClosingTag = "^</rdf:RDF>";
		boolean isXmlDeclarationAppears = false;
		boolean isRdfStartingTagAppears = false;
		boolean isRdfClosingTagAppears = false;
		
		while((line=br.readLine())!=null){
			if(line.trim().matches(xmlDeclaration)){
				isXmlDeclarationAppears = true;
			}
			if(line.trim().matches(rdfStartingTag)){
				isRdfStartingTagAppears = true;
			}
			if(line.trim().matches(rdfClosingTag)){
				isRdfClosingTagAppears = true;
			}
		}
		
		if(isXmlDeclarationAppears && isRdfStartingTagAppears && isRdfClosingTagAppears){
			return true;
		}
		return false;
	}

	/**
	 * Performs any SPARQL query against the provided repository.
	 * 
	 * @return
	 * @throws Exception
	 */
	public String query() throws Exception {
		try{
			checkRepositoryDataAvailability(this.endPointURL, this.repositoryName);
			checkStoreTypeDataAvailability(this.storeType);
			checkQueryAvailability(this.query);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		StringBuilder json = new StringBuilder();
		RemoteRepository repository = getRepository(this.endPointURL, this.repositoryName, this.storeType);
		final IPreparedTupleQuery tupleQuery = repository.prepareTupleQuery(this.query);
		final TupleQueryResult result = tupleQuery.evaluate();
		log.info("Query Result: "+result);
		System.out.println("Query Result: "+result);
		try {
			json = getResultInJson(json, result);	
		} finally {
			result.close();
		}
		return json.toString();
	}
	
	/**
	 * Creates the instance of the current repository (knowledge base) if it<br>
	 * exists and returns it.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param storeType the name of knowledge storage, e.g. Blazegraph and RDF4J.
	 */
	public RemoteRepository getRepository(String endPointURL, String repositoryName, RDFStoreType storeType) throws Exception{
		try{
			checkRepositoryDataAvailability(endPointURL, repositoryName);
			checkStoreTypeDataAvailability(storeType);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		RemoteRepository repository = null;
		if(storeType.toString().equals(RDFStoreType.BLAZEGRAPH.toString())){
			RemoteRepositoryManager repositoryManager = new RemoteRepositoryManager(endPointURL, false);
			if(repositoryExists(endPointURL, repositoryName, repositoryManager)){
				repository = repositoryManager.getRepositoryForNamespace(repositoryName);
				repositoryManager.close();
				return repository; 
			}
		}
		return repository;
	}
	
	/**
	 * Checks the availability of a repository (knowledge base) on a triple store.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param repositoryManager an instance of the repository manager.
	 * @return
	 * @throws Exception
	 */
	private boolean repositoryExists(String endPointURL, String repositoryName, RemoteRepositoryManager repositoryManager) throws Exception{
		final GraphQueryResult res = repositoryManager.getRepositoryDescriptions();
		try{
			while(res.hasNext()){
				final Statement stmt = res.next();
				if (stmt.getPredicate().toString().equals(SD.KB_NAMESPACE.stringValue())) {
					if(repositoryName.equals(stmt.getObject().stringValue())){
						return true;
					}
				}
			}
		} finally {
			res.close();
		}
		return false;
	}
	
	/**
	 * Uploads a single ontology file to the current repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param ontologyFilePath the absolute path to the ontology file, e.g. 
	 * C:/path/to/the/ontology/ontokin.owl and C:/path/to/the/ontology/ABF.owl.
	 * @throws Exception
	 */
	public void uploadOntology(String endPointURL, String repositoryName, String ontologyFilePath) throws Exception {
		try{
			checkRepositoryDataAvailability(endPointURL, repositoryName);
			checkOntologyUploadDataAvailability(ontologyFilePath);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		try {
			RemoteRepository repository = getRepository(endPointURL, repositoryName, RDFStoreType.BLAZEGRAPH);
			if (repository != null) {
				final InputStream is = new FileInputStream(new File(ontologyFilePath));
				try {
					repository.add(new AddOp(is, RDFFormat.forMIMEType("application/xml")));
				} finally {
					is.close();
				}
			} else {
				log.info("The following repository does not exist: " + endPointURL + repositoryName);
				log.info("Create a repository with this name and try again.");
			}
		} catch (Exception e) {
			System.out.println("UploadOntology:" + e.getMessage());
			System.out.println("UploadOntology: uploading the file " + ontologyFilePath);
			throw new Exception(e.getMessage());
		}
	}
	
	/**
	 * Uploads all ontology files available under the given folder to<br>
	 * the current repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param ontologyDirectory the path to the folder containing a list<br>
	 * of ontologies, e.g. C:/path/to/the/ontology_folder.
	 * @throws Exception
	 */
	public void uploadOntologies(String endPointURL, String repositoryName, String ontologyDirectory) throws Exception{
		try{
			checkRepositoryDataAvailability(endPointURL, repositoryName);
			checkOntologyDiretoryAvailability(ontologyDirectory);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		File dir = new File(ontologyDirectory);
		if(dir.isDirectory()){
			int i = 0;
			FileUtil fileUtil = new FileUtil();
			List<File> files = fileUtil.getDirectoryFiles(dir, Arrays.asList(".owl",".rdf"));
			System.out.println("Total number of files to upload: "+files.size());
			if(files==null){
				return;
			}
			for(File file:files){
				if(file.isFile()){
					uploadOntology(endPointURL, repositoryName, file.getAbsolutePath());
					log.info("["+ ++i+"] Uploaded "+file.getAbsolutePath());
				}
			}
		}
	}

	/**
	 * Performs any SPARQL query against the provided repository.
	 * 
	 * @param endPointURL the URL of the current triple store EndPoint, e.g.<br>
	 * http://theworldavatar.com/blazegraph and http://theworldavatar.com/rdf4j-server 
	 * @param repositoryName the name of the current repository, e.g.<br>
	 * ontokin and ontocompchem.
	 * @param storeType the name of knowledge storage, e.g. Blazegraph and RDF4J.
	 * @param query the query that is being performed.
	 * @return
	 * @throws Exception
	 */
	public String query(String endPointURL, String repositoryName, RDFStoreType storeType, String query) throws Exception {
		try{
			checkRepositoryDataAvailability(endPointURL, repositoryName);
			checkStoreTypeDataAvailability(storeType);
			checkQueryAvailability(query);
		}catch(Exception e){
			throw new Exception(e.getMessage());
		}
		StringBuilder json = new StringBuilder();
		RemoteRepository repository = getRepository(endPointURL, repositoryName, storeType);
		final IPreparedTupleQuery tupleQuery = repository.prepareTupleQuery(query);
		final TupleQueryResult result = tupleQuery.evaluate();
		System.out.println(result);
		try {
			json = getResultInJson(json, result);	
		} finally {
			result.close();
		}
		return json.toString();
	}

	/**
	 * Produces and returns the given result in JSON format.
	 * 
	 * @param json
	 * @param result
	 * @return
	 */
	private StringBuilder getResultInJson(StringBuilder json, TupleQueryResult result) {
		json.append("{\n");
		json.append("  \"head\" : {\n");
		json.append("    \"vars\" : [\n");
		try{
		// flag to close the header variables created above and to start the results
		boolean flag = true; 
		// we just iterate over all solutions in the result...
		while (result.hasNext()) {
			BindingSet solution = result.next();
			int count = 0;
			int size = solution.getBindingNames().size();
			if(flag){
				for(String bindingName: solution.getBindingNames()){
					json.append("      \"");
					json.append(bindingName);
					json.append("\"");
					if(++count<size){
						json.append(",");
					}
					json.append("\n");
				}
				json.append("    ]\n");
				json.append("  },\n");
				json.append("  \"results\" : {\n");
				json.append("    \"bindings\" : [\n");
				flag = false;
			}
			count = 0;
			json.append("      {\n");
			for (String bindingName : solution.getBindingNames()) {
				json.append("        \"");
				json.append(bindingName);
				json.append("\" : {\n");
				json.append("          \"value\" : ");
				json.append(jsonifyString(solution.getValue(bindingName).toString()));
				json.append("\n        }");
				if(++count<size){
					json.append(",\n");
				}else{
					json.append("\n");
				}
			}
			json.append("      },\n");
		}
		if (json.toString().contains(",")) {
			json.replace(json.lastIndexOf(","), json.lastIndexOf(",")+1, "");
		}
		}catch(QueryEvaluationException e){
			log.info(e.getMessage());
		}
		json.append("    ]\n");
		json.append("  }\n");
		json.append("}\n");
		return json;
	}
	
	/**
	 * Converts a value string into its JSON equivalent.</br>
	 * However, currently it cannot produce a valid JSON equivalent</br>
	 * for comments.
	 * 
	 * @param value
	 * @return
	 */
	private String jsonifyString(String value){
		String stringType = "^^<http://www.w3.org/2001/XMLSchema#string>";
		String integerType = "^^<http://www.w3.org/2001/XMLSchema#integer>";
		String floatType = "^^<http://www.w3.org/2001/XMLSchema#float>";
		if(value.contains(stringType)){
			value = value.replace(stringType, "");
		} else if(value.contains(integerType)){
			value = value.replace(integerType, "");
			value = replaceInvertedComma(value);
		} else if(value.contains(floatType)){
			value = value.replace(floatType, "");
			value = replaceInvertedComma(value);
		} else {
			value = "\""+value+"\"";
		}
		return value;
	}
	
	/**
	 * Removes the start and end inverted commas from a string.
	 * 
	 * @param value
	 * @param type
	 * @return
	 */
	private String replaceInvertedComma(String value){
		if(value.startsWith("\"")){
			value = value.replaceFirst("\"", "");
		}
		if(value.endsWith("\"")){
			value = value.substring(0, value.length()-1);
		}		
		return value;
	}

	public String getEndPointURL() {
		return endPointURL;
	}

	public void setEndPointURL(String endPointURL) {
		this.endPointURL = endPointURL;
	}

	public String getRepositoryName() {
		return repositoryName;
	}

	public void setRepositoryName(String repositoryName) {
		this.repositoryName = repositoryName;
	}

	public RDFStoreType getStoreType() {
		return storeType;
	}

	public void setStoreType(RDFStoreType storeType) {
		this.storeType = storeType;
	}

	public String getOntologyFilePath() {
		return ontologyFilePath;
	}

	public void setOntologyFilePath(String ontologyFilePath) {
		this.ontologyFilePath = ontologyFilePath;
	}

	public String getOntologyDirectory() {
		return ontologyDirectory;
	}

	public void setOntologyDirectory(String ontologyDirectory) {
		this.ontologyDirectory = ontologyDirectory;
	}

	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
	
	/**
	 * Checks the availability of the following data required to call the corresponding methods.
	 * 
	 * @param endPointURL
	 * @param repositoryName
	 * @throws Exception
	 */
	private void checkRepositoryDataAvailability(String endPointURL, String repositoryName)
			throws Exception {
		if (endPointURL == null) {
			throw new Exception("The value of endPointURL is null.");
		}
		if (endPointURL.trim().isEmpty()) {
			throw new Exception("The value of endPointURL is empty.");
		}
		if (repositoryName == null) {
			throw new Exception("The value of repositoryName is null.");
		}
		if (repositoryName.isEmpty()) {
			throw new Exception("The value of repositoryName is emptry.");
		}
//		if (this.storeType == null) {
//			throw new Exception("The value of storeType is null.");
//		}
	}
	
	/**
	 * Checks the availability of the following data required to call the corresponding methods.
	 * 
	 * @param endPointURL
	 * @param repositoryName
	 * @throws Exception
	 */
	private void checkStoreTypeDataAvailability(RDFStoreType storeType) throws Exception {
		if (storeType == null) {
			throw new Exception("The value of storeType is null.");
		}
	}
	
	/**
	 * Checks the availability of the following data required to call the corresponding methods.
	 * 
	 * @param ontologyFilePath
	 * @throws Exception
	 */
	private void checkOntologyUploadDataAvailability(String ontologyFilePath)
			throws Exception {
		if (ontologyFilePath == null) {
			throw new Exception("The value of ontologyFilePath is null.");
		}
		if (ontologyFilePath == null) {
			throw new Exception("The value of ontologyFilePath is empty.");
		}
	}
	
	/**
	 * Checks the availability of the following data required to call the corresponding methods.
	 * 
	 * @param ontologyDirectory
	 * @throws Exception
	 */
	private void checkOntologyDiretoryAvailability(String ontologyDirectory)
			throws Exception {
		if (ontologyDirectory == null) {
			throw new Exception("The value of ontologyDirectory is null.");
		}
		if (ontologyDirectory == null) {
			throw new Exception("The value of ontologyDirectory is empty.");
		}
	}
	
	/**
	 * Checks the availability of the query required to call the corresponding query methods.
	 * 
	 * @param query
	 * @throws Exception
	 */
	private void checkQueryAvailability(String query)
			throws Exception {
		if (query == null) {
			throw new Exception("The value of query is null.");
		}
		if (query == null) {
			throw new Exception("The value of query is empty.");
		}
	}
	
	/**
	 * The main method of the class developed to import ontologies (both TBox<p>
	 * and ABox)
	 * 
	 * @param args
	 */
	public static void main(String[] args){
		if(args.length>=3){
			if(args[0].isEmpty()){
				System.out.println("The first argument is empty. Provide an Endpoint URL like 'http://localhost:8080/blazegraph'");
				System.exit(0);
			}
			if(args[1].isEmpty()){
				System.out.println("The second argument is empty. Provide a repository name like 'ontokin'");
				System.exit(0);
			}
			if(args[2].isEmpty()){
				System.out.println("The third argument is empty. Provide the path to the directory where ontologies reside like C:\\data\\kb");
				System.exit(0);
			}
			KnowledgeRepository kr = new KnowledgeRepository();
			kr.endPointURL = args[0];
			kr.repositoryName = args[1];
			kr.ontologyDirectory = args[2];
			try{
				kr.uploadOntologies();
				System.out.println("To upload the already uploaded files, please delete the imported-file.log file.");
				System.exit(0);
			}catch(Exception e){
				System.out.println(e.getMessage());
			}
		}else{
			System.out.println("For uploading ontologies from a directory provide the URL of Endpoint, name of repository and absolute path of the ontology directory as follows:");
			System.out.println("http://localhost:8080/blazegraph ontokin C:\\data\\kb");
		}
	}
}
