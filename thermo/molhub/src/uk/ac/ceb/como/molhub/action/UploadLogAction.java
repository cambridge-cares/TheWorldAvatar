package uk.ac.ceb.como.molhub.action;

import java.io.BufferedReader;
import java.io.File;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;

import java.util.List;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import org.apache.commons.exec.ExecuteException;
import org.apache.log4j.Logger;

import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import com.opensymphony.xwork2.ActionSupport;
import com.opensymphony.xwork2.ValidationAware;

import uk.ac.cam.ceb.como.compchem.ontology.InconsistencyExplanation;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.ceb.como.molhub.bean.GaussianUploadReport;
import uk.ac.ceb.como.molhub.model.ExecutorManager;
import uk.ac.ceb.como.molhub.model.FolderManager;
import uk.ac.ceb.como.molhub.model.PropertiesManager;


/**
 * The Class uploads one or more selected Gaussian files (g09)<br>
 * on server, and generates XML, ontology file, image file, and<br>
 * adds ontologies into triple store (RDF4J).
 *
 * @author nk510 (Nenad Krdzavac)
 * @author msff2 (Feroz Farazi)
 * 
 */

public class UploadLogAction extends ActionSupport implements ValidationAware {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(UploadLogAction.class.getName());

	/**
	 * @author NK510 Adds molhub properties such as: Folder path where g09, xml and
	 *         png files are stored. Folder path where generated ontology is stored.
	 *         Xslt file path. Xsd file path. Jmol data file path, that is used to
	 *         generated png file.
	 */
	Properties molhubPropreties = PropertiesManager.loadProperties(UploadLogAction.class.getClassLoader().getResourceAsStream("molhub.management.properties"));

	private String dataFolderPath = molhubPropreties.getProperty("data.folder.path").toString();

	private String kbFolderPath = molhubPropreties.getProperty("kb.folder.path").toString();
	
	private String jmolDataJarFilePath = molhubPropreties.getProperty("jmol.data.jar.file.path").toString();
	
	private String pythonParserPath = molhubPropreties.getProperty("python.parser.file.path").toString();
	

	/**
	 * 
	 * @author NK510 Adds kb properties such as: OntoCompChem URI, RDF4J server URL.
	 * 
	 */
	Properties kbProperties = PropertiesManager.loadProperties(UploadLogAction.class.getClassLoader().getResourceAsStream("kb.ontocompchem.management.properties"));

	private final String ONTOCOMPCHEM_KB_URL = kbProperties.getProperty("ontocompchem.kb.uri").toString();
	
	private String serverURL = kbProperties.getProperty("ontocompchem.kb.local.rdf4j.server").toString();
	
	private String REPOSITORY_ID = kbProperties.getProperty("ontocompchem.repository.id").toString();

	/** The files. */
	private List<File> files = new ArrayList<File>();

	/** The upload file name. */
	private String[] uploadFileName;

	/** The upload content type. */
	private String[] uploadContentType;
	
	/** The OntoSpecies entry that is connected to the current<br> 
	 * Gaussian calculation */
	private String ontoSpeciesIRI;

	/** The start time. */
	final long startTime = System.currentTimeMillis();

	/** The running time. */
	private String runningTime = null;

	/**
	 * The column.
	 *
	 * @author nk510
	 *         <p>
	 *         List of column names in table that reports about uploading process of
	 *         Gaussina file. Columns are named as (uuid, file name, XML validation,
	 *         OWL consistency).
	 *         </p>
	 */

	private List<String> column = new ArrayList<String>();

	/** The gaussian upload report. */
	private GaussianUploadReport gaussianUploadReport;

	/** The upload report list. */
	private List<GaussianUploadReport> uploadReportList = new ArrayList<GaussianUploadReport>();
	
	/**
	 * When user clicks on the upload button, this method executes<br>
	 * as it is instructed in the struts2.xml file, which is located in<br>
	 * molhub\WebContent\WEB-INF\classes.
	 */
	/* (non-Javadoc)
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 */
	@Override
	public String execute() throws Exception {
		int fileNumber = 0;
		/**
		 *  These column names appear in the generated report table after the upload. 
		 */
		if (!files.isEmpty()) {
			column.add("UUID");
			column.add("Gaussian file name ");
			column.add("OWL file name");
			column.add("OWL consistency");
		}
		/**
		 *  If user clicks on the upload button without selecting any files.
		 */
		if (files.isEmpty()) {
			addActionMessage("Please select Gaussian files first, and than press 'Upload' button.");
		}
		/**
		 *  For each file selected, it iterates once.
		 */
		
		
		for (File f : files) {	
			
			
			/**
			 *  Creates unique folder name for each uploaded Gaussian file (g09),  XML file, OWL file, and PNG file.
			 */
			String uuidFolderName = FolderManager.generateUniqueFolderName(f.getName());
			
			String fileExtension = uploadFileName[fileNumber].substring(uploadFileName[fileNumber].lastIndexOf(".") + 1);
			
			File inputG09File = new File(dataFolderPath + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + "." + fileExtension);
			
			/** Png file name is the same as the name of folder where that
			*
			* image is saved. Adds .png extension to the png file.
			*/
			File pngFile = new File(dataFolderPath + "/" + uuidFolderName + "/"
					+ uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".png");
			/**
			 *  Creates folders where molhub stores G09, xml and png files
			 */
			FolderManager.createFolder(dataFolderPath + "/" + uuidFolderName);
			/**
			 *  Creates a folder for saving the ontology.
			 */
			FolderManager.createFolder(kbFolderPath + "/" + uuidFolderName);
			/** 
			 * User uploaded Gaussian file is saved.
			 * 
			 */
			FolderManager.saveFileInFolder(inputG09File, f.getAbsolutePath());
				
				 /**
				 * Both in the if block and else block, it runs the XSLT
				 *  transformation and the UUID folder name generated earlier
				 *  is used as part of IRI of the OWL file.
				 */
				if((getOntoSpeciesIRI() == null) || (getOntoSpeciesIRI().trim().length() == 0)) {
					
					/**
					 * Runs python code that parses uploaded Gaussian file and generates json and owl files.
					 */	
				  
					new ExecutorManager().runParser(pythonParserPath+ " -f "+ dataFolderPath + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + "." + fileExtension + " -j True" + " -p " + kbFolderPath + "/" + uuidFolderName + "/" );
					
				}else {
					
					/**
					 * Verifies the validity of the OntoSpcecies IRI in the case
					 * of a single file upload.
					 */
					
					checkURLValidity(getOntoSpeciesIRI());
					
					/**
					 * @author NK510 (caresssd@hermes.cam.ac.uk)
					 * 
					 * TO DO: Here we should discuss how to implement adding unique species IRI into OWL file, upon uploading log files. There are two options:
					 * 1. Extend Angiras and Daniel's parser to generate owl file that will optionally contain unique species IRI
					 * 2. Implement Java method that will add given unique species IRI inside the content of generated owl file.
					 * 
					 */
					
				}

				
				List<File> owlFileList = new FileUtility().getArrayFileList(kbFolderPath  + uuidFolderName +"/", ".owl");
				
				logger.info("owlFileList.isEmpty(): " + owlFileList.isEmpty() + " owlFileList.size(): " + owlFileList.size());				
				
				
				if(!owlFileList.isEmpty()) {
					
				for(File owlFiles: owlFileList) {
				
				 File owl_File = new File(kbFolderPath + uuidFolderName + "/" + owlFiles.getName());
					
			    logger.info("ontology file path: " + owl_File.getAbsolutePath().toString());
			    
			    OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			    OWLOntology ontology = manager.loadOntologyFromOntologyDocument(owl_File);

			       
			        /**
			         * @author NK510 (caresssd@hermes.cam.ac.uk)
		             * Save the OWL file into RDF/XML format.
			         */
			    RDFXMLOntologyFormat rdfxmlFormat = new RDFXMLOntologyFormat();
			    manager.saveOntology(ontology, rdfxmlFormat, IRI.create(owl_File));
			        
				logger.info("owlFiles.getName(): " +owl_File.getName());
				
				/**
				 * It validates generated Compchem xml file against Compchem XML schema,
				 * and checks consistency of the generated Compchem ontology (ABox).
				 */
				
				boolean consistency = InconsistencyExplanation.getConsistencyOWLFile(kbFolderPath +  uuidFolderName + "/" + owl_File.getName());
				
				/**
				 * If the generated OWL file is valid, it is loaded to the triple
				 * store.
				 */
				
				if (consistency) {
					
				logger.info("owl_File.getName().toString(): " + owl_File.getName().toString());
				
				/**
				 * Load ontology into triple store.
				 */
				loadOntology(owlFiles, serverURL, owl_File.getName().toString(), kbFolderPath + uuidFolderName + "/",  uuidFolderName , REPOSITORY_ID);
				
				}				
				
				/**
				 * An error message is shown if the generated ontology (ABox) is
				 * inconsistent.
				 */
				if (!consistency) {
					addFieldError("term.name", "Ontology '" + owl_File.getName()
							+ "' is not consistent. Owl file is not loaded into triple store.");
					return ERROR;
				}
				
				gaussianUploadReport = new GaussianUploadReport(
						/**
						 * @author NK510 (caresssd@hermes.cam.ac.uk)
						 * Generates uploading report without information whether XML file is valid or not. This version of the code does not generate XML file. 
						 */
						uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1), uploadFileName[fileNumber], owl_File.getName(),consistency);
				
				uploadReportList.add(gaussianUploadReport);
				
				}
			}
				/**
				 * Generates image (.png file) from uploaded Gaussian file by
				 * using JmolData.jar.
				 * 
				 */
				String[] cmd = { "java", "-jar", jmolDataJarFilePath, "--nodisplay", "-j", "background white",
						inputG09File.getAbsolutePath().toString(), "-w",
						"png:" + pngFile.getAbsolutePath().toString() };
				
				Runtime.getRuntime().exec(cmd);
			
			fileNumber++;
			
			
		}
		
		NumberFormat formatter = new DecimalFormat("#00.000");
		final long endTime = System.currentTimeMillis();
		runningTime = formatter.format((endTime - startTime) / 1000d) + " seconds";
		
		if (!files.isEmpty()) {
			
			addActionMessage("Upload completed in " + runningTime);
		}
		
		
		return SUCCESS;
	}	
	
	/**
	 * Gets the upload.
	 *
	 * @return the upload
	 */

	public List<File> getUpload() {
		return files;
	}

	/**
	 * Sets the upload.
	 *
	 * @param upload the new upload
	 */
	public void setUpload(List<File> upload) {
		this.files = upload;
	}

	/**
	 * Gets the upload file name.
	 *
	 * @return the upload file name
	 */
	public String[] getUploadFileName() {
		return uploadFileName;
	}

	/**
	 * Sets the upload file name.
	 *
	 * @param uploadFileName the new upload file name
	 */

	public void setUploadFileName(String[] uploadFileName) {
		this.uploadFileName = uploadFileName;
	}
	
	/**
	 * Gets the IRI of the corresponding OntoSpecies entry of the current<br>
	 * Gaussian file.
	 * 
	 * @return
	 */
	public String getOntoSpeciesIRI() {
		return ontoSpeciesIRI;
	}

	/**
	 * Sets the IRI of the corresponding OntoSpecies entry of the current<br>
	 * Gaussian file. 
	 * 
	 * @param ontoSpeciesIRI
	 */
	public void setOntoSpeciesIRI(String ontoSpeciesIRI) {
		this.ontoSpeciesIRI = ontoSpeciesIRI;
	}

	/**
	 * Gets the upload report list.
	 *
	 * @return the upload report list
	 */
	public List<GaussianUploadReport> getUploadReportList() {
		return uploadReportList;
	}

	/**
	 * Sets the upload report list.
	 *
	 * @param uploadReportList the new upload report list
	 */
	public void setUploadReportList(List<GaussianUploadReport> uploadReportList) {
		this.uploadReportList = uploadReportList;
	}

	/**
	 * Gets the gaussian upload report.
	 *
	 * @return the gaussian upload report
	 */
	public GaussianUploadReport getGaussianUploadReport() {
		return gaussianUploadReport;
	}

	/**
	 * Sets the gaussian upload report.
	 *
	 * @param gaussianUploadReport the new gaussian upload report
	 */
	public void setGaussianUploadReport(GaussianUploadReport gaussianUploadReport) {
		this.gaussianUploadReport = gaussianUploadReport;
	}

	/**
	 * Gets the column.
	 *
	 * @return the column
	 */

	public List<String> getColumn() {
		return column;
	}

	/**
	 * Sets the column.
	 *
	 * @param column the new column
	 */

	public void setColumn(List<String> column) {

		/**
		 * @author nk510 Assigns names for each column in table report.
		 */

		this.column = column;
	}

	/**
	 * Gets the running time.
	 *
	 * @return the running time
	 */
	public String getRunningTime() {
		return runningTime;
	}

	/**
	 * Sets the running time.
	 *
	 * @param runningTime the new running time
	 */
	public void setRunningTime(String runningTime) {
		this.runningTime = runningTime;
	}

	/**
	 * Gets the files.
	 *
	 * @return the files
	 */
	public List<File> getFiles() {
		return files;
	}

	/**
	 * Sets the files.
	 *
	 * @param files the new files
	 */
	public void setFiles(List<File> files) {
		this.files = files;
	}

	/**
	 * Gets the upload content type.
	 *
	 * @return the upload content type
	 */
	public String[] getUploadContentType() {
		return uploadContentType;
	}

	/**
	 * Sets the upload content type.
	 *
	 * @param uploadContentType the new upload content type
	 */
	public void setUploadContentType(String[] uploadContentType) {
		this.uploadContentType = uploadContentType;
	}
	/**
	 * Sets the URL of the RDF4J server.
	 * @return
	 */
	public String getServerUrl() {
		return serverURL;
	}
	/**
	 * Reads the URL of the RDF4J server.
	 * @param serverUrl
	 */
	public void setServerUrl(String serverUrl) {
		this.serverURL = serverUrl;
	}
	
	/**
	 * @param iri the IRI
	 * @throws Exception
	 */
	private void checkURLValidity(String iri) throws Exception{
		if(iri==null){
				throw new Exception("Provided IRI is null.");
		}
		if(iri.isEmpty()){
			throw new Exception("Provided IRI is empty.");
		}
		try{
			URL url = new URL(iri);
			url.toURI();
		}catch(Exception e){
			throw new Exception("Provided IRI is not a valid URL.");
		}
	}

	/**
	 * Loads an ontology to the OntoCompChem KB repository. It also creates</br>
	 * a context, which is a necessary feature to delete the mechanism</br>
	 * if user wants.
	 * 
	 * @param serverURL the server url
	 * @param owlFileName owl file name
	 * @param owlFilePath owl file path
	 * @param baseFolder folder where owl file is stored
	 * @param repositoryID the repository id
	 * @throws OntoException
	 */
	public synchronized void loadOntology(File owlFile, String serverURL, String owlFileName, String owlFilePath, String baseFolder, String repositoryID) throws Exception{			
			
		try {
			
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
			RepositoryConnection con = repo.getConnection();
			ValueFactory f = repo.getValueFactory();
			
			org.eclipse.rdf4j.model.IRI context = f.createIRI(ONTOCOMPCHEM_KB_URL.concat(baseFolder + "/" +owlFileName));
			
			con.begin();
			
			try {
				
				URL url = new URL("file:/".concat(owlFilePath).concat(owlFileName));

				con.add(owlFile, url.toString(), RDFFormat.RDFXML,context);
				
				con.commit();
				
			} finally {
				con.close();
				repo.shutDown();
			}
			
			
			
		} catch (RDF4JException e) {
			
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
			
		} catch (IOException e) {
			
			logger.error("IOException occurred.");
			e.printStackTrace();
			
		}
	}
	
	
	private void runParser(String CMD) throws IOException, InterruptedException {
	    logger.info("Standard output: " + CMD);
	    Process process = Runtime.getRuntime().exec(CMD);

	    // Get input streams
	    BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));
	    BufferedReader stdError = new BufferedReader(new InputStreamReader(process.getErrorStream()));
	    String line = "";
	    String newLineCharacter = System.getProperty("line.separator");

	    boolean isOutReady = false;
	    boolean isErrorReady = false;
	    boolean isProcessAlive = false;

	    boolean isErrorOut = true;
	    boolean isErrorError = true;


	    logger.info("Read command ");
	    while (process.isAlive()) {
	        //Read the stdOut

	        do {
	            isOutReady = stdInput.ready();
	            //System.out.println("OUT READY " + isOutReady);
	            isErrorOut = true;
	            isErrorError = true;

	            if (isOutReady) {
	                line = stdInput.readLine();
	                isErrorOut = false;
	                logger.info("=====================================================================================" + line + newLineCharacter);
	            }
	            isErrorReady = stdError.ready();
	            //System.out.println("ERROR READY " + isErrorReady);
	            if (isErrorReady) {
	                line = stdError.readLine();
	                isErrorError = false;
	                logger.info("ERROR::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::" + line + newLineCharacter);

	            }
	            isProcessAlive = process.isAlive();
	            //System.out.println("Process Alive " + isProcessAlive);
	            if (!isProcessAlive) {
	                logger.info(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: Process DIE " + line + newLineCharacter);
	                line = null;
	                isErrorError = false;
	                process.waitFor(1000, TimeUnit.MILLISECONDS);
	            }

	        } while (line != null);

	        //Nothing else to read, lets pause for a bit before trying again
	        logger.info("PROCESS WAIT FOR");
	        process.waitFor(100, TimeUnit.MILLISECONDS);
	    }
	    logger.info("Command finished");
	}
	
	


}