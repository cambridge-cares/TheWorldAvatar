package uk.ac.ceb.como.molhub.action;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.xml.transform.stream.StreamSource;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

import com.opensymphony.xwork2.ActionSupport;
import com.opensymphony.xwork2.ValidationAware;

import uk.ac.cam.ceb.como.compchem.ontology.InconsistencyExplanation;
import uk.ac.cam.ceb.como.compchem.xslt.Transformation;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.jaxb.xml.generation.GenerateXml;
import uk.ac.ceb.como.molhub.bean.GaussianUploadReport;
import uk.ac.ceb.como.molhub.model.ExecutorManager;
import uk.ac.ceb.como.molhub.model.FolderManager;
import uk.ac.ceb.como.molhub.model.PropertiesManager;
import uk.ac.ceb.como.molhub.model.XMLValidationManager;

/**
 * The Class UploadAction.
 *
 * @author nk510
 *         <p>
 *         The Class UploadAction: Uploads one or more selected Gaussian files
 *         (g09) on server, and generates XML, ontology file, image file, and
 *         adds ontology files into tripe store (RDF4J).
 *         </p>
 */

public class UploadAction extends ActionSupport implements ValidationAware {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(UploadAction.class.getName());

	/**
	 * @author NK510 Adds molhub properties such as: Folder path where g09, xml and
	 *         png files are stored. Folder path where generated ontology is stored.
	 *         Xslt file path. Xsd file path. Jmol data file path, that is used to
	 *         generated png file.
	 */
	Properties molhubPropreties = PropertiesManager.loadProperties(UploadAction.class.getClassLoader().getResourceAsStream("molhub.management.properties"));

	private String dataFolderPath = molhubPropreties.getProperty("data.folder.path").toString();

	private String kbFolderPath = molhubPropreties.getProperty("kb.folder.path").toString();

	private String xslt = molhubPropreties.getProperty("xslt.file.path").toString();

	private String xsd = molhubPropreties.getProperty("xsd.file.path").toString();

	private String jmolDataJarFilePath = molhubPropreties.getProperty("jmol.data.jar.file.path").toString();

	/**
	 * 
	 * @author NK510 Adds kb properties such as: OntoCompChem URI, RDF4J server URL.
	 * 
	 */
	Properties kbProperties = PropertiesManager.loadProperties(UploadAction.class.getClassLoader().getResourceAsStream("kb.ontocompchem.management.properties"));

	private String ontoCompChemUri = kbProperties.getProperty("ontocompchem.kb.tbox.uri").toString();

	private String serverUrl = kbProperties.getProperty("ontocompchem.kb.local.rdf4j.server.url").toString();

	/** The files. */
	private List<File> files = new ArrayList<File>();

	/** The upload file name. */
	private String[] uploadFileName;

	/** The upload content type. */
	private String[] uploadContentType;

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
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 */
	@Override
	public String execute() throws Exception {

		int fileNumber = 0;

		/**
		 * @author nk510
		 *         <p>
		 *         Column names in generated table (report) appearing after completing
		 *         upload action.
		 *         </p>
		 */

		if (!files.isEmpty()) {

			column.add("UUID");
			column.add("Gaussian file");
			column.add("XML validation");
			column.add("OWL consistency");
		}

		if (files.isEmpty()) {

			addActionMessage("Please select Gaussian files first, and than press 'Upload' button.");

		}

		/**
		 * 
		 * @author nk510
		 *         <p>
		 *         Iterates over selected (uploaded) files.
		 *         </p>
		 * 
		 */

		for (File f : files) {

			Module rootModule = new Module();

			/**
			 * @author nk510
			 *         <p>
			 *         Creates unique folder name for each uploaded Gaussian file. (g09),
			 *         XML file, OWL file, and PNG file.
			 *         </p>
			 */
			
			String uuidFolderName = FolderManager.generateUniqueFolderName(f.getName());

//			File inputG09File = new File(dataFolderPath + "/" + uuidFolderName + "/" + uploadFileName[fileNumber].replaceAll(".log", "") + ".g09");

			File inputG09File = new File(dataFolderPath + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".g09");
			
			/**
			 * 
			 * @author NK510
			 *         <p>
			 *         Get file extension.
			 *         </p>
			 * 
			 */
			
//			File outputXMLFile = new File(dataFolderPath + "/" + uuidFolderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "") + ".xml");
			
			File outputXMLFile = new File(dataFolderPath + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".xml");

			String outputOwlFile = kbFolderPath + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".owl";

			final File owlFile = new File(outputOwlFile);

			/**
			 * 
			 * @author nk510
			 * 
			 *         <p>
			 *         Png file name is the same as the name of folder where that image is
			 *         saved.
			 *         </p>
			 *
			 */

			File pngFile = new File(dataFolderPath + "/" + uuidFolderName + "/"
					+ uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".png");

			/**
			 * @author nk510 Creates folders where molhub stores ontologies (owl file), G09,
			 *         xml, and png files.
			 */

			FolderManager.createFolder(dataFolderPath + "/" + uuidFolderName);

			
			FolderManager.createFolder(kbFolderPath + "/" + uuidFolderName);

			/**
			 * @author nk510
			 *         <p>
			 *         Gaussian file and XML file are saved into generated folder.
			 *         </p>
			 */
			FolderManager.saveFileInFolder(inputG09File, f.getAbsolutePath());

			GenerateXml.generateRootModule(inputG09File, outputXMLFile, rootModule);

			boolean xmlValidation = XMLValidationManager.validateXMLSchema(xsd, outputXMLFile);

			/**
			 * @author nk510
			 *         <p>
			 *         If generated XML file is valid against Compchem XML Schema then run
			 *         XSLT transformations from XML file to OWL file.
			 *         </p>
			 */
			if (xmlValidation) {
				/**
				 * 
				 * @author nk510
				 *         <p>
				 *         Runs Xslt transformation. Here we use just created folder name as a
				 *         part of IRI in generated ontology (owl file).
				 *         </p>
				 * 
				 */

				Transformation.trasnformation(uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1),
						new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile),
						new StreamSource(xslt));

				/**
				 * @author nk510
				 *         <p>
				 *         Generates image (.png file) from uploaded Gaussian file by using
				 *         JmolData.jar.
				 *         </p>
				 */

//				String[] cmd = { "java", "-jar", catalinaFolderPath + "/conf/Catalina/jmol/JmolData.jar", "--nodisplay", 
				String[] cmd = { "java", "-jar", jmolDataJarFilePath, "--nodisplay", "-j", "background white",
						inputG09File.getAbsolutePath().toString(), "-w",
						"png:" + pngFile.getAbsolutePath().toString() };

				Runtime.getRuntime().exec(cmd);

			}

			/**
			 * @author nk510
			 *         <p>
			 *         Validates of generated Compchem xml file against Compchem XML schema,
			 *         and checks consistency of generated Compchem ontology (owl file).
			 *         </p>
			 * 
			 */

			boolean consistency = InconsistencyExplanation.getConsistencyOWLFile(outputOwlFile);

			gaussianUploadReport = new GaussianUploadReport(
					uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1), uploadFileName[fileNumber],
					xmlValidation, consistency);

			uploadReportList.add(gaussianUploadReport);

			/**
			 * 
			 * @author nk510
			 *         <p>
			 *         Adding generated ontology files (owl) into RDF4J triple store, only
			 *         in case when generated ontology (owl file) is consistent.
			 *         </p>
			 * 
			 */

			if (consistency) {

				ExecutorService executor = Executors.newSingleThreadExecutor();

				Thread threadTask = new Thread(new Runnable() {

					@Override
					public void run() {

						/**
						 * @author nk510 Gets the repository connection.
						 * @param serverUrl remote molhub sparql endpoint.
						 * 
						 */

						Repository repository = new HTTPRepository(serverUrl);

						repository.initialize();

						RepositoryConnection connection = repository.getConnection();

						try {

							/**
							 * @author nk510
							 *         <p>
							 *         Begins a new transaction. Requires commit() or rollback() to be
							 *         called to end of the transaction.
							 *         </p>
							 */

							connection.begin();

							try {

								/**
								 * @author nk510
								 *         <p>
								 *         Each generated owl file will be stored in RDF4J triple store.
								 *         </p>
								 */
								
								connection.add(owlFile, ontoCompChemUri, RDFFormat.RDFXML);
								
								connection.commit();

							} catch (RepositoryException e) {

								/**
								 * @author nk510
								 *         <p>
								 *         If something is wrong during the transaction, it will return a
								 *         message about it.
								 *         </p>
								 * 
								 */

								logger.info("RepositoryException: " + e.getMessage());

								connection.rollback();
							}

						} catch (Exception e) {

							logger.info(e.getStackTrace());

						}

						connection.close();

						repository.shutDown();
					}

				});

				executor.submit(threadTask);

				ExecutorManager em = new ExecutorManager();

				em.shutdownExecutorService(executor);

			}

			/**
			 *
			 * @author nk510
			 *         <p>
			 *         In case of inconsistency of generated ontology (Abox) Error message
			 *         will appear.
			 *         </p>
			 *
			 */

			if (!consistency) {

				addFieldError("term.name", "Ontology '" + owlFile.getName()
						+ "' is not consistent. Owl file is not loaded into triple store.");

				return ERROR;

			}

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

}