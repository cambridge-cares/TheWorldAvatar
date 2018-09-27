package uk.ac.ceb.como.molhub.action;

import com.opensymphony.xwork2.ActionSupport;
import com.opensymphony.xwork2.ValidationAware;

import uk.ac.cam.ceb.como.compchem.ontology.InconsistencyExplanation;
import uk.ac.cam.ceb.como.compchem.xslt.Transformation;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;

import uk.ac.cam.ceb.como.jaxb.xml.generation.GenerateXml;
import uk.ac.ceb.como.molhub.bean.GaussianUploadReport;
import uk.ac.ceb.como.molhub.model.FolderManager;
import uk.ac.ceb.como.molhub.model.XMLValidationManager;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.stream.StreamSource;

import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

import org.apache.log4j.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class UploadAction.
 *
 * @author nk510 The Class UploadAction: Uploads one or more selected Gaussian
 *         files (g09) on server, and generates XML, ontology file, image file,
 *         and adds ontology files into tripe store (RDF4J).
 */
public class UploadAction extends ActionSupport implements ValidationAware {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(UploadAction.class.getName());

	/** The upload file content type. */
	private String uploadFileContentType;

	/** The catalina folder path. */
	private String catalinaFolderPath = System.getProperty("catalina.home");

	/** The xslt. */
	private String xslt = catalinaFolderPath + "/conf/Catalina/xslt/ontochem_rdf.xsl";

	/** The xsd. */
	private String xsd = catalinaFolderPath + "/conf/Catalina/xml_schema/schema.xsd";

	/** The files. */
	private List<File> files = new ArrayList<File>();

	/** The upload file name. */
	private String[] uploadFileName;

	/**
	 * The column.
	 *
	 * @author nk510
	 * List of comun names in table that reports about uploading process of Gaussina
	 * file. Columns are named as (uuid, file name, XML validation, OWL
	 * consistency).
	 */
	List<String> column = new ArrayList<String>();

	/** The gaussian upload report. */
	GaussianUploadReport gaussianUploadReport;

	/** The upload report list. */
	private List<GaussianUploadReport> uploadReportList = new ArrayList<GaussianUploadReport>();

	/** The uri. */
	private String uri = "http://como.cheng.cam.ac.uk/molhub/compchem/";

	/** The rdf4j server url (localhost). */
	private String serverUrl = "http://localhost:8080/rdf4j-server/";

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 */
	@Override
	public String execute() throws Exception {

		int fileNumber = 0;
		
		/**
		 * @author nk510 Column names in generated table (report).
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
		 * @author nk510 Iterates over selected (uploaded) files.
		 * 
		 */

		for (File f : files) {

			Module rootModule = new Module();

			/**
			 * @author nk510 Creates unique folder name for each uploaded Gaussian file.
			 *         (g09), XML file, OWL file, and PNG file.
			 */

			String folderName = FolderManager.generateUniqueFolderName(f.getName(), catalinaFolderPath);

			File inputG09File = new File(folderName + "/" + uploadFileName[fileNumber]);

			File outputXMLFile = new File(
					folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "") + ".xml");

			String outputOwlPath = folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "").toString()
					+ ".owl";

			File owlFile = new File(outputOwlPath);
			
			/**
			 * @author nk510 Png file name is the same as the name of folder where that
			 *         image is saved.
			 */

			File pngFile = new File(folderName + "/" + folderName.substring(folderName.lastIndexOf("/") + 1) + ".png");

			/**
			 * @author nk510 Creates a folder.
			 */

			FolderManager.createFolder(folderName);

			/**
			 * @author nk510 Gaussian file and XML file are saved into generated
			 *         folder.
			 */
			FolderManager.saveFileInFolder(inputG09File, f.getAbsolutePath());

			GenerateXml.generateRootModule(inputG09File, outputXMLFile, rootModule);

			/**
			 * 
			 * @author nk510 Runs Xslt transformation. Here we use just created folder name
			 *         as a part of IRI in generated ontology (owl file).
			 * 
			 */

			Transformation.trasnformation(folderName.substring(folderName.lastIndexOf("/") + 1),
					new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile),
					new StreamSource(xslt));

			/**
			 * @author nk510 Generate image (.png file) from uploaded Gaussian file by using
			 *         JmolData.
			 */

			String[] cmd = { "java", "-jar", catalinaFolderPath + "/conf/Catalina/jmol/JmolData.jar", "--nodisplay",
					"-j", "background white", inputG09File.getAbsolutePath().toString(), "-w",
					"png:" + pngFile.getAbsolutePath().toString() };

			Runtime.getRuntime().exec(cmd);

			/**
			 * @author nk510 Validates of generated Compchem xml file against Compchem XML
			 *         schema, and checks consistency of generated Compchem ontology (owl
			 *         file).
			 * 
			 */

			boolean consistency = InconsistencyExplanation.getConsistencyOWLFile(outputOwlPath);

			boolean xmlValidation = XMLValidationManager.validateXMLSchema(xsd, outputXMLFile);

			gaussianUploadReport = new GaussianUploadReport(folderName.substring(folderName.lastIndexOf("/") + 1),
					uploadFileName[fileNumber], xmlValidation, consistency);

			uploadReportList.add(gaussianUploadReport);

			/**
			 * 
			 * @author nk510 Adding generated ontology files (owl) into RDF4J triple store,
			 *         only in case when generated ontology (owl file) is consistent.
			 * 
			 */

			if (consistency) {

				/**
				 * @author nk510 Gets the repository connection.
				 * @param serverUrl
				 *            remote sparql endpoint.
				 */

				Repository repository = new HTTPRepository(serverUrl);

				repository.initialize();

				RepositoryConnection connection = repository.getConnection();

				try {
					/**
					 * @author nk510 Begins a new transaction. Requires commit() or rollback() to be called to end the transaction.
					 */
					connection.begin();

					try {

						/**
						 * @author nk510 Each generated owl file will be stored in RDF4J triple store.
						 */
						connection.add(owlFile, uri, RDFFormat.RDFXML);

						connection.commit();

					} catch (RepositoryException e) {
						
						/**
						 * @author nk510
						 * If something is wrong during the transaction, it will return a message about
						 * it.
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

			/**
			 * 
			 * In case of inconsistency of generated ontology (Abox) then Error message will
			 * appear.
			 * 
			 */

			if (!consistency) {

				addFieldError("term.name", "Ontology '" + owlFile.getName()
						+ "' is not consistent. Owl file is not loaded into triple store.");
				return ERROR;
			}

			fileNumber++;
		}

		return INPUT;

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
	 * @param upload
	 *            the new upload
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
	 * @param uploadFileName
	 *            the new upload file name
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
	 * Gets the upload file content type.
	 *
	 * @return the upload file content type
	 */
	public String getUploadFileContentType() {
		return uploadFileContentType;
	}

	/**
	 * Sets the upload file content type.
	 *
	 * @param uploadFileContentType the new upload file content type
	 */
	public void setUploadFileContentType(String uploadFileContentType) {
		this.uploadFileContentType = uploadFileContentType;
	}

}