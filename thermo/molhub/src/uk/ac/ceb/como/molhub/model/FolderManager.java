package uk.ac.ceb.como.molhub.model;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import javax.xml.transform.stream.StreamSource;

import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.rio.RDFFormat;

import uk.ac.cam.ceb.como.jaxb.xml.generation.GenerateXml;
import uk.ac.ceb.como.molhub.bean.GaussianUploadReport;
import uk.ac.ceb.como.molhub.controler.ConnectionToTripleStore;
import uk.ac.cam.ceb.como.compchem.xslt.Transformation;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;

public class FolderManager {

	private List<GaussianUploadReport> uploadReportList = new ArrayList<GaussianUploadReport>();
	
	/**
	 * @author nk510
	 * @param fileName Name of Gaussian file
	 * @param catalinaFolderPath A path where folder will be created.
	 * @return A unique folder name.
	 * @throws UnsupportedEncodingException
	 */
	public static String generateUniqueFolderName(String fileName, String catalinaFolderPath)
			throws UnsupportedEncodingException {
		
		String folderName = "";

		long milliseconds = System.currentTimeMillis();

		String datetime = new Date().toString();

		datetime = datetime.replace(" ", "");
		datetime = datetime.replace(":", "");

		/**
		 * 
		 * @author nk510 Generates source for universally unique identifier (uuid) based
		 *         on file name, date, time, and milliseconds.
		 * 
		 */

		String source = fileName + datetime + milliseconds;

		byte[] bytes = source.getBytes("UTF-8");

		UUID uuid = UUID.nameUUIDFromBytes(bytes);

		folderName = catalinaFolderPath + "/conf/Catalina/" + uuid.toString();

		return folderName;

	}

	/**
	 * @author nk510
	 * @param folderName a folder's name to be created.
	 */
	public static void createFolder(String folderName) {

		Path folderPath = Paths.get(folderName);

		if (!Files.exists(folderPath)) {

			try {
				Files.createDirectories(folderPath);
			} catch (IOException e) {

				e.printStackTrace();
			}
		}

	}

	/**
	 * @author nk510
	 * @param inputFile Input file to be saved in created folder.
	 * @param absoluteFilePath a path to a folder where input file will be saved.
	 * @throws IOException
	 */
	public static void saveFileInFolder(File inputFile, String absoluteFilePath) throws IOException {

		Path path = Paths.get(absoluteFilePath);

		byte[] data = Files.readAllBytes(path);

		BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(inputFile));

		stream.write(data);

		stream.close();

	}

	/**
	 * @author nk510 Uploads selected Gaussian files. By parsing Gaussian file it
	 *         generates Compchem XML files, and by using XSLT transformations it
	 *         generates Comchem ABox ontologies for each Gaussian file. Generated
	 *         ontologies will be stored in Tomcat's server folders as well as in a
	 *         triple store.
	 * @param files
	 *            set of selected file names which will be uploaded.
	 * @param uploadFileName
	 *            a list of file names.
	 * @param fileNumber
	 *            a number of files to be uploaded.
	 * @param catalinaFolderPath
	 *            a folder path for Tomcat's Catalina folder.
	 * @param serverUrl
	 *            a triple store server's URL
	 * @param xslt
	 * @param uri
	 * @throws Exception
	 */
	public static void iteratesOverFolder(List<File> files, String[] uploadFileName, int fileNumber,
			String catalinaFolderPath, String serverUrl, String xslt, String uri) throws Exception {

		/**
		 * 
		 * @author nk510 Iterates over selected file names.
		 * 
		 */

		for (File f : files) {

			Module rootModule = new Module();

			/**
			 * @author nk510 Creates unique folder name for each uploaded Gaussian file
			 *         (g09).
			 */

			String folderName = FolderManager.generateUniqueFolderName(f.getName(), catalinaFolderPath);

			File inputG09File = new File(folderName + "/" + uploadFileName[fileNumber]); // uploadFileName[fileNumber]

			File outputXMLFile = new File(
					folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "") + ".xml");

			String outputOwlPath = folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "").toString()
					+ ".owl";

			File owlFile = new File(outputOwlPath);

			FolderManager.createFolder(folderName);

			FolderManager.saveFileInFolder(inputG09File, f.getAbsolutePath());

			GenerateXml.generateRootModule(inputG09File, outputXMLFile, rootModule);

			/**
			 * 
			 * @author nk510 Runs Xslt transformation.
			 * 
			 */

			Transformation.trasnformation(new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile),
					new StreamSource(xslt));
			/**
			 * 
			 * @author nk510 Adding generated ontology files (owl) into RDF4J triple store.
			 * 
			 */
			try (RepositoryConnection connection = ConnectionToTripleStore.getRepositoryConnection(serverUrl,
					"compchemkb")) {
				/**
				 * @author nk510 Beginning of transaction.
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
					 * If something is wrong during the transaction, it will return a message about
					 * it.
					 */
					connection.rollback();
				}
				connection.close();
			}

			fileNumber++;
		}
	}
	
	public List<GaussianUploadReport> getUploadReportList() {
		return uploadReportList;
	}

	public void setUploadReportList(List<GaussianUploadReport> uploadReportList) {
		this.uploadReportList = uploadReportList;
	}
	
}