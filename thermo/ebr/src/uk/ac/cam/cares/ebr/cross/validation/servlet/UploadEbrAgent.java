package uk.ac.cam.cares.ebr.cross.validation.servlet;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.transform.stream.StreamSource;

import org.apache.log4j.BasicConfigurator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.ebr.constant.Constants;

import uk.ac.cam.cares.ebr.manager.FolderManager;
import uk.ac.cam.cares.ebr.manager.JsonManager;
import uk.ac.cam.cares.ebr.manager.RepositoryManager;
import uk.ac.cam.ceb.como.compchem.ontology.InconsistencyExplanation;

/**
 * should be used: import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
 */

import uk.ac.cam.ceb.como.compchem.xslt.Transformation;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.Utility;
import uk.ac.cam.ceb.como.jaxb.xml.generation.GenerateXml;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * The service parses Gaussian files, generates XML and OWL files, uploads OWL file on RDF4J repository. Additionally, checks the consistency of generated owl files (ontocompchem abox).
 *
 */
@WebServlet("/convert")
public class UploadEbrAgent extends HttpServlet{
	
	private static final long serialVersionUID = 1L;
	
	Logger logger = LoggerFactory.getLogger(UploadEbrAgent.class);

	public Utility utility = new FileUtility(); 
	
	/**
	 * 
	 * @author NK510 Adds ebr upload properties such as: Folder path where g09, xml and
	 *          are stored. Folder path where generated ontology is stored.
	 *         Xslt file path. Xsd file path.
	 *
	 *Comment: Works without AgentCaller that is not in the line of JPS architecture.
	 * 
	 */
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		response.setContentType("text/html;charset=UTF-8");

		BasicConfigurator.configure();

		/**
		 * 
		 * Code does not use AgentCaller implemented in JSP BASE LIB.
		 * Example http request:
		 * https://localhost:8080/ebragent/convert?input={"referenceSpecies": "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\login-skylake.hpc.cam.ac.uk_2207410956489400"}
		 * https://localhost:8080/ebragent/convert?input={"referenceSpecies": "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\login-skylake.hpc.cam.ac.uk_2207410956489400", "uniqueSpeciesIRI": "http://www.theworldavatar.com/kb/ontospecies/00b537ef-8b6f-3246-9a7e-edd0259c6e09.owl#00b537ef-8b6f-3246-9a7e-edd0259c6e09"}
		 * http://localhost:8080/ebragent/convert?input=%7B"%7B"referenceSpecies"%3A%20"C%3A%5C%5CUsers%5C%5CNK%5C%5CDocuments%5C%5Cphilipp%5C%5C180-pb556%5C%5Cg09"%7D"%7D
		 * 
		 */
		PrintWriter printerWriter = response.getWriter();
		
		String[] inputs = request.getParameterValues("input");
		
		printerWriter.println("json content (input parameter): " + inputs[0] + "<br>");
		
		String referenceSpecieFolderPath = JsonManager.getReferenceSpeciesFolderPath(inputs[0]);
		
		String speciesIRI = JsonManager.getSpeciesIRI(inputs[0]);
		
		printerWriter.println("reference species folder path: " + referenceSpecieFolderPath+ "<br>");
		printerWriter.println("unique species IRI: " + speciesIRI + "<br>");

		/**
		 * 
		 * @author NK510 (cresssd@hermes.cam.ac.uk)
		 * 
		 * Does not parse properly complex json content that contains more than one field.
		 * 
		 */
//		JSONObject jsonObject = new JSONObject(inputs[0]); 
		
		File[] fileList = utility.getFileList(referenceSpecieFolderPath,".g09",".log");
		
		for(File file : fileList) {
			
			boolean consistency = false;
			
			Module rootModule = new Module();
			
			String uuidFolderName = FolderManager.generateUniqueFolderName(file.getName());
			
			File outputXMLFile = new File(Constants.DATA_FOLDER_PATH_LOCAL_HOST + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".xml"); 
			
			File owlFile = new File(Constants.KB_FOLDER_PATH_LOCAL_HOST + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".owl");
			
			FolderManager.createFolder(Constants.DATA_FOLDER_PATH_LOCAL_HOST + "/" + uuidFolderName);
			
			FolderManager.createFolder(Constants.KB_FOLDER_PATH_LOCAL_HOST + "/" + uuidFolderName);
			
			try {
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * 
				 * Generates XML file. Does not validate generated XML.
				 * 
				 */
				GenerateXml.generateRootModule(file, outputXMLFile, rootModule);
				
				/**
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * 
				 * Generates owl file without unique species IRI. 
				 */
//				Transformation.trasnformation(uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1), new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile), new StreamSource(Constants.XSLT_FILE_PATH_LOCAL_HOST.toString()));
				
				
				/**
				 * @author NK510 (caressd@hermes.cam.ac.uk)
				 * 
				 * Generates owl file including unique species IRI
				 */				
				Transformation.transfromation(uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1), speciesIRI, new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile), new StreamSource(Constants.XSLT_FILE_PATH_LOCAL_HOST.toString()));
				
				consistency = InconsistencyExplanation.getConsistencyOWLFile(owlFile.getCanonicalPath());
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * 
				 * If owl file is consistent then it is uploaded on RDF4J server.
				 */
				if(consistency) {
					
					RepositoryManager.uploadOwlFileOnRDF4JRepository(owlFile, Constants.ONTOCOMPCHEM_KB_LOCAL_RDF4J_SERVER_URL_CLAUDIUS.toString(), Constants.ONTOCOMPCHEM_KB_TBOX_URI.toString());
				}

				
			} catch (Exception e) {
			
				e.printStackTrace();
			}
			
			printerWriter.println("[ xml file path:" + outputXMLFile.getCanonicalPath()+ "] [owl file path: " + owlFile.getCanonicalPath() + " ]" + " [consistency:   " + consistency +" ]" + "<br>");
			
		}
		
		printerWriter.close();

		
	}
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)	throws ServletException, IOException {
		
		super.doPost(request, response);

	}
	
}