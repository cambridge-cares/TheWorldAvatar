package uk.ac.cam.cares.ebr.cross.validation.servlet;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Properties;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.transform.stream.StreamSource;

import org.apache.log4j.BasicConfigurator;
import org.json.JSONObject;

import com.google.common.io.Files;

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

@WebServlet("/convert/single")
public class OntoCompChemUploadSingle extends HttpServlet{	

		private static final long serialVersionUID = 1L;
		
		public Utility utility = new FileUtility();	
		
		JSONObject inputJson =null;
		/**
		 * 
		 * @author NK510 Adds ebr upload properties such as: Folder path where g09, xml and
		 *          are stored. Folder path where generated ontology is stored.         
		 *
		 *Comment: Works without AgentCaller that is not in the line of JPS architecture.
		 * 
		 */
		
		@Override
		protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			
			response.setContentType("text/html;charset=UTF-8");

			BasicConfigurator.configure();
			
			InputStream input =  getServletContext().getResourceAsStream("/WEB-INF/ontocompchemupload.properties");			
			
			Properties properties = new Properties();
			
			properties.load(input);
			
			/**
			 * 
			 * Code does not use AgentCaller implemented in JSP BASE LIB.
			 * Example http request:
			 * https://localhost:8080/ontocompchemupload/convert?input={"referenceSpecies": "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\login-skylake.hpc.cam.ac.uk_2207410956489400\\530-1-2.g09", "uniqueSpeciesIRI": "http://www.theworldavatar.com/kb/ontospecies/00b537ef-8b6f-3246-9a7e-edd0259c6e09.owl#00b537ef-8b6f-3246-9a7e-edd0259c6e09"}
			 * http://localhost:8080/ebragent/convert?input=%7B"%7B"referenceSpecies"%3A%20"C%3A%5C%5CUsers%5C%5CNK%5C%5CDocuments%5C%5Cphilipp%5C%5C180-pb556%5C%5Cg09"%7D"%7D
			 * 
			 */
			PrintWriter printerWriter = response.getWriter();
			
//			printerWriter.println("ontocompchem.ns:  " + properties.getProperty("ontocompchem.ns")+ "<br>");
			
			String[] inputs = request.getParameterValues("input");
			
			System.out.println("json content (input parameter): " + inputs[0] + "<br>");
			
			JSONObject inputJson = new JSONObject(inputs[0]);
			
			String referenceSpecieFilePath = JsonManager.getReferenceSpeciesFolderPath(inputs[0]);
			
			File gaussianFile = new File(referenceSpecieFilePath);
			
			
			if(!gaussianFile.exists()) {
				
				throw new IOException("Gaussian file does not exist!");
			
			}
			
			/**
			 * 
			 * @author NK510 (caresssd@hermes.cam.ac.uk)
			 * if 'uniqueSpeciesIRI' exists that store it as instance of speciesIRI String object.
			 *  
			 */
			String	speciesIRI="";
			
			if(inputJson.has("uniqueSpeciesIRI")) {
			
				speciesIRI = JsonManager.getSpeciesIRI(inputs[0]);
					
					System.out.println("unique species IRI: " + speciesIRI + "<br>");
					
			}else {
				
				System.out.println("unique species IRI is empty  : " + speciesIRI + "<br>");
				
			}
			
			System.out.println("reference species file path: " + referenceSpecieFilePath+ "<br>");
			
			/**
			 * 
			 * @author NK510 (cresssd@hermes.cam.ac.uk)
			 * 
			 * Does not parse properly complex json content that contains more than one field.
			 * 
			 */
			
			File[] fileList = {gaussianFile};
			
			for(File file : fileList) {
				
				boolean consistency = false;
				
				Module rootModule = new Module();
				
				String uuidFolderName = FolderManager.generateUniqueFolderName(file.getName());
				
//				Constants.DATA_FOLDER_PATH_LOCAL_HOST
				FolderManager.createFolder(properties.getProperty("data.folder.path.local.host") + "/" + uuidFolderName);
				
//				Constants.KB_FOLDER_PATH_LOCAL_HOST
				FolderManager.createFolder(properties.getProperty("kb.folder.path.local.host") + "/" + uuidFolderName);
				
//				Constants.KB_FOLDER_PATH_LOCAL_HOST
				File owlFile = new File(properties.getProperty("kb.folder.path.local.host") + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".owl");
				
				
//				Constants.DATA_FOLDER_PATH_LOCAL_HOST				
				File outputXMLFile = new File(properties.getProperty("data.folder.path.local.host") + "/" + uuidFolderName + "/" + uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".xml");
				
				String gaussinaFileExtension = Files.getFileExtension(file.getAbsolutePath());
				
				File outputGaussianFile = new File(properties.getProperty("data.folder.path.local.host") + "/" + uuidFolderName + "/" + uuidFolderName + "." + gaussinaFileExtension);
				
				FolderManager.saveFileInFolder(outputGaussianFile, file.getAbsolutePath());

				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * 
				 * Save Gaussian file to target folder path
				 * 
				 */
				System.out.println("outputGaussianFile.getAbsolutePath(): " +outputGaussianFile.getAbsolutePath());
				
				FolderManager.copyFileToDestinationFolder(file, outputGaussianFile);			

				
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
					 * 
					 * Previous version of the code creates wiht StreamSource.
					 * 
					 */
					String xsltFilePath =getClass().getClassLoader().getResource("gxmltoowl.xsl").getPath();
					
					System.out.println("xsltFilePath: " + xsltFilePath);
					
					/**
					 * 
					 * @author NK510 (caresssd@hermes.cam.ac.uk)
					 * 
					 * Generates owl file without unique species IRI.
					 * 
					 */				
					if((speciesIRI == null) || (speciesIRI.length() == 0)) {
						
					Transformation.trasnformation(uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1), new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile), new StreamSource(xsltFilePath));
						
					}else {
						
						Transformation.transfromation(uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1), speciesIRI, new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile), new StreamSource(xsltFilePath));
						
					}
					
					
					/**
					 * 
					 * @author NK510 (caressd@hermes.cam.ac.uk)
					 * 
					 * Generates owl file including unique species IRI
					 * 
					 */
//					Constants.XSLT_FILE_PATH_LOCAL_HOST.toString()
//					properties.getProperty("xslt.file.path.local.host"))
					
					consistency = InconsistencyExplanation.getConsistencyOWLFile(owlFile.getCanonicalPath());
					
					/**
					 * 
					 * @author NK510 (caresssd@hermes.cam.ac.uk)
					 * 
					 * If owl file is consistent then it is uploaded on RDF4J server.
					 * 
					 */
					
					if(consistency) {
					
//					Constants.ONTOCOMPCHEM_KB_LOCAL_RDF4J_SERVER_URL_LOCAL_HOST.toString()
//					Constants.ONTOCOMPCHEM_KB_TBOX_URI.toString()
						
					RepositoryManager.uploadOwlFileOnRDF4JRepository(owlFile,properties.getProperty("ontocompchem.kb.local.rdf4j.server.url.local.host").toString(),properties.getProperty("ontocompchem.kb.tbox.uri").toString(), properties.getProperty("ontocompchem.ns").toString());
						
//					RepositoryManager.loadOntology("http://localhost/rdf4j-server/", uuidFolderName.substring(uuidFolderName.lastIndexOf("/") + 1) + ".owl", owlFile.getAbsolutePath(), "ontocompchem");
					
					}
					
				}catch (Exception e){
				
				e.printStackTrace();
					
				}
				
			printerWriter.println("[ xml file path:" + outputXMLFile.getCanonicalPath()+ "] [owl file path: " + owlFile.getCanonicalPath() + " ]" + " [consistency:   " + consistency +" ]" + "<br>");

			}
			
printerWriter.close();
			
	}

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		// TODO Auto-generated method stub
		super.doPost(req, resp);
	}

}
