package uk.ac.ceb.como.molhub.action;

import java.io.File;

import org.apache.jena.ontology.OntModel;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.cam.ceb.como.compchem.ontology.query.CompChemQuery;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.Utility;

import org.apache.commons.io.FileUtils;

/**
 * 
 * @author nk510 
 * <p>Implement methods which runs sparql query on generated Abox
 *         (owl files) of Ontochem ontology. Runs thermo calculations
 *         implemented in Python, and generated results using json format.</p>
 */

public class CalculationAction extends ActionSupport {

	private static final long serialVersionUID = 1L;

	String catalicaFolderPath = System.getProperty("catalina.home");
	
	String sparql = catalicaFolderPath + "/conf/Catalina/sparql_query/query_all.sparql";

	@Override
	public String execute() throws Exception {

		Utility utility = new FileUtility();

		File sparqlFile = new File(sparql);

		/**
		 * 
		 * Gets the ontology file list.
		 *
		 * @author nk510
		 * @param aboxFiles
		 *            the list of owl or rdf files. These files are Abox for Compchem ontology.
		 * @return
		 *         <p>
		 * 		Method reads all files in given folder path. Supported file
		 *         extensions is '.owl' or ".rdf".
		 *         </p>
		 * 
		 */
		
		//TODO: SPARQL on triple store. Update XSLT: CUID, Output file name, inChiKey, search based on InChiKey and file name.
		
		File[] aboxFiles = utility.getFileList(catalicaFolderPath + "/conf/Catalina/compchem_ontology/", ".owl", ".rdf");

		String sparqlResultFolder = catalicaFolderPath + "/conf/Catalina/sparql_results/";

		for (File af : aboxFiles) {

			OntModel model = CompChemQuery.getOntModel(af.getAbsolutePath());

			String q = FileUtils.readFileToString(sparqlFile, "UTF-8");

			CompChemQuery.performQuery(model, q, af.getName().toString(), sparqlResultFolder);
		}

		/**
		 * Gets the json file list.
		 *
		 * @author nk510
		 * @param jsonFiles
		 *            the list of json files.
		 * @return
		 *         <p>
		 * 		Method reads all json files in given folder path. Supported file
		 *         extension is '.json'.
		 *         </p>
		 */

		File[] jsonFiles = utility.getFileList(catalicaFolderPath + "/conf/Catalina/sparql_results/", ".json", null,
				null);

		for (File jsonFile : jsonFiles) {

			System.out.println(jsonFile.getAbsolutePath());
			
			/**
			 * @author nk510
			 * Runs Python script for thermodynamic calculations. 
			 */

			String[] cmd = { "python", "C:/Users/nk510/git/c4e-dln22-TDC/Source/thermoDriver.py", "-j",
					jsonFile.getAbsolutePath(), };

			Runtime.getRuntime().exec(cmd);
		}

		return SUCCESS;

	}
}