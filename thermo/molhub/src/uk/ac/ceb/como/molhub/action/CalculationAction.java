package uk.ac.ceb.como.molhub.action;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.OntModel;
import org.apache.struts2.interceptor.SessionAware;
import org.apache.struts2.ServletActionContext;
import org.apache.struts2.dispatcher.SessionMap;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.cam.ceb.como.compchem.ontology.query.CompChemQuery;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.Utility;

import org.apache.commons.io.FileUtils;

import uk.ac.ceb.como.molhub.bean.MoleculeProperty;

import org.apache.log4j.Logger;
/**
 * 
 * @author nk510 
 * <p>Implement methods which runs sparql query on generated Abox
 *         (owl files) of Ontochem ontology. Runs thermo calculations
 *         implemented in Python, and generated results using json format.</p>
 */

public class CalculationAction extends ActionSupport implements SessionAware {

	final static Logger logger = Logger.getLogger(CalculationAction.class.getName());
	
	private static final long serialVersionUID = 1L;
	
	Map<String, Object> session;

	String catalinaFolderPath = System.getProperty("catalina.home");
	
	String sparql = catalinaFolderPath + "/conf/Catalina/sparql_query/query_all.sparql";
	
	List<MoleculeProperty> finalSearchResultSet = new ArrayList<MoleculeProperty>();	
	
	@Override
	public String execute() throws Exception {

		Utility utility = new FileUtility();

		File sparqlFile = new File(sparql);
		
		if(session.isEmpty()) {
			
			addFieldError("term.name","There are no selected species for which calculation will be performed.");
			
			return ERROR;
		}
		
		for(Map.Entry<String, Object> mp: session.entrySet()) {
			
			String speciesFolder = catalinaFolderPath + "/webapps/ROOT/"+ mp.getKey().toString()+ "/";
			
			List<File> aboxFiles = utility.getArrayFileList(speciesFolder, ".owl");
			
			for (File af : aboxFiles) {

				OntModel model = CompChemQuery.getOntModel(af.getAbsolutePath());

				String q = FileUtils.readFileToString(sparqlFile, "UTF-8");

				CompChemQuery.performQuery(model, q, af.getName().toString(), speciesFolder);
				
			}
				List<File> jsonFiles = utility.getArrayFileList(speciesFolder, ".json");				
				
				for(int i=0;i<jsonFiles.size();i++) {


					logger.info("jsonFile.getAbsolutePath(): " + jsonFiles.get(i).getAbsolutePath());
					
					/**
					 * @author nk510
					 * Runs Python script for thermodynamic calculations. 
					 */

					String[] cmd = { "python", "C:/Users/nk510/git/c4e-dln22-TDC/Source/thermoDriver.py", "-j",
							jsonFiles.get(i).getAbsolutePath(), };

					Runtime.getRuntime().exec(cmd);
				}
				
				/**
				 * @author nk510
				 * Remove session keys
				 */
				session.remove(mp.getKey());
				
		}
		
		
		addActionMessage("Calculations successfully completed.");
		
		return SUCCESS;

	}

	public List<MoleculeProperty> getFinalSearchResultSet() {
		return finalSearchResultSet;
	}

	public void setFinalSearchResultSet(List<MoleculeProperty> finalSearchResultSet) {
		this.finalSearchResultSet = finalSearchResultSet;
	}
	
	public Map<String, Object> getSession() {
		return session;
	}

	@Override
	public void setSession(Map<String, Object> session) {
		this.session = (SessionMap<String,Object>)session;
	}
}