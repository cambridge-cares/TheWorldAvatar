package uk.ac.ceb.como.molhub.action;

import java.io.File;

import org.apache.jena.ontology.OntModel;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.cam.ceb.como.compchem.ontology.query.CompChemQuery;

import org.apache.commons.io.FileUtils;

public class CalculationAction extends ActionSupport  {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public String execute() throws Exception {
		
		String catalicaFolderPath = System.getProperty("catalina.home");
		
        File sparqlFile = new File(catalicaFolderPath +"/conf/Catalina/sparql_query/query_all.sparql");
        
        File[] aboxFiles =  CompChemQuery.getFileList(catalicaFolderPath+ "/conf/Catalina/compchem_ontology/");
        
        String sparqlResulFolder = catalicaFolderPath +"/conf/Catalina/sparql_results/";
        
        for(File af : aboxFiles) {
        	
            	OntModel model = CompChemQuery.getOntModel(af.getAbsolutePath());
            	
            	String q = FileUtils.readFileToString(sparqlFile, "UTF-8");
        		
            	CompChemQuery.performQuery(model, q, af.getName().toString(), sparqlResulFolder);
        }
        
        File[] jsonFiles =  CompChemQuery.getJSONFileList(catalicaFolderPath+ "/conf/Catalina/sparql_results/");
        
        for(File jsonFile : jsonFiles) {
        
        	String jsonF = FileUtils.readFileToString(jsonFile, "UTF-8");
        	
        	System.out.println(jsonFile.getAbsolutePath());
        	
//        	String[] cmd = {"python", "C:/Users/nk510/git/c4e-dln22-TDC/Source/thermoDriver.py", "-j", "C:/Users/nk510/git/c4e-dln22-TDC/Source/TiCl4.json", };
        	
        	String[] cmd = {"python", "C:/Users/nk510/git/c4e-dln22-TDC/Source/thermoDriver.py", "-j", jsonFile.getAbsolutePath(), };
        	
        	Runtime.getRuntime().exec(cmd);
        }
        
		return SUCCESS;
	
	}

}