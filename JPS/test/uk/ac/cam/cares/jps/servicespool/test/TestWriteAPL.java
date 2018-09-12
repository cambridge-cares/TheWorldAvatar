package uk.ac.cam.cares.jps.servicespool.test;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.semantic.QueryWarehouse;

public class TestWriteAPL {

	@Test
	public void test() throws JSONException {
		
		
		JSONObject regionObject = new JSONObject("{\"ymax\":455190,\"xmax\":80000,\"ymin\":454670,\"xmin\":79480}");
		JSONObject buildingsObject = TestBuildingData.getBuildingData();
		String plantIRI = "http://www.theworldavatar.com/Plant-001.owl#Plant-001";
		String buildingInString = convertBuildingData(buildingsObject);
		
		//System.out.println(buildingInString);
		
 		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";

		System.out.println(fullPath);
		
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsTest.py"); 
	
  		args.add(buildingInString.replace("\"", "'"));
 		args.add(regionObject.toString().replace("\"", "'"));
 		args.add(plantIRI.replace("\"", "'"));
 		args.add(fullPath.replace("/", "\\"));
 		
 		
  		String result = CommandHelper.executeCommands(targetFolder, args);
 		System.out.println(result);
		 
		
	}
	
	
	public String convertBuildingData(JSONObject buildings) {


 		Model model = ModelFactory.createDefaultModel();
		try {
			RDFDataMgr.read(model, new ByteArrayInputStream(buildings.toString().getBytes("UTF-8")), Lang.RDFJSON);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		return 	QueryWarehouse.getBuildingData(model);

	}
	
	
	
	

}
