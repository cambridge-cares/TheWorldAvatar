package uk.ac.cares.jps.thermo.test;
import java.io.IOException;
import java.util.ArrayList;

import org.json.JSONObject;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.controller.OntoKin;
import com.cmclinnovations.ontochem.controller.OntoKinFactory;
import com.cmclinnovations.ontochem.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.exception.OntoException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;

public class TestThermo extends TestCase {
	
	 private ApplicationContext applicationContext;
	 private AppConfigOperationControl opCtrl;

	 
	
	public void testReadFile() {
		
		String content = new QueryBroker().readFile("http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");
		
		System.out.println(content);
	}
	
	@Test
	public void testThermoCalculationAgent() { 
		
		JSONObject json = new JSONObject();
		
		json.put("gaussian", "http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");
		
		// GET ...twa.com/JPS_THERMO/thermocalcualtion?query={"gaussian":".......owl"}
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_THERMO/calculation", json.toString());
		
		System.out.println("result = " + result);
		
	}
	
	@Test
	public void convertJsonIntoOwl() throws IOException {
		
		/**
		 * @author NK
		 * This source code (method) is implemented by Dr M.S.F. Farazi <msff2@cam.ac.uk>.
		 */
		
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (opCtrl == null) {
			opCtrl = applicationContext.getBean(AppConfigOperationControl.class);
		}
		
		ArrayList<String> sourceFiles = new ArrayList<String>();

		sourceFiles.add("C:\\apache-tomcat-8.5.35\\webapps\\ROOT\\kb\\fba9f419-c117-348a-862f-f377f58a3b07\\fba9f419-c117-348a-862f-f377f58a3b07_updated_nasa.json");
		
		String owlFileDir = "C:\\apache-tomcat-8.5.35\\webapps\\ROOT\\kb\\fba9f419-c117-348a-862f-f377f58a3b07";

		if (sourceFiles != null) {
			OntoKin ontoKin = OntoKinFactory.getOntoKin(opCtrl.getOpReadCompChem(), opCtrl.getOpWriteOwl(), sourceFiles, owlFileDir);
			try {
				ontoKin.convert();
			} catch (OWLOntologyCreationException | OntoException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	   }	
}