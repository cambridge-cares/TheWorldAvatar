package uk.ac.cam.cares.jps.agents.test;

import java.io.FileNotFoundException;
import java.net.URISyntaxException;

import org.junit.Test;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.agents.ontology.ServiceBuilder;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
/**
 * 
 * @author NK510
 * Creates agent description for thermo calculation ans save it as abox of msm ontology.
 * 
 */
public class TestThermoAgentDescription extends TestCase {

	private static final String JPS_THERMO = "http://www.theworldavatar.com/JPS_THERMO";

	/**
	 * 
	 * @return Json object that contains input and output parameters for thermo calculation agent.
	 */
	
	private Service createAgentDescriptionForThermoCalculation() {

		return new ServiceBuilder().operation(null, JPS_THERMO + "/thermocalculation")
				.input("https://como.cheng.cam.ac.uk/kb/compchem.owl#G09", "g09")
				.output("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#Species", "species").build();
	}

	@Test
	public void testDescription() throws URISyntaxException, FileNotFoundException {

		Service service = createAgentDescriptionForThermoCalculation();

		String json = new Gson().toJson(service);
		System.out.println(json);
		
		/**
		 * Method generates owl file and stores in in given folder. The Owl file is instance of MSM ontology.
		 */
		TestAgentDescriptions.backAndforthAndWrite(service, "_thermocalculation");
		
	}

}