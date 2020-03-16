package uk.ac.cam.cares.jps.wte.test;

import java.io.IOException;

import org.apache.jena.ontology.OntModel;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;
import uk.ac.cam.cares.jps.wte.visualization.WTEVisualization;

public class TestVisualization  extends TestCase {
	public String WasteTopNode = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	public void testFCQuery(){
		WTEVisualization a = new WTEVisualization();
		OntModel model = WastetoEnergyAgent.readModelGreedy(WasteTopNode);
		try {
			String g = a.createMarkers(model);
			System.out.println(g);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
