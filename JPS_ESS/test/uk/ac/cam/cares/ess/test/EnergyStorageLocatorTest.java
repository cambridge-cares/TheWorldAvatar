package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.util.List;

import org.apache.jena.ontology.OntModel;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.ess.EnergyStorageLocator;

public class EnergyStorageLocatorTest extends TestCase {
	
	
	public static String ENIRI = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	OntModel model = EnergyStorageLocator.readModelGreedy(ENIRI);
	
	public void testprepareSelectedBranch() {
		
		List<String[]> SelectedBranch=new EnergyStorageLocator().prepareSelectedBranch(model,0.3);
		System.out.println("size= "+SelectedBranch.size()); //assume size=7
			
	}
	
	public void testcreateOwl() throws IOException {
		
		String resultofbattery="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		new EnergyStorageLocator().createBatteryOwlFile(model,resultofbattery,0.3);
		
	}

}
