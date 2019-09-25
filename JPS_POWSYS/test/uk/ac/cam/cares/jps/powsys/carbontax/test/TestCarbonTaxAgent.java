package uk.ac.cam.cares.jps.powsys.carbontax.test;

import java.io.IOException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.powsys.carbontax.CarbonTaxAgent;

public class TestCarbonTaxAgent extends TestCase {
	
	public void testCSVReactorParameter() throws IOException, InterruptedException { //warning, need to put owl file in root localhost
		
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String dataPath = QueryBroker.getLocalDataPath();
		System.out.println("what is dataPath="+dataPath);
		
		CarbonTaxAgent a= new CarbonTaxAgent();
		a.prepareCSVGeneratorParameter(iriofnetwork,dataPath);
		a.runGAMS(dataPath);
		
	}

}
