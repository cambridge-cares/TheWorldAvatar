package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.IOException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;

public class TestNuclear extends TestCase{

	
	public void testPrepareCSVLoad() throws IOException {
		String irinetwork="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		NuclearAgent b=new NuclearAgent();
		
		String diroutput= b.prepareCSVLoad(irinetwork);
		assertEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/inputloadpoints.csv", diroutput);
	}
	
	public void testPrepareCSVLots() throws IOException {
		String lotiri="http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		NuclearAgent b=new NuclearAgent();
		
		String diroutput= b.prepareCSVLandlot(lotiri);
		assertEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/inputlandlots.csv", diroutput);
		
	}
	
	public void testreadcsv() throws IOException {
		String oldcsv="D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Landlots.csv";
		NuclearAgent b=new NuclearAgent();
		b.readCSV(oldcsv,"s");
	}
	
	public void testrunGAMS() {
		NuclearAgent b=new NuclearAgent();
		b.runGAMS();
	}
}
