package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.IOException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.powsys.nuclear.Nuclear;

public class TestNuclear extends TestCase{

	
	public void testPrepareCSVLoad() throws IOException {
		String irinetwork="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		Nuclear b=new Nuclear();
		
		String diroutput= b.prepareCSVLoad(irinetwork);
		assertEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/inputloadpoints.csv", diroutput);
	}
	
	public void testPrepareCSVLots() throws IOException {
		String lotiri="http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		Nuclear b=new Nuclear();
		
		String diroutput= b.prepareCSVLandlot(lotiri);
		assertEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/inputlandlots.csv", diroutput);
		
	}
	
	public void testreadcsv() throws IOException {
		String oldcsv="D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Landlots.csv";
		Nuclear b=new Nuclear();
		b.readCSV(oldcsv,"s");
	}
}
