package uk.ac.cam.cares.jps.wte.test;

import java.io.File;
import java.util.List;
import java.util.UUID;
import org.apache.jena.ontology.OntModel;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.wte.FCQuerySource;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

public class WastetoEnergyAgentTest extends TestCase{
	static String iriofnetwork=null;
	static String baseUrl = null;
	String usecaseID = null;
	
	@Before
	public void setUp() {
		iriofnetwork ="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
		baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\WTETest";
		usecaseID = UUID.randomUUID().toString();
	}
	
	/** test the FCQuery creation by 
	 * a. reading the model and getting the number of FC *number of years of waste levels 
	 * b. creating the csv file of site locations, and waste levels of those FoodCourts per year
	 * c. test presence of file there. 
	 * 
	 */
	@Test
	public void testPrepareCSVFC() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		OntModel model= FCQuerySource.readModelGreedy(iriofnetwork);
		int noOfYears = 15;
		a.prepareCSVFC("Site_xy.csv","Waste.csv", baseUrl,model, noOfYears);
		File file = new File( baseUrl + "\\Site_xy.csv");
		assertTrue(file.exists());
		File file2 = new File(baseUrl +"\\Waste.csv");
		assertTrue(file2.exists());
	}
	
	/** get Offsite WTF locations and save in csv file
	 * 
	 */
	@Test
	public void testQueryOffsiteWT() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		
		OntModel model= FCQuerySource.readModelGreedy(iriofnetwork);
		a.prepareCSVWT("Location.csv", baseUrl,model); 
		File file = new File(baseUrl +  "\\Location.csv");
		assertTrue(file.exists());
	}
	
	/** get Transport costs and save in a csv files
	 * should display four results, all non-null
	 */
	@Test
	public void testQueryTransportQuery() {
		WastetoEnergyAgent a= new WastetoEnergyAgent ();
		
		OntModel model= FCQuerySource.readModelGreedy(iriofnetwork);
		a.prepareCSVTransport(WastetoEnergyAgent.getTransportQuery(),"transport.csv", baseUrl,model); 

		File file = new File(baseUrl +  "\\transport.csv");
		assertTrue(file.exists());
		//TODO:check that file has no null value
	}
	
	/** Query transport data and check only one result is returned. 
	 * 
	 */
	@Test
	public void testQuerytransport() {
		OntModel model = FCQuerySource.readModelGreedy(iriofnetwork);
		String query=WastetoEnergyAgent.getTransportQuery();
		List<String[]> resultList =  FCQuerySource.queryResult(model, query);
        assertEquals(1, resultList.size());
	
	}
	
	
	/** Query types of technology of offsite. Currently three (anerobic, incineration, co-digestion)
	 * 
	 */
	@Test
	public void testQueryTechOffsiteQuery() {
	String query = FCQuerySource.getTechQuery() 
			.addWhere("?entity" ,"a", "j1:OffsiteWasteTreatmentFacility").buildString();
	OntModel model= FCQuerySource.readModelGreedy(iriofnetwork);
	List<String[]> resultList = FCQuerySource.queryResult(model,query);
    assertEquals(3, resultList.size());
	}
	
	/** Query types of technology of onsite. Currently only one. 
	 * 
	 */
	@Test
	public void testQueryTechOnsiteQuery() {
		String query = FCQuerySource.getTechQuery() 
			.addWhere("?entity" ,"a", "j1:OnsiteWasteTreatmentFacility")
			.addWhere("?Tech1" ,"a", "j1:OnSiteDigester").buildString();
		OntModel model= FCQuerySource.readModelGreedy(iriofnetwork);
		List<String[]> resultList = FCQuerySource.queryResult(model,query);
        assertEquals(1, resultList.size());
	}
	
	
	
	/** checks input to see if user input was valid for WasteToEnergyAgent
	 * WTEProcessResult should not have a test because it looks if the file is already created. 
	 * @throws JSONException
	 */
	@Test
	public void testInputValidatorWTEAgent() throws JSONException {
		
		JSONObject jo = new JSONObject().put("wastenetwork", iriofnetwork)
				.put("n_cluster", "jk");
		WastetoEnergyAgent j = new WastetoEnergyAgent();
		assertFalse(j.validateInput(jo));
		jo.put("n_cluster", "30");
		assertTrue(j.validateInput(jo));
		
	}
	
	
}
