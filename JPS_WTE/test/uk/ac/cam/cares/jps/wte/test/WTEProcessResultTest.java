package uk.ac.cam.cares.jps.wte.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.OntModel;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.wte.FCQuerySource;
import uk.ac.cam.cares.jps.wte.WTEProcessResult;

/** WTEProcessResultTest does not have many tests as it is mostly reliant on updates (which rely on a library)
 * and WTEKBCreator
 * The complete test, like WasteToEnergyAgent, can be found in WTECoordinationTest as we cannot call this agent
 * without a pre-run function 
 *
 */
public class WTEProcessResultTest {
	static String iriofnetwork=null;
	static String baseUrl = null;
	String usecaseID = null;
	@Before
	public void setUp() {
		iriofnetwork ="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
		baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\WTETest";
		usecaseID = UUID.randomUUID().toString();
	}
	
	/** test validateInput method of WTEProcessResult
	 * 
	 * @throws JSONException
	 */
	@Test
	public void testInputValidatorWTEAgent() throws JSONException {
		
		JSONObject jo = new JSONObject().put("baseUrl", baseUrl );
		assertFalse(new WTEProcessResult().validateInput(jo));
		
		
	}

	/** Query Output data. In base scenario, it should be only one
	 * 
	 */
	@Test
	public void testWTEWasteSystemOutputQuery() {
		OntModel model = FCQuerySource.readModelGreedy(iriofnetwork);		
		String query = WTEProcessResult.getWasteSystemOutputQuery();
		List<String[]> resultList =  FCQuerySource.queryResult(model, query);
		System.out.println("size of result="+resultList.size()); 
        assertEquals(1, resultList.size());
        
	}
}
