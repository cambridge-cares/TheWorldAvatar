package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;

public class BlockchainWrapperTest {


	/** checks for empty input using validateInput() for BlockchainWrapper Agent
	 * 
	 */
	@Test
	public void testInputValidatorBlockchainWrapper(){
		JSONObject jo = new JSONObject();
	    assertFalse(new BlockchainWrapper().validateInput(jo));	
	    jo.put("key", "value");
	    assertTrue(new BlockchainWrapper().validateInput(jo));	
	}
	
    
    /** test loadProperties in BlockchainWrapper
     * 
     */
    @SuppressWarnings("static-access")
	@Test
    public void testgetPropertiesFromBlockchainWrapper() {
    	BlockchainWrapper ab = new BlockchainWrapper();
    	assertNotNull(ab.addrOfI, "industrial.json");
    }
    
	/**
	 * Calls and runs the Blockchain transaction directly
	 */
    @Test
	public void testBlockchainWrappercalculateTrade() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("industrial", "2.311116263469459966e+01");
		jo.put("commercial", "5.000000000000000000e+01");
		jo.put("residential","8.826121920185781278e+00");
		jo.put("gridsupply","4.409266691007480290e+01");
		jo.put("solar","3.784461764480557235e+01");
		JSONObject v = new BlockchainWrapper().calculateTrade(jo);
		assertNotNull(v.get("txHash"));
		assertNotNull(v.get("sandr"));
	}

	/**
	 * Calls and runs the Blockchain transaction using Agent
	 */
    @Test
	public void testBlockchainWrapperAgentCall() throws IOException{
		JSONObject jo = new JSONObject();
		String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
		assertNotNull(new JSONObject(v).get("txHash"));
		assertNotNull(new JSONObject(v).get("sandr"));
	}
    
	
	/** test if Blockchain Wrapper works if called directly
	 * Assuming that a run was completed beforehand
	 */
	@Test
	public void testBlockchainWrapperDirect() {
		BlockchainWrapper bc = new BlockchainWrapper();
		//looks for last created directory through the Metadata Query
		String directorychosen= bc.getLastModifiedDirectory();
		System.out.println(directorychosen);
		//looks for the data according to the csvs stored
		JSONObject graData  = bc.provideJSONResult(directorychosen);
		JSONObject jo = bc.determineValue (graData);
		System.out.println(jo.toString());
		assertNotNull(jo);
		JSONObject result = bc.calculateTrade(jo);
		assertNotNull(result.get("txHash"));
		assertNotNull(result.get("sandr"));
		
	}
}
