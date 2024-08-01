package uk.ac.cam.cares.jps.agent.assetmanager;

import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.*;


public class deleteTest extends EndpointTest {
      /*
     * Functionality tested:
    *       "/retrieve", 
            "/retrievebydocs", 
            "/getuidata", 
            "/instantiate",
            "/addmaintenance",
            "/updatetime",
            "/deletemaintenance",
            "/delete"
        Functionality not tested (relies on other agent / printer):
            "/print", 
            "/printbulk",
            "/addmanual",
            "/addassetimage",
            "/addpurchdocfile"
            
     */

    /*
    ==EXPECTED BEHAVIOUR TESTED==
    Test fail when content is invalid
        - ID is invalid IRI/ID
        - ID does not exist

    //Test succeed
        - ID using assetID or IRI
        - Asset in either lab or office namespace
    */
        
    //read example test data
    JSONObject doesNotExist, invalid;
    JSONObject inLab, justID, justIRI;
    
    @Before
    public void importJSONfiles () throws Exception{
        try {
            doesNotExist = parseJSON(folderReqBody  + "/retrieve/fail/retrieve_doesNotExist.json");
            invalid = parseJSON(folderReqBody  + "/retrieve/fail/retrieve_invalid.json");
            
            justID = parseJSON(folderReqBody  + "/retrieve/success/retrieve_justID.json");
            justIRI = parseJSON(folderReqBody  + "/retrieve/success/retrieve_justIRI.json");
            inLab = parseJSON(folderReqBody  + "/retrieve/success/retrieve_inLab.json");
            
        } catch (Exception e) {
            throw new Exception("Test Failed when importing request body from file:: " + e);
        }
    }

    @Before
    private void createExampleData () throws Exception{
        // Reuse instances from testInstantiate
        JSONObject defaultBody, existingIRI, inLab, outOfCARES;
        try{
            defaultBody = parseJSON(folderReqBody  + "/init/success/init_default.json");
            existingIRI = parseJSON(folderReqBody  + "/init/success/init_existingIRI.json");
            inLab = parseJSON(folderReqBody  + "/init/success/init_inLab.json");
            outOfCARES = parseJSON(folderReqBody  + "/init/success/init_outOfCARES.json");
        }
        catch (Exception e){
            throw new Exception("Test Failed when sample init request body from file:: " + e);
        }
        try {
            //TODO Check if init succeed from returned value. Currently assumes it succeed
            agent.getRequestParameters(defaultBody, "/instantiate");
            agent.getRequestParameters(existingIRI, "/instantiate");
            agent.getRequestParameters(inLab, "/instantiate");
            agent.getRequestParameters(outOfCARES, "/instantiate");
        } catch (Exception e) {
            throw new Exception("Failed to create sample instances:: " + e);
        }
    }

    //Test calling agents
    //@Ignore("Reuiqres docker installed")
    @Test
    public void deleteInvalidID() {
        JSONObject invalidID = new JSONObject("{\"assetData\":{\"ID\": \"duck\"}}");
        JSONObject result = agent.getRequestParameters(invalidID, "/delete");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
    
        JSONObject notExistID = new JSONObject("{\"assetData\":{\"ID\": \"2023-10-27/6000\"}}");
        result = agent.getRequestParameters(notExistID, "/delete");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void deleteSuccess() {
        String[] idArr = {"2023-10-27/6", "2023-10-27/5", "2023-10-27/7", "https://www.theworldavatar.com/kg/ontodevice/Device_fdaa745d-c447-4a90-ab13-b00e0d95675c"};
        for (String id: idArr ){
            agent.getRequestParameters(new JSONObject("{\"assetData\": {\"ID\" : "+id+"}}"), "/deletemaintenance");
        }
    }
}