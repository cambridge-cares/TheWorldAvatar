package uk.ac.cam.cares.jps.agent.assetmanager;

import static org.junit.Assert.assertTrue;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;

import java.time.LocalDate;


public class maintenanceTest extends EndpointTest {
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
        - Add maintenance:
            - ID does not exist / invalid
            - ID is an IRI (only accepts asset ID)
            - next service time after last service time
            - last service time in the future
            - negative interval or 0 month interval
            - no service provider
        - update maintenance time:
            SHOULD NOT FAIL unless the other feature failed
        - delete maintenenance schedule:
            - IRI does not exist

    //Test succeed
        - Add maintenance:
            - One time maintenance - with either last service or next schedule
            - Regular maintenance - with next service schedule
            - Regular maintenance - without next service schedule, with interval
            - Multiple maintenance sched on a single asset
        - update maintenance time:
            SHOULD NOT FAIL unless the other feature failed
        - delete maintenenance schedule:
            - Just run on an existing maintenance schedule IRI, should remove the all associated triples
    */
        
    //read example test data
    JSONObject doesNotExist, invalidID, lastInFuture, nextBeforeLast, nonPositiveInterval, noServiceProvider, useIRI;
    JSONObject addOneTime, regular, regularNoNext;
    
    @Before
    public void importJSONfiles () throws Exception{
        try {
            doesNotExist = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_delete_idDoesNotExist.json");
            invalidID = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_invalidID.json");
            lastInFuture = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_lastInFuture.json");
            nextBeforeLast = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_nextBeforeLast.json");
            nonPositiveInterval = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_nonPositiveInterval.json");
            noServiceProvider = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_noServiceProvider.json");
            useIRI = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_useIRI.json");
            
            addOneTime = parseJSON(folderReqBody  + "/maintenance/success/maintenance_add_oneTime.json");
            regular = parseJSON(folderReqBody  + "/maintenance/success/maintenance_add_regular.json");
            regularNoNext = parseJSON(folderReqBody  + "/maintenance/success/maintenance_add_regular_withoutNext.json");
        
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
    public void testInvalidAssetID() {
        JSONObject result = agent.getRequestParameters(invalidID, "/addmaintenance");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Maintenance data is invalid."));
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testInvalidMaintenanceTiming() {
        lastInFuture.put("LastService", LocalDate.now().plusDays(1).toString());
        JSONObject resultLastInFuture = agent.getRequestParameters(lastInFuture, "/addmaintenance");
        assertTrue(resultLastInFuture.getJSONArray("Result").getString(0).contains("Last service date cannot be in the future"));

        JSONObject resultNextBeforeLast = agent.getRequestParameters(nextBeforeLast, "/addmaintenance");
        assertTrue(resultNextBeforeLast.getJSONArray("Result").getString(0).contains("Next service date is before last service date"));

        JSONObject resultNonPositiveInterval = agent.getRequestParameters(nonPositiveInterval, "/addmaintenance");
        assertTrue(resultNonPositiveInterval.getJSONArray("Result").getString(0).contains("Interval has to be > 0"));
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testNoServiceProvider() {
        JSONObject result = agent.getRequestParameters(noServiceProvider, "/addmaintenance");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Maintenance data is invalid."));
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testAddMaintenance() {
        JSONObject result = agent.getRequestParameters(addOneTime, "/addmaintenance");
        result = agent.getRequestParameters(regular, "/addmaintenance");
        result = agent.getRequestParameters(regularNoNext, "/addmaintenance");

        //TODO check here if instances are created correctly
        JSONArray allMaintenanceData = agent.instanceHandler.assetRetriever.getAllMaintenanceScheduleIRI();
        assertTrue(allMaintenanceData.length() > 0);
        //Check triple data here
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testUpdateMaintenance() {
        JSONObject result = agent.getRequestParameters(new JSONObject("{\"assetData\": {}}"), "/updatetime");

        //TODO check if time instances are updated correctly
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testAssetDoesNotExist() {
        JSONObject result = agent.getRequestParameters(doesNotExist, "/addmaintenance");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Device is unregistered for ID"));

        result = agent.getRequestParameters(doesNotExist, "/deletemaintenance");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Device is unregistered for ID"));
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testDeleteMaintenance() {
        //Get IRI of the maintenance data and call delete on all
        JSONArray maintenanceList = agent.instanceHandler.assetRetriever.getAllMaintenanceScheduleIRI();
        for (int i = 0; i < maintenanceList.length(); i++) {
            String maintenanceScheduleIRI = maintenanceList.getJSONObject(i).getString("maintenanceScheduleIRI");
            agent.getRequestParameters(new JSONObject("{\"assetData\": {\"ID\" : "+maintenanceScheduleIRI+"}}"), "/deletemaintenance");
        }

        maintenanceList = agent.instanceHandler.assetRetriever.getAllMaintenanceScheduleIRI();
        assertTrue(maintenanceList.length() == 0);

        //TODO check here if instances are deleted correctly
    }
}