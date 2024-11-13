package uk.ac.cam.cares.jps.agent.assetmanager;


import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.*;


public class InstantiateTest extends EndpointTest {
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
    Test fail when content is invalid - singular asset
        - Missing mandatory field
        - ID invalid or already exist
        - AssetClass not in list
        - key Room empty/ room does not exist when FacilityLocation in CARES premise      
    //Test fail when content is invalid - set assets
        - No setData key
        * NOTE: The request content of each assets does not need to be retested as internally
        it called the same methods for singular assets - as long as the singular asset methods are tested first
    //Test succeed - singular asset and set assets
        - in Cares Office / Cares Lab / out of Cares facility
        - with/without ID provided
        - with/without existing IRIs
        - 
    */
        
        //read example test data
        JSONObject missingMandatory, invalidID, noRoomKey, invalidAssetClass, roomNotRecorded;
        JSONObject defaultBody, existingIRI, inLab, noID, outOfCARES;
        
        @Before
        public void importJSONfiles () throws Exception{
            try {
                missingMandatory = parseJSON(folderReqBody  + "/init/fail/init_missingMandatory.json");
                invalidID = parseJSON(folderReqBody  + "/init/fail/init_invalidID.json");
                noRoomKey = parseJSON(folderReqBody  + "/init/fail/init_NoRoomKey.json");
                invalidAssetClass = parseJSON(folderReqBody  + "/init/fail/init_invalidAssetClass.json");
                roomNotRecorded = parseJSON(folderReqBody  + "/init/fail/init_roomDoesNotExist.json");

                defaultBody = parseJSON(folderReqBody  + "/init/success/init_default.json");
                existingIRI = parseJSON(folderReqBody  + "/init/success/init_existingIRI.json");
                inLab = parseJSON(folderReqBody  + "/init/success/init_inLab.json");
                noID = parseJSON(folderReqBody  + "/init/success/init_NoID.json");
                outOfCARES = parseJSON(folderReqBody  + "/init/success/init_outOfCARES.json");

            }catch (Exception e) {
                throw new Exception("Test Failed when importing request body from file:: " + e);
            }
        }

        //Test calling agents
        //@Ignore("Reuiqres docker installed")
        @Test
        public void testMandatoryField() { 
            assertTrue(agent.getRequestParameters(missingMandatory, "/instantiate").getJSONArray("Result").getString(0).contains("The asset class keys cannot be retrieved from the properties file:"));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testinvalidID() { 
            assertTrue(agent.getRequestParameters(invalidID, "/instantiate").getJSONArray("Result").getString(0).contains("Asset data is invalid."));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testRoomsAndLocationsError() { 
            assertTrue(agent.getRequestParameters(roomNotRecorded, "/instantiate").getJSONArray("Result").getString(0).contains("Instantiation failed"));
            assertTrue(agent.getRequestParameters(noRoomKey, "/instantiate").getJSONArray("Result").getString(0).contains("Instantiation failed"));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testDefaultBody() { 
            JSONObject result = agent.getRequestParameters(defaultBody, "/instantiate");
            System.out.print(defaultBody);
            System.out.print(result);
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2023-10-27/6"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").contains("https://www.theworldavatar.com/kg/ontodevice/"));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testExistingIRI() { 
            JSONObject result = agent.getRequestParameters(existingIRI, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2023-10-27/4"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").equals("https://www.theworldavatar.com/kg/ontodevice/Device_fdaa745d-c447-4a90-ab13-b00e0d95675c"));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testLocationOutOfCARES() { 
            JSONObject result = agent.getRequestParameters(outOfCARES, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2023-10-27/7"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").contains("https://www.theworldavatar.com/kg/ontodevice/"));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testNoIDProvided() { 
            JSONObject result = agent.getRequestParameters(noID, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            //assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2024-03-10/8")); // Assuming the test order follows the order it is written it should be 8.
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").contains("2024-03-10"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").contains("https://www.theworldavatar.com/kg/ontodevice/"));
        }

        //@Ignore("Reuiqres docker installed")
        @Test
        public void testAssetAlreadyExist () throws Exception {
            //Assumes the other unit tests succeed
            defaultBody.put("ID", "2023-10-27/1000");
            JSONObject debug = agent.getRequestParameters(defaultBody, "/instantiate");
            if(debug.getJSONArray("Result").getString(1).contains("Command Success")){
                JSONObject result = agent.getRequestParameters(defaultBody, "/instantiate");
                //assertTrue(result.getJSONArray("Result").getString(0).contains("Instance already exist for id:"));
                assertTrue(result.getJSONArray("Result").getString(0).contains("Instantiation failed:"));
            }
            else{
                throw new Exception("Failed to create duplicate for instantiate duplicate test" );
            }
        }
}