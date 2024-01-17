package uk.ac.cam.cares.jps.ship.coordination;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.coordination.ADMSCoordinationAgentForShipWithoutComposition;
import uk.ac.cam.cares.jps.coordination.AgentCallerWrapper;

import javax.ws.rs.BadRequestException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;


public class ADMSCoordinationAgentTest {

    @Test
    public void testCheckAgent() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkAgent=admsCoordAgent.getClass().getDeclaredMethod("checkAgent", JSONObject.class);
        checkAgent.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkAgent.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("agent","");
        val= checkAgent.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("agent",JSONObject.NULL);
        val= checkAgent.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        requestParams.put("agent","testAgent");
        val= checkAgent.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckLocation() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkLocation=admsCoordAgent.getClass().getDeclaredMethod("checkLocation", JSONObject.class);
        checkLocation.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkLocation.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("location","");
        val=checkLocation.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("location",JSONObject.NULL);
        val= checkLocation.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.put("location","testLocation");
        val= checkLocation.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckReactionMechanism() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkReactionMechanism=admsCoordAgent.getClass().getDeclaredMethod("checkReactionMechanism", JSONObject.class);
        checkReactionMechanism.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkReactionMechanism.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("reactionmechanism","");
        val=checkReactionMechanism.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("reactionmechanism",JSONObject.NULL);
        val= checkReactionMechanism.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.put("reactionmechanism","testReactionMechanism");
        val= checkReactionMechanism.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckRegion() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkRegion=admsCoordAgent.getClass().getDeclaredMethod("checkRegion", JSONObject.class);
        checkRegion.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkRegion.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        val=checkRegion.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("region",JSONObject.NULL);
        val= checkRegion.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.remove("region");
        region.put("key","value");
        requestParams.put("region",region);
        val= checkRegion.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testSrsname() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkSrsname=admsCoordAgent.getClass().getDeclaredMethod("checkSrsname", JSONObject.class);
        checkSrsname.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        String val= checkSrsname.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        region.put("srsname",JSONObject.NULL);
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        region.put("srsname","");
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        region.remove("srsname");
        region.put("srsname","testSrs");
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckLowerCorner() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkLowerCorner=admsCoordAgent.getClass().getDeclaredMethod("checkLowerCorner", JSONObject.class);
        checkLowerCorner.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        String val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        region.put("lowercorner",JSONObject.NULL);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        JSONObject lowercorner= new JSONObject();
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx but without ly
        lowercorner.put("lowerx","x_coord");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ly but without lx
        lowercorner.remove("lowerx");
        lowercorner.put("lowery","y_coord");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx and ly null
        lowercorner.put("lowery",JSONObject.NULL);
        lowercorner.put("lowerx",JSONObject.NULL);
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx and ly empty
        lowercorner.put("lowery","");
        lowercorner.put("lowerx","");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx and ly as a valid key:value pair
        lowercorner.put("lowery","x_coord");
        lowercorner.put("lowerx","y_coord");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }
    @Test
    public void testCheckUpperCorner() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent= new ADMSCoordinationAgentForShipWithoutComposition();
        Method checkUpperCorner=admsCoordAgent.getClass().getDeclaredMethod("checkUpperCorner", JSONObject.class);
        checkUpperCorner.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        String val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        region.put("uppercorner",JSONObject.NULL);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        JSONObject uppercorner= new JSONObject();
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux but without uy
        uppercorner.put("upperx","x_coord");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with uy but without ux
        uppercorner.remove("upperx");
        uppercorner.put("uppery","y_coord");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux and uy null
        uppercorner.put("uppery",JSONObject.NULL);
        uppercorner.put("upperx",JSONObject.NULL);
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux and uy empty
        uppercorner.put("uppery","");
        uppercorner.put("upperx","");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux and uy as a valid key:value pair
        uppercorner.put("uppery","x_coord");
        uppercorner.put("upperx","y_coord");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsCoordAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testValidateInput() {
        ADMSCoordinationAgentForShipWithoutComposition admsCoordAgent = new ADMSCoordinationAgentForShipWithoutComposition();
        //Start with an empty request Parameter
        JSONObject requestParams = new JSONObject();
        try {
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("RequestParam is empty", e.getMessage());
        }
        //test the block associated with the key:agent
        requestParams.put("agent","");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:agent is missing or is null or is empty.",e.getMessage());
        }
        requestParams.put("agent",JSONObject.NULL);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:agent is missing or is null or is empty.",e.getMessage());
        }
        //test the block associated with the key:location
        requestParams.put("agent","testAgent");
        requestParams.put("location","");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:location is missing or is null or is empty.",e.getMessage());
        }
        requestParams.put("location",JSONObject.NULL);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:location is missing or is null or is empty.",e.getMessage());
        }
        //test the block associated with the key:reactionmechanism
        requestParams.put("location","testLocation");
        requestParams.put("reactionmechanism","");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:reactionmechanism is missing or is null or is empty.",e.getMessage());
        }
        requestParams.put("reactionmechanism",JSONObject.NULL);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:reactionmechanism is missing or is null or is empty.",e.getMessage());
        }
        //test the block associated with key:region
        requestParams.put("reactionmechanism","testReactionMechanism");
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:region is missing or is null or is empty.",e.getMessage());
        }
        requestParams.put("region",JSONObject.NULL);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:region is missing or is null or is empty.",e.getMessage());
        }
        //test the block associated with key:srsname
        region.put("srsname","");
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object either the key:srsname is missing or is null or is empty.",e.getMessage());
        }
        region.put("srsname",JSONObject.NULL);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object either the key:srsname is missing or is null or is empty.",e.getMessage());
        }
        //test the block associated with key:lowercorner
        region.put("srsname","testSrc");
        JSONObject lowerCorner=new JSONObject();
        region.put("lowercorner",lowerCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        region.put("lowercorner",JSONObject.NULL);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        lowerCorner.put("lowerx","x_coord");
        region.put("lowercorner",lowerCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        lowerCorner.remove("lowerx");
        lowerCorner.put("lowery","y_coord");
        region.put("lowercorner",lowerCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        lowerCorner.put("lowerx",JSONObject.NULL);
        lowerCorner.put("lowery",JSONObject.NULL);
        region.put("lowercorner",lowerCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        lowerCorner.put("lowerx","");
        lowerCorner.put("lowery","");
        region.put("lowercorner",lowerCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        //test the block associated with key:uppercorner
        lowerCorner.put("lowerx","x_coord");
        lowerCorner.put("lowery","y_coord");
        JSONObject upperCorner=new JSONObject();
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        region.put("uppercorner",JSONObject.NULL);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        upperCorner.put("upperx","x_coord");
        region.put("uppercorner",upperCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        upperCorner.remove("upperx");
        upperCorner.put("uppery","y_coord");
        region.put("uppercorner",upperCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        upperCorner.put("upperx",JSONObject.NULL);
        upperCorner.put("uppery",JSONObject.NULL);
        region.put("uppercorner",upperCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        upperCorner.put("upperx","");
        upperCorner.put("uppery","");
        region.put("uppercorner",upperCorner);
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        //finally check the case for missing keys
        upperCorner.put("upperx","x_coord");
        upperCorner.put("uppery","y_coord");
        requestParams.put("uppercorner",upperCorner);
        //missing key:agent
        requestParams.remove("agent");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:agent is missing or is null or is empty.",e.getMessage());
        }
        //missing key:location
        requestParams.put("agent","testAgent");
        requestParams.remove("location");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:location is missing or is null or is empty.",e.getMessage());
        }
        //missing key:reactionmechanism
        requestParams.put("location","testLocation");
        requestParams.remove("reactionmechanism");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:reactionmechanism is missing or is null or is empty.",e.getMessage());
        }
        //missing key:region
        requestParams.put("reactionmechanism","testReactionMechanism");
        requestParams.remove("region");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:region is missing or is null or is empty.",e.getMessage());
        }
        //missing key:srsname
        region.remove("srsname");
        requestParams.put("region",region);
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object either the key:srsname is missing or is null or is empty.",e.getMessage());
        }
        //missing key:lowercorner
        region.put("srsname","testSrc");
        region.remove("lowercorner");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        //missing uppercorner
        region.put("lowercorner",lowerCorner);
        region.remove("uppercorner");
        try{
            admsCoordAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
    }

    @Test
    public void testGetNewWasteAsync() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException{
        AgentCallerWrapper mockWrapper= Mockito.mock(AgentCallerWrapper.class);
        ADMSCoordinationAgentForShipWithoutComposition admsCoordinationAgentForShipWithoutComposition= new ADMSCoordinationAgentForShipWithoutComposition(mockWrapper);
        Method getNewWasteAsync= admsCoordinationAgentForShipWithoutComposition.getClass().getDeclaredMethod("getNewWasteAsync", String.class, JSONObject.class);
        getNewWasteAsync.setAccessible(true);

        Method setLogger= admsCoordinationAgentForShipWithoutComposition.getClass().getDeclaredMethod("setLogger");
        setLogger.setAccessible(true);

        //construct the JSONObject to be passed in as an argument
        JSONObject jsonObject= new JSONObject();
        JSONObject uppercorner= new JSONObject();
        JSONObject lowercorner= new JSONObject();
        JSONObject region =new JSONObject();

        jsonObject.put("location","Singapore");

        uppercorner.put("ux","1110");
        uppercorner.put("uy","2222");

        lowercorner.put("lx","111");
        lowercorner.put("ly","222");
        region.put("srsname","EPSG:1234");
        region.put("lowercorner",lowercorner);
        region.put("uppercorner",uppercorner);
        jsonObject.put("region",region);
        jsonObject.put("agent","testAgent");
        jsonObject.put("reactionmechanism","testReactionMech");

        String shipSource="{\"collection\":{\"items\":[{\"ss\":16.9,\"country\":\"Singapore\",\"mmsi\":563064250}]}}";
        JSONObject ship= new JSONObject(shipSource);
        jsonObject.put("ship",ship);

        String reactionMechanism = jsonObject.optString("reactionmechanism");
        JSONObject jsonReactionShip = new JSONObject();
        jsonReactionShip.put("reactionmechanism", reactionMechanism);
        jsonReactionShip.put("ship", ship.getJSONObject("collection").getJSONArray("items").getJSONObject(0));

        String wasteResult= "{\"waste\":\"wasteInfo\"}";
        JSONArray expected=new JSONArray();
        expected.put("wasteInfo");

        Mockito.doReturn(wasteResult).when(mockWrapper).executeGet("/JPS_SHIP/ShipAgent", "query", jsonReactionShip.toString());
        setLogger.invoke(admsCoordinationAgentForShipWithoutComposition);
        JSONArray actual = (JSONArray) getNewWasteAsync.invoke(admsCoordinationAgentForShipWithoutComposition, reactionMechanism, ship);

        Assert.assertEquals(expected.length(),actual.length());
        for (int i=0; i<actual.length();i++){
            Assert.assertEquals(expected.get(i),actual.get(i));
        }
    }

    @Test
    public void testCreateFolder() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        AgentCallerWrapper mockWrapper= Mockito.mock(AgentCallerWrapper.class);
        ADMSCoordinationAgentForShipWithoutComposition admsCoordinationAgentForShipWithoutComposition= new ADMSCoordinationAgentForShipWithoutComposition(mockWrapper);
        Method createFolder= admsCoordinationAgentForShipWithoutComposition.getClass().getDeclaredMethod("createFolder", JSONObject.class, JSONObject.class);
        createFolder.setAccessible(true);

        Method setLogger= admsCoordinationAgentForShipWithoutComposition.getClass().getDeclaredMethod("setLogger");
        setLogger.setAccessible(true);

        //construct the JSONObject to be passed in as an argument
        JSONObject jsonObject=new JSONObject();
        JSONObject uppercorner= new JSONObject();
        JSONObject lowercorner= new JSONObject();
        JSONObject region =new JSONObject();

        jsonObject.put("location","Singapore");

        uppercorner.put("ux","1110");
        uppercorner.put("uy","2222");

        lowercorner.put("lx","111");
        lowercorner.put("ly","222");
        region.put("srsname","EPSG:1234");
        region.put("lowercorner",lowercorner);
        region.put("uppercorner",uppercorner);
        jsonObject.put("region",region);
        jsonObject.put("agent","testAgent");
        jsonObject.put("reactionmechanism","testReactionMech");

        String shipSource="{\"collection\":{\"items\":[{\"ss\":16.9,\"country\":\"Singapore\",\"mmsi\":563064250}]}}";
        JSONObject ship= new JSONObject(shipSource);
        jsonObject.put("ship",ship);

        String wasteResult= "{\"waste\":\"wasteInfo\"}";
        JSONArray newwaste=new JSONArray();
        newwaste.put("wasteInfo");
        jsonObject.put("waste",newwaste);

        String reactionMechanism = jsonObject.optString("reactionmechanism");
        JSONObject jsonReactionShip = new JSONObject();
        jsonReactionShip.put("reactionmechanism", reactionMechanism);
        jsonReactionShip.put("ship", ship.getJSONObject("collection").getJSONArray("items").getJSONObject(0));

        String result="{\"folder\":\"testFolder\"}";
        String expected = new JSONObject(result).getString("folder");

        //treat method for waste
        Mockito.doReturn(wasteResult).when(mockWrapper).executeGet("/JPS_SHIP/ShipAgent", "query", jsonReactionShip.toString());
        setLogger.invoke(admsCoordinationAgentForShipWithoutComposition);

        try(MockedStatic<AgentCaller>agentCaller = Mockito.mockStatic(AgentCaller.class)){
            agentCaller.when(()->AgentCaller.executePost("/JPS/ADMSAgent", jsonObject.toString())).thenReturn(result);
            String actual=(String) createFolder.invoke(admsCoordinationAgentForShipWithoutComposition,jsonObject,ship);
            Assert.assertEquals(expected,actual);
        }
    }

    @Test
    public void testProcessRequestParam() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException{
        AgentCallerWrapper mockWrapper= Mockito.mock(AgentCallerWrapper.class);
        ADMSCoordinationAgentForShipWithoutComposition admsCoordinationAgentForShipWithoutComposition= new ADMSCoordinationAgentForShipWithoutComposition(mockWrapper);
        Method setLogger= admsCoordinationAgentForShipWithoutComposition.getClass().getDeclaredMethod("setLogger");
        setLogger.setAccessible(true);

        //construct the JSONObject to be passed in as requestParams
        JSONObject jsonObject =new JSONObject();

        JSONObject uppercorner= new JSONObject();
        JSONObject lowercorner= new JSONObject();
        JSONObject region =new JSONObject();

        jsonObject.put("location","Singapore");

        uppercorner.put("upperx","1110");
        uppercorner.put("uppery","2222");

        lowercorner.put("lowerx","111");
        lowercorner.put("lowery","222");
        region.put("srsname","EPSG:1234");
        region.put("lowercorner",lowercorner);
        region.put("uppercorner",uppercorner);
        jsonObject.put("region",region);
        jsonObject.put("agent","testAgent");
        jsonObject.put("reactionmechanism","testReactionMech");

        String regionToCityResult="{\"city\":\"http://dbpedia.org/resource/Singapore\"}";
        String weatherSource = "{\"weatherstate\":{\"hashumidity\":{\"hasvalue\":\"49\"},\"hasexteriortemperature\":{\"hasvalue\":\"33.18\"},\"haswind\":{\"hasspeed\":\"4.1\",\"hasdirection\":\"95\"},\"hascloudcover\":{\"hascloudcovervalue\":\"75\"},\"hasweathercondition\":\"few_clouds\",\"hasprecipation\":{\"hasintensity\":\"0.0\"}}}";
        String shipSource="{\"collection\":{\"items\":[{\"ss\":16.9,\"country\":\"Singapore\",\"mmsi\":563064250}]}}";
        String buildingSource="{\"building\":[{\"BldIRI\":\"myIRI\",\"BldName\":\"Building 404\",\"BldType\":0,\"BldX\":45.0,\"BldY\":90.0,\"BldLength\":222.0,\"BldHeight\":120.0,\"BldWidth\":156.0,\"BldAngle\":85.0}]}";
        JSONObject weatherState = new JSONObject(weatherSource);
        JSONObject ship= new JSONObject(shipSource);
        JSONObject city= new JSONObject(regionToCityResult);
        JSONArray building = new JSONObject(buildingSource).getJSONArray("building");

        JSONObject jsonObjectCity=new JSONObject();
        JSONObject jsonObjectCityCopy=copyVal(jsonObject,jsonObjectCity);
        jsonObjectCityCopy.put("city",city.getString("city"));

        JSONObject jsonObjectBuilding=new JSONObject();
        JSONObject jsonObjectBuildingCopy= copyVal(jsonObjectCityCopy,jsonObjectBuilding);
        jsonObjectBuildingCopy.put("building",building);

        JSONObject jsonObjectWeather= new JSONObject();
        JSONObject jsonObjectWeatherCopy=copyVal(jsonObjectBuildingCopy,jsonObjectWeather);
        jsonObjectWeatherCopy.put("weatherstate",weatherState.getJSONObject("weatherstate"));
        JSONObject jsonObjectShip= new JSONObject();
        JSONObject jsonObjectShipCopy= copyVal(jsonObjectWeatherCopy,jsonObjectShip);
        jsonObjectShipCopy.put("ship",ship);

        String baseUrl= "http://myTestUrl";
        String url=baseUrl+"/getEntitiesWithinRegion";

        String wasteResult= "{\"waste\":\"wasteInfo\"}";
        JSONArray newwaste=new JSONArray();
        newwaste.put("wasteInfo");
        JSONObject wasteObj= new JSONObject();
        JSONObject wasteObjCopy=copyVal(jsonObjectShipCopy,wasteObj);
        wasteObjCopy.put("waste",newwaste);

        JSONObject folder= new JSONObject();
        JSONObject folderCopy=copyVal(wasteObj,folder);
        JSONObject expected= folderCopy.put("folder","testFolder");

        String reactionMechanism = jsonObject.optString("reactionmechanism");
        JSONObject jsonReactionShip = new JSONObject();
        jsonReactionShip.put("reactionmechanism", reactionMechanism);
        jsonReactionShip.put("ship", ship.getJSONObject("collection").getJSONArray("items").getJSONObject(0));

        String folderSource="{\"folder\":\"testFolder\"}";

        //treat method for waste
        Mockito.doReturn(wasteResult).when(mockWrapper).executeGet("/JPS_SHIP/ShipAgent", "query", jsonReactionShip.toString());
        setLogger.invoke(admsCoordinationAgentForShipWithoutComposition);

        try(MockedStatic<AgentCaller>agentCaller = Mockito.mockStatic(AgentCaller.class)){
            try(MockedStatic<KeyValueManager> keyvalueManager=Mockito.mockStatic(KeyValueManager.class)) {
                    //treat static methods called in processRequestParameters
                    keyvalueManager.when(() -> KeyValueManager.get(IKeys.URL_POSITIONQUERY)).thenReturn(baseUrl);
                    agentCaller.when(() -> AgentCaller.executeGet("/JPS/RegionToCity", "query", jsonObject.toString())).thenReturn(regionToCityResult);
                    agentCaller.when(() -> AgentCaller.executeGet("/JPS/GetBuildingListFromRegion", "query", jsonObjectCityCopy.toString())).thenReturn(buildingSource);
                    agentCaller.when(() -> AgentCaller.executeGet("/JPS_COMPOSITION/CityToWeather", "query", regionToCityResult)).thenReturn(weatherSource);
                    agentCaller.when(() -> AgentCaller.executeGetWithURLAndJSON(url, jsonObjectWeatherCopy.toString())).thenReturn(shipSource);

                    //treat method for folder
                    agentCaller.when(() -> AgentCaller.executePost("/JPS/ADMSAgent", wasteObjCopy.toString())).thenReturn(folderSource);

                    Object actual = (JSONObject) admsCoordinationAgentForShipWithoutComposition.processRequestParameters(jsonObject);
                    Assert.assertEquals(expected.toString(), actual.toString());
            }
        }
    }

    /**
     * Helper function for creating
     * deep copies of JSONObjects.
     */
    private JSONObject copyVal(JSONObject original, JSONObject modified){
        Iterator<String> keys = original.keys();
        while(keys.hasNext()){
            String key = keys.next();
            modified.put(key,original.get(key));
        }
        return modified;
    }


}
