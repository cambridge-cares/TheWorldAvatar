package uk.ac.cam.cares.jps.admsagent;


import org.junit.Rule;
import org.junit.rules.TemporaryFolder;
import org.mockito.*;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.stubbing.Answer;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.admsagent.ADMSAgent;

import javax.management.Query;
import javax.ws.rs.BadRequestException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class ADMSAgentTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testCheckRegion() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkRegion=admsAgent.getClass().getDeclaredMethod("checkRegion", JSONObject.class);
        checkRegion.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkRegion.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        val=checkRegion.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("region",JSONObject.NULL);
        val= checkRegion.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.remove("region");
        region.put("key","value");
        requestParams.put("region",region);
        val= checkRegion.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testSrsname() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkSrsname=admsAgent.getClass().getDeclaredMethod("checkSrsname", JSONObject.class);
        checkSrsname.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        String val= checkSrsname.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        region.put("srsname",JSONObject.NULL);
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);

        //check case with empty value
        region.put("srsname","");
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);

        //check case with a valid key:value pair
        region.remove("srsname");
        region.put("srsname","testSrs");
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckLowerCorner() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkLowerCorner=admsAgent.getClass().getDeclaredMethod("checkLowerCorner", JSONObject.class);
        checkLowerCorner.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        String val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        region.put("lowercorner",JSONObject.NULL);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        JSONObject lowercorner= new JSONObject();
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx but without ly
        lowercorner.put("lowerx","x_coord");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ly but without lx
        lowercorner.remove("lowerx");
        lowercorner.put("lowery","y_coord");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx and ly null
        lowercorner.put("lowery",JSONObject.NULL);
        lowercorner.put("lowerx",JSONObject.NULL);
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx and ly empty
        lowercorner.put("lowery","");
        lowercorner.put("lowerx","");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with lx and ly as a valid key:value pair
        lowercorner.put("lowery","x_coord");
        lowercorner.put("lowerx","y_coord");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        val= checkLowerCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckUpperCorner() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkUpperCorner=admsAgent.getClass().getDeclaredMethod("checkUpperCorner", JSONObject.class);
        checkUpperCorner.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject region= new JSONObject();
        requestParams.put("region",region);
        String val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        region.put("uppercorner",JSONObject.NULL);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        JSONObject uppercorner= new JSONObject();
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux but without uy
        uppercorner.put("upperx","x_coord");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with uy but without ux
        uppercorner.remove("upperx");
        uppercorner.put("uppery","y_coord");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux and uy null
        uppercorner.put("uppery",JSONObject.NULL);
        uppercorner.put("upperx",JSONObject.NULL);
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux and uy empty
        uppercorner.put("uppery","");
        uppercorner.put("upperx","");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with ux and uy as a valid key:value pair
        uppercorner.put("uppery","x_coord");
        uppercorner.put("upperx","y_coord");
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        val= checkUpperCorner.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckAgent() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkAgent= admsAgent.getClass().getDeclaredMethod("checkAgent", JSONObject.class);
        checkAgent.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkAgent.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("agent","");
        val= checkAgent.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("agent",JSONObject.NULL);
        val= checkAgent.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        requestParams.put("agent","testAgent");
        val= checkAgent.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckCity() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkCity= admsAgent.getClass().getDeclaredMethod("checkCity", JSONObject.class);
        checkCity.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkCity.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("city","");
        val=checkCity.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("city",JSONObject.NULL);
        val= checkCity.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.put("city","http://dbpedia.org/resource/Singapore");
        val= checkCity.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckStationIRI() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkStationIRI= admsAgent.getClass().getDeclaredMethod("checkStationIRI", JSONObject.class);
        checkStationIRI.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkStationIRI.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case1 with empty string value
        JSONArray stationIRI= new JSONArray();
        requestParams.put("stationiri",stationIRI);
        val=checkStationIRI.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case2 with empty string value
        stationIRI.put("testStationIRI1");
        stationIRI.put("");
        requestParams.put("stationiri",stationIRI);
        val=checkStationIRI.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("stationIRI",JSONObject.NULL);
        val= checkStationIRI.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        stationIRI= new JSONArray();
        stationIRI.put("testStationIRI1");
        stationIRI.put("testStationIRI2");
        requestParams.put("stationiri",stationIRI);
        val= checkStationIRI.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckPlant() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkPlant= admsAgent.getClass().getDeclaredMethod("checkPlant", JSONObject.class);
        checkPlant.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkPlant.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("plant","");
        val=checkPlant.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("plant",JSONObject.NULL);
        val= checkPlant.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.put("plant","http://plants.org/myPlantIRI");
        val= checkPlant.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckShip() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkShip= admsAgent.getClass().getDeclaredMethod("checkShip", JSONObject.class);
        checkShip.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkShip.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        JSONObject ship= new JSONObject();
        requestParams.put("ship",ship);
        val=checkShip.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("ship",JSONObject.NULL);
        val= checkShip.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair

        ship.put("key","value");
        requestParams.put("ship",ship);
        val= checkShip.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckMMSI() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkMMSI= admsAgent.getClass().getDeclaredMethod("checkMMSI", JSONObject.class);
        checkMMSI.setAccessible(true);

        //check case with no key
        JSONObject obj= new JSONObject();
        String val= checkMMSI.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        obj.put("mmsi",JSONObject.NULL);
        val=checkMMSI.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        obj.put("mmsi","");
        val=checkMMSI.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        obj.put("mmsi","testMMSI");
        val=checkMMSI.invoke(admsAgent,obj).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckLat() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkLat= admsAgent.getClass().getDeclaredMethod("checkLat", JSONObject.class);
        checkLat.setAccessible(true);

        //check case with no key
        JSONObject obj= new JSONObject();
        String val= checkLat.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        obj.put("lat",JSONObject.NULL);
        val=checkLat.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        obj.put("lat","");
        val=checkLat.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        obj.put("lat","100");
        val=checkLat.invoke(admsAgent,obj).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckLon() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkLon= admsAgent.getClass().getDeclaredMethod("checkLon", JSONObject.class);
        checkLon.setAccessible(true);

        //check case with no key
        JSONObject obj= new JSONObject();
        String val= checkLon.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        obj.put("lon",JSONObject.NULL);
        val=checkLon.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        obj.put("lon","");
        val=checkLon.invoke(admsAgent,obj).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        obj.put("lon","100");
        val=checkLon.invoke(admsAgent,obj).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckItems() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent= new ADMSAgent();
        Method checkItems= admsAgent.getClass().getDeclaredMethod("checkItems", JSONObject.class);
        checkItems.setAccessible(true);

        //check case with no collection key
        JSONObject requestParams= new JSONObject();
        JSONObject ship = new JSONObject();
        requestParams.put("ship",ship);
        String val= checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty collection key
        JSONObject collection= new JSONObject();
        ship.put("collection",collection);
        val= checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with no items key
        collection.put("key","value");
        val= checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with items key with empty value
        JSONArray items= new JSONArray();
        collection.put("items",items);
        val= checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with items key with null value
        items.put(JSONObject.NULL);
        val= checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check individual cases for mmsi, lat , lon in items array
        //case with where none of the keys is inside items
        items = new JSONArray();
        JSONObject obj= new JSONObject();
        obj.put("key","value");
        items.put(obj);
        collection.put("items",items);
        val= checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with keys having null value
        items = new JSONArray();
        obj.put("mmsi", JSONObject.NULL);
        obj.put("lat",JSONObject.NULL);
        obj.put("lon",JSONObject.NULL);
        val=checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with keys having empty value
        items = new JSONArray();
        obj.put("mmsi","");
        obj.put("lon","");
        obj.put("lat","");
        val=checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        items = new JSONArray();
        obj.put("mmsi","563009850");
        obj.put("lon","114.15338");
        obj.put("lat","22.28822");
        val=checkItems.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testValidateInput() throws NoSuchMethodException,IllegalAccessException,InvocationTargetException {
        ADMSAgent admsAgent= new ADMSAgent();
        JSONObject requestParams= new JSONObject();

        //case where requestParams is empty
       try {
           admsAgent.validateInput(requestParams);
           Assert.fail();
       }catch (BadRequestException e){
           Assert.assertEquals("RequestParam is empty.",e.getMessage());
       }

        //case where city,agent,stationiri keys are missing
        requestParams.put("key","value");
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object one of the keys:city,agent and stationiri are either not present or incorrectly assigned.",e.getMessage());
        }
        //case where city,agent,stationiri keys are empty
        requestParams.put("city","");
        requestParams.put("agent","");
        JSONArray stationIRI= new JSONArray();
        requestParams.put("stationiri",stationIRI);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object one of the keys:city,agent and stationiri are either not present or incorrectly assigned.",e.getMessage());
        }
        //case where city,agent,stationiri keys are null
        requestParams.put("city",JSONObject.NULL);
        requestParams.put("agent",JSONObject.NULL);
        requestParams.put("stationiri",JSONObject.NULL);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object one of the keys:city,agent and stationiri are either not present or incorrectly assigned.",e.getMessage());
        }
        //case where region key is missing
        requestParams.put("city","Singapore");
        requestParams.put("agent","testAgent");
        stationIRI.put("http://SingaporeStationIRI/test");
        requestParams.put("stationiri",stationIRI);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:region is missing or is null or is empty.",e.getMessage());
        }
        //case where region key is empty
        JSONObject region = new JSONObject();
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:region is missing or is null or is empty.",e.getMessage());
        }
        //case where region key is null
        requestParams.put("region",JSONObject.NULL);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:region is missing or is null or is empty.",e.getMessage());
        }
        //case with no ship or plant key
        region.put("key","value");
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object one of either the keys:ship or plant are either not present or incorrectly assigned.",e.getMessage());
        }
        //ship key is empty
        JSONObject ship = new JSONObject();
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object one of either the keys:ship or plant are either not present or incorrectly assigned.",e.getMessage());
        }
        //ship key is null
        requestParams.put("ship",JSONObject.NULL);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object one of either the keys:ship or plant are either not present or incorrectly assigned.",e.getMessage());
        }
        //srsname key not present in the region object
        ship.put("key","value");
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the key:srsname is missing.",e.getMessage());
        }
        //lowercorner object is incorrect
        JSONObject lowercorner= new JSONObject();
        region.put("srsname","mySrs");
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        //null values assigned to the keys inside the lowercorner
        lowercorner.put("lowerx",JSONObject.NULL);
        lowercorner.put("lowery",JSONObject.NULL);
        region.put("lowercorner",lowercorner);
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.",e.getMessage());
        }
        //uppercorner object is incorrect
        lowercorner.put("lowerx",50);
        lowercorner.put("lowery",100);
        JSONObject uppercorner= new JSONObject();
        region.put("srsname","mySrs");
        region.put("lowercorner",lowercorner);
        region.put("upperconer",uppercorner);
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        //null values assigned to the keys inside the uppercorner
        uppercorner.put("upperx",JSONObject.NULL);
        uppercorner.put("uppery",JSONObject.NULL);
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.",e.getMessage());
        }
        //collection key missing from ship object
        uppercorner.put("upperx",100);
        uppercorner.put("uppery",300);
        region.put("uppercorner",uppercorner);
        requestParams.put("region",region);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the ship object check the keys: mmsi,lat and lon. Either they are missing or incorrectly assigned.",e.getMessage());
        }
        //container key is empty
        JSONObject collection= new JSONObject();
        ship.put("collection",collection);
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the ship object check the keys: mmsi,lat and lon. Either they are missing or incorrectly assigned.",e.getMessage());
        }
        //items array is empty
        JSONArray items= new JSONArray();
        collection.put("items",items);
        ship.put("collection",collection);
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the ship object check the keys: mmsi,lat and lon. Either they are missing or incorrectly assigned.",e.getMessage());
        }
        //items array contains null value
        items.put(JSONObject.NULL);
        collection.put("items",items);
        ship.put("container",collection);
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the ship object check the keys: mmsi,lat and lon. Either they are missing or incorrectly assigned.",e.getMessage());
        }
        //items does not contain the keys
        JSONObject obj= new JSONObject();
        obj.put("key","value");
        items.put(obj);
        collection.put("items",items);
        ship.put("collection",collection);
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the ship object check the keys: mmsi,lat and lon. Either they are missing or incorrectly assigned.",e.getMessage());
        }
        //the keys in items are null
        items.remove(0);
        obj.remove("key");
        obj.put("mmsi",JSONObject.NULL);
        obj.put("lat",JSONObject.NULL);
        obj.put("lon",JSONObject.NULL);
        items.put(obj);
        collection.put("items",items);
        ship.put("collection",collection);
        requestParams.put("ship",ship);
        try{
            admsAgent.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the ship object check the keys: mmsi,lat and lon. Either they are missing or incorrectly assigned.",e.getMessage());
        }
        // the keys in items are valid
        items.remove(0);
        obj.remove("key");
        obj.put("mmsi","123456789");
        obj.put("lat",22);
        obj.put("lon",120);
        items.put(obj);
        collection.put("items",items);
        ship.put("collection",collection);
        requestParams.put("ship",ship);
        boolean actual=admsAgent.validateInput(requestParams);
        Assert.assertTrue(actual);

        //remove ship key and try a valid plant key
        requestParams.remove("ship");
        requestParams.put("plant","http://testPlant/myIRI");
        actual=admsAgent.validateInput(requestParams);
        Assert.assertTrue(actual);
    }

    @Test
    public void testRetrieveBuildingDataInJSON() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent = new ADMSAgent();
        Method retrieveBuildingDataInJSON = admsAgent.getClass().getDeclaredMethod("retrieveBuildingDataInJSON", JSONObject.class, String.class);
        retrieveBuildingDataInJSON.setAccessible(true);

        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",100);
        upperCorner.put("uppery",300);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",50);
        lowerCorner.put("lowery",100);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","testSrsName");
        String city="http://dbpedia.org/resource/Singapore";

        /**
         * Test based on
         * the original code in the method
         */
/*
        JSONObject req= new JSONObject();
        req.put("region",region);
        req.put("city",city);
        String expected="testString";
        try(MockedStatic<AgentCaller>ac=Mockito.mockStatic(AgentCaller.class)){
            ac.when(()->AgentCaller.executeGet("/JPS/BuildingsData","query",req.toString())).thenReturn(expected);
            String actual=(String) retrieveBuildingDataInJSON.invoke(admsAgent,region,city);
            Assert.assertEquals(expected,actual);
        }
*/
        /**
         * Test based on the mock values
         * directly specified in the method.
         */

        JSONObject building= new JSONObject();
        JSONArray bldIRI= new JSONArray();
        bldIRI.put("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/HongkongDistrict02.owl#BuildingB09332fb1-0b21-4bca-a52c-c71f8cd0e5a1");
        building.put("BldIRI",bldIRI);
        JSONArray bldName= new JSONArray();
        bldName.put("a-a52c-c71f8cd0e5a1");
        building.put("BldName",bldName);
        JSONArray bldType= new JSONArray();
        bldType.put(0);
        building.put("BldType",bldType);
        JSONArray bldX= new JSONArray();
        bldX.put(30283.28271214908);
        building.put("BldX",bldX);
        JSONArray bldY= new JSONArray();
        bldY.put(816155.3357251927);
        building.put("BldY",bldY);
        JSONArray bldHeight= new JSONArray();
        bldHeight.put(130.79999999999998);
        building.put("BldHeight",bldHeight);
        JSONArray bldLength= new JSONArray();
        bldLength.put(16.278820596099706);
        building.put("BldLength",bldLength);
        JSONArray bldWidth= new JSONArray();
        bldWidth.put(17.392230495361243);
        building.put("BldWidth",bldWidth);
        JSONArray bldAngle= new JSONArray();
        bldAngle.put(42.510447078000844);
        building.put("BldAngle",bldAngle);
        String expected= building.toString();

        String actual=(String)retrieveBuildingDataInJSON.invoke(admsAgent,region,city);
        Assert.assertEquals(expected,actual);
    }

    @Test
    public void testGetBuildingData() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent = new ADMSAgent();
        Method getBuildingData= admsAgent.getClass().getDeclaredMethod("getBuildingData", JSONObject.class, String.class);
        getBuildingData.setAccessible(true);


        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",100);
        upperCorner.put("uppery",300);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",50);
        lowerCorner.put("lowery",100);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","testSrsName");
        String city="http://dbpedia.org/resource/Singapore";

        JSONObject building= new JSONObject();
        JSONArray bldIRI= new JSONArray();
        bldIRI.put("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/HongkongDistrict02.owl#BuildingB09332fb1-0b21-4bca-a52c-c71f8cd0e5a1");
        building.put("BldIRI",bldIRI);
        JSONArray bldName= new JSONArray();
        bldName.put("a-a52c-c71f8cd0e5a1");
        building.put("BldName",bldName);
        JSONArray bldType= new JSONArray();
        bldType.put(0);
        building.put("BldType",bldType);
        JSONArray bldX= new JSONArray();
        bldX.put(30283.28271214908);
        building.put("BldX",bldX);
        JSONArray bldY= new JSONArray();
        bldY.put(816155.3357251927);
        building.put("BldY",bldY);
        JSONArray bldHeight= new JSONArray();
        bldHeight.put(130.79999999999998);
        building.put("BldHeight",bldHeight);
        JSONArray bldLength= new JSONArray();
        bldLength.put(16.278820596099706);
        building.put("BldLength",bldLength);
        JSONArray bldWidth= new JSONArray();
        bldWidth.put(17.392230495361243);
        building.put("BldWidth",bldWidth);
        JSONArray bldAngle= new JSONArray();
        bldAngle.put(42.510447078000844);
        building.put("BldAngle",bldAngle);
        String expected= building.toString();
        expected= expected.replace("\"","\'");

        String actual=(String)getBuildingData.invoke(admsAgent,region,city);
        Assert.assertEquals(expected,actual);
    }

    @Test
    public void testGetTargetCRS() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent = new ADMSAgent();
        Method getTargetCRS= admsAgent.getClass().getDeclaredMethod("getTargetCRS", String.class);
        getTargetCRS.setAccessible(true);

        String cityIRI= "http://dbpedia.org/resource/Berlin";
        String expected="EPSG:25833";
        String actual=(String) getTargetCRS.invoke(admsAgent,cityIRI);
        Assert.assertEquals(expected,actual);

        cityIRI= "http://dbpedia.org/resource/The_Hague";
        expected="EPSG:28992";
        actual=(String) getTargetCRS.invoke(admsAgent,cityIRI);
        Assert.assertEquals(expected,actual);

        cityIRI="http://dbpedia.org/resource/Singapore";
        expected="EPSG:3414";
        actual=(String) getTargetCRS.invoke(admsAgent,cityIRI);
        Assert.assertEquals(expected,actual);

        cityIRI="http://dbpedia.org/resource/Hong_Kong";
        expected="EPSG:2326";
        actual=(String) getTargetCRS.invoke(admsAgent,cityIRI);
        Assert.assertEquals(expected,actual);

        cityIRI="http://dbpedia.org/resource/Stockholm";
        expected="EPSG:3857";
        actual=(String) getTargetCRS.invoke(admsAgent,cityIRI);
        Assert.assertEquals(expected,actual);
    }

    @Test
    public void testGetEntityCoordinates() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent admsAgent = new ADMSAgent();
        Method getEntityCoordiantes = admsAgent.getClass().getDeclaredMethod("getEntityCoordinates", JSONObject.class);
        getEntityCoordiantes.setAccessible(true);

        //case where input object is empty
        JSONObject input= new JSONObject();
        JSONArray expected= new JSONArray();
        JSONArray actual= (JSONArray)getEntityCoordiantes.invoke(admsAgent,input);
        Assert.assertEquals(expected.length(),actual.length());
        for (int i=0; i<actual.length();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }

        //case where input object only contains collection key but without the correct JSONObject
        JSONObject collection=  new JSONObject();
        input.put("collection",collection);
        actual= (JSONArray)getEntityCoordiantes.invoke(admsAgent,input);
        Assert.assertEquals(expected.length(),actual.length());
        for (int i=0; i<actual.length();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }

        //case where input object only contains collection key and items key but items key does not contain lat,lon and mmsi
        JSONArray items= new JSONArray();
        JSONObject obj= new JSONObject();
        obj.put("key","value");
        items.put(obj);
        collection.put("items",items);
        actual= (JSONArray)getEntityCoordiantes.invoke(admsAgent,input);
        Assert.assertEquals(expected.length(),actual.length());
        for (int i=0; i<actual.length();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }

        //case where all keys are present
        obj.put("mmsi","12345689");
        obj.put("lat","22");
        obj.put("lon","112");
        JSONObject expectedLatLon= new JSONObject();
        expectedLatLon.put("mmsi",obj.get("mmsi"));
        expectedLatLon.put("lat",obj.getDouble("lat"));
        expectedLatLon.put("lon",obj.getDouble("lon"));
        expected.put(expectedLatLon);
        actual= (JSONArray)getEntityCoordiantes.invoke(admsAgent,input);
        Assert.assertEquals(expected.length(),actual.length());
        for (int i=0; i<actual.length();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }
    }

    @Test
    public void testCreateWeatherInput() throws IOException {
        ADMSAgent admsAgent= new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();

        //Mock file name passed as an argument
        String filename="testFilename";

        //Mock stationIRI passed as an argument
        List<String> stationIRI= new ArrayList<>();
        stationIRI.add("testStationIRI1");
        stationIRI.add("testStationIRI2");

        //create weather data in JSON format
        JSONObject weatherInJSON= new JSONObject();
        JSONObject wind = new JSONObject();
        JSONObject temperature = new JSONObject();
        JSONObject relativehumidity= new JSONObject();
        JSONObject cloudcover= new JSONObject();
        JSONObject precipation = new JSONObject();

        wind.put("hasspeed","12.0");
        wind.put("hasdirection","200.0");
        temperature.put("hasvalue","12.0");
        relativehumidity.put("hasvalue","20.0");
        cloudcover.put("hascloudcovervalue","1.0");
        precipation.put("hasintensity","12.0");

        weatherInJSON.put("haswind",wind);
        weatherInJSON.put("hasexteriortemperature",temperature);
        weatherInJSON.put("hashumidity",relativehumidity);
        weatherInJSON.put("hasprecipation",precipation);
        weatherInJSON.put("hascloudcover",cloudcover);


        admsAgent.createWeatherInput(fullPath,filename,stationIRI);
        File metFile= new File(fullPath+"/test.met");
        Assert.assertTrue(metFile.exists());//check if the file is created
        Assert.assertTrue(metFile.length()>0);//check if there is data inside the file
    }

    @Test
    public void testWriteMetFile() throws IOException {
        ADMSAgent admsAgent= new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();

        //create weather data in JSON format
        JSONObject weatherInJSON= new JSONObject();
        JSONObject wind = new JSONObject();
        JSONObject temperature = new JSONObject();
        JSONObject relativehumidity= new JSONObject();
        JSONObject cloudcover= new JSONObject();
        JSONObject precipation = new JSONObject();

        wind.put("hasspeed",12.0);
        wind.put("hasdirection",200.0);
        temperature.put("hasvalue",12.0);
        relativehumidity.put("hasvalue",0.2);
        cloudcover.put("hascloudcovervalue",1.0);
        precipation.put("hasintensity",50);

        weatherInJSON.put("haswind",wind);
        weatherInJSON.put("hasexteriortemperature",temperature);
        weatherInJSON.put("hashumidity",relativehumidity);
        weatherInJSON.put("hasprecipation",precipation);
        weatherInJSON.put("hascloudcover",cloudcover);

        admsAgent.writeMetFile(weatherInJSON,fullPath);
        File metFile= new File(fullPath+"/test.met");
        Assert.assertTrue(metFile.exists());//check if the file is created
        Assert.assertTrue(metFile.length()>0);//check if there is data inside the file
    }

    @Test
    public void testWriteBkgFile() throws IOException {
        ADMSAgent agent = new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();

        //create mock region object
        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",100);
        upperCorner.put("uppery",300);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",50);
        lowerCorner.put("lowery",100);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","testSrsName");

        agent.writeBkgFile(fullPath);
        File bgdFile= new File(fullPath+"/testbackgrnd.bgd");
        Assert.assertTrue(bgdFile.exists());//check if the file is created
        Assert.assertTrue(bgdFile.length()>0);//check if there is data inside the file
    }

    //case where key:"ship" is present
    @Test
    public void testCreateEmissionInput1() throws IOException {
        ADMSAgent agent = new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();

        JSONObject requestParams= new JSONObject();

        //create mock region object
        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",834498.5457081277);
        upperCorner.put("uppery",817460.3860207011);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",833044.9253603141);
        lowerCorner.put("lowery",816015.5674630373);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","EPSG:3857");

        //create mock stationiri array
        JSONArray stationIRI= new JSONArray();
        stationIRI.put("testStationIRI1");
        stationIRI.put("testStationIRI2");

        //create ship object
        JSONObject ship= new JSONObject();
        JSONObject collection= new JSONObject();
        JSONArray items= new JSONArray();
        JSONObject obj = new JSONObject();
        obj.put("mmsi","563009850");
        obj.put("lat","22.28822");
        obj.put("lon","114.15338");
        items.put(obj);
        collection.put("items",items);
        ship.put("collection",collection);

        requestParams.put("region",region);
        requestParams.put("city","http://dbpedia.org/resource/Hong_Kong");
        requestParams.put("agent","testAgent");
        requestParams.put("stationiri",stationIRI);
        requestParams.put("ship",ship);
        requestParams.put("precipitation","50.0");
        String targetCRSName="EPSG:2326";

        //create mock building object
        JSONObject building= new JSONObject();
        JSONArray bldIRI= new JSONArray();
        bldIRI.put("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/HongkongDistrict02.owl#BuildingB09332fb1-0b21-4bca-a52c-c71f8cd0e5a1");
        building.put("BldIRI",bldIRI);
        JSONArray bldName= new JSONArray();
        bldName.put("a-a52c-c71f8cd0e5a1");
        building.put("BldName",bldName);
        JSONArray bldType= new JSONArray();
        bldType.put(0);
        building.put("BldType",bldType);
        JSONArray bldX= new JSONArray();
        bldX.put(30283.28271214908);
        building.put("BldX",bldX);
        JSONArray bldY= new JSONArray();
        bldY.put(816155.3357251927);
        building.put("BldY",bldY);
        JSONArray bldHeight= new JSONArray();
        bldHeight.put(130.79999999999998);
        building.put("BldHeight",bldHeight);
        JSONArray bldLength= new JSONArray();
        bldLength.put(16.278820596099706);
        building.put("BldLength",bldLength);
        JSONArray bldWidth= new JSONArray();
        bldWidth.put(17.392230495361243);
        building.put("BldWidth",bldWidth);
        JSONArray bldAngle= new JSONArray();
        bldAngle.put(42.510447078000844);
        building.put("BldAngle",bldAngle);

        String buildingInString= building.toString().replace("\"","'");

        agent.createEmissionInput(requestParams, buildingInString, region, targetCRSName, fullPath);
        File aplFile= new File(fullPath+"/test.apl");
        Assert.assertTrue(aplFile.exists());//check if the file is created
        Assert.assertTrue(aplFile.length()>0);//check if there is data inside the file
    }

    //case where key:"plant" is present
    @Test
    public void testCreateEmissionInput2() throws IOException {
        ADMSAgent agent = new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();

        JSONObject requestParams= new JSONObject();

        //create mock region object
        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",834498.5457081277);
        upperCorner.put("uppery",817460.3860207011);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",833044.9253603141);
        lowerCorner.put("lowery",816015.5674630373);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","EPSG:3857");

        //create mock stationiri array
        JSONArray stationIRI= new JSONArray();
        stationIRI.put("testStationIRI1");
        stationIRI.put("testStationIRI2");


        requestParams.put("region",region);
        requestParams.put("city","http://dbpedia.org/resource/Hong_Kong");
        requestParams.put("agent","testAgent");
        requestParams.put("stationiri",stationIRI);
        requestParams.put("plant","myPlantIRI");
        String targetCRSName="EPSG:2326";

        //create mock building object
        JSONObject building= new JSONObject();
        JSONArray bldIRI= new JSONArray();
        bldIRI.put("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/HongkongDistrict02.owl#BuildingB09332fb1-0b21-4bca-a52c-c71f8cd0e5a1");
        building.put("BldIRI",bldIRI);
        JSONArray bldName= new JSONArray();
        bldName.put("a-a52c-c71f8cd0e5a1");
        building.put("BldName",bldName);
        JSONArray bldType= new JSONArray();
        bldType.put(0);
        building.put("BldType",bldType);
        JSONArray bldX= new JSONArray();
        bldX.put(30283.28271214908);
        building.put("BldX",bldX);
        JSONArray bldY= new JSONArray();
        bldY.put(816155.3357251927);
        building.put("BldY",bldY);
        JSONArray bldHeight= new JSONArray();
        bldHeight.put(130.79999999999998);
        building.put("BldHeight",bldHeight);
        JSONArray bldLength= new JSONArray();
        bldLength.put(16.278820596099706);
        building.put("BldLength",bldLength);
        JSONArray bldWidth= new JSONArray();
        bldWidth.put(17.392230495361243);
        building.put("BldWidth",bldWidth);
        JSONArray bldAngle= new JSONArray();
        bldAngle.put(42.510447078000844);
        building.put("BldAngle",bldAngle);

        String buildingInString= building.toString().replace("\"","'");

        agent.createEmissionInput(requestParams, buildingInString, region, targetCRSName, fullPath);
        File aplFile= new File(fullPath+"/test.apl");
        Assert.assertTrue(aplFile.exists());//check if the file is created
        Assert.assertTrue(aplFile.length()>0);//check if there is data inside the file
    }

    @Test
    public void testGetEntityType() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ADMSAgent agent = new ADMSAgent();
        Method getEntityType=agent.getClass().getDeclaredMethod("getEntityType", JSONObject.class);
        getEntityType.setAccessible(true);

        JSONObject obj= new JSONObject();
        obj.put("ship","value");
        obj.put("key","value1");
        String expected="ship";

        String actual=(String)getEntityType.invoke(agent,obj);
        Assert.assertEquals(expected,actual);

        obj.remove("ship");
        obj.put("plant","value");
        expected="plant";
        actual=(String)getEntityType.invoke(agent,obj);
        Assert.assertEquals(expected,actual);
    }

    //case where key:"ship" is present
    @Test
    public void testExecuteModel1() throws IOException {
        ADMSAgent agent = new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();
        /*
        Strategy for the testing the method:
        Firstly, generate all the input files needed to
        run ADMS. Verify if the files are present in the
        directory. Finally, run executeModel in the very same
        directory and see if any output file is generated.
         */


        /*
            Generate met file in the appropriate directory
            by calling the writeMetFile method.
            Check if indeed the met file was generated
         */

        //create weather data in JSON format
        JSONObject weatherInJSON= new JSONObject();
        JSONObject wind = new JSONObject();
        JSONObject temperature = new JSONObject();
        JSONObject relativehumidity= new JSONObject();
        JSONObject cloudcover= new JSONObject();
        JSONObject precipation = new JSONObject();

        wind.put("hasspeed",12.0);
        wind.put("hasdirection",200.0);
        temperature.put("hasvalue",12.0);
        relativehumidity.put("hasvalue",0.2);
        cloudcover.put("hascloudcovervalue",1.0);
        precipation.put("hasintensity",50);

        weatherInJSON.put("haswind",wind);
        weatherInJSON.put("hasexteriortemperature",temperature);
        weatherInJSON.put("hashumidity",relativehumidity);
        weatherInJSON.put("hasprecipation",precipation);
        weatherInJSON.put("hascloudcover",cloudcover);

        agent.writeMetFile(weatherInJSON,fullPath);
        File metFile= new File(fullPath+"/test.met");
        Assert.assertTrue(metFile.exists());//check if the file is created
        Assert.assertTrue(metFile.length()>0);//check if there is data inside the file

        /*
            Generate bkg file in the appropriate directory
            by calling the writeBkgFile method.
            Check if indeed the bgd file was generated
         */

        //create mock region object
        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",834498.5457081277);
        upperCorner.put("uppery",817460.3860207011);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",833044.9253603141);
        lowerCorner.put("lowery",816015.5674630373);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","testSrsName");

        agent.writeBkgFile(fullPath);
        File bgdFile= new File(fullPath+"/testbackgrnd.bgd");
        Assert.assertTrue(bgdFile.exists());//check if the file is created
        Assert.assertTrue(bgdFile.length()>0);//check if there is data inside the file

        /*
            Generate apl file in the appropriate directory
            by calling the createEmissionInput method.
            Check if indeed an apl file was generated
         */

        //create mock stationiri array
        JSONArray stationIRI= new JSONArray();
        stationIRI.put("testStationIRI1");
        stationIRI.put("testStationIRI2");


        //create ship object
        JSONObject ship= new JSONObject();
        JSONObject collection= new JSONObject();
        JSONArray items= new JSONArray();
        JSONObject obj = new JSONObject();
        obj.put("mmsi","563009850");
        obj.put("lat","22.28822");
        obj.put("lon","114.15338");
        items.put(obj);
        collection.put("items",items);
        ship.put("collection",collection);

        JSONObject requestParams= new JSONObject();
        requestParams.put("region",region);
        requestParams.put("city","http://dbpedia.org/resource/Hong_Kong");
        requestParams.put("agent","testAgent");
        requestParams.put("stationiri",stationIRI);
        requestParams.put("ship",ship);
        requestParams.put("precipitation","50.0");
        String targetCRSName="EPSG:2326";


        //create mock building object
        JSONObject building= new JSONObject();
        JSONArray bldIRI= new JSONArray();
        bldIRI.put("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/HongkongDistrict02.owl#BuildingB09332fb1-0b21-4bca-a52c-c71f8cd0e5a1");
        building.put("BldIRI",bldIRI);
        JSONArray bldName= new JSONArray();
        bldName.put("a-a52c-c71f8cd0e5a1");
        building.put("BldName",bldName);
        JSONArray bldType= new JSONArray();
        bldType.put(0);
        building.put("BldType",bldType);
        JSONArray bldX= new JSONArray();
        bldX.put(30283.28271214908);
        building.put("BldX",bldX);
        JSONArray bldY= new JSONArray();
        bldY.put(816155.3357251927);
        building.put("BldY",bldY);
        JSONArray bldHeight= new JSONArray();
        bldHeight.put(130.79999999999998);
        building.put("BldHeight",bldHeight);
        JSONArray bldLength= new JSONArray();
        bldLength.put(16.278820596099706);
        building.put("BldLength",bldLength);
        JSONArray bldWidth= new JSONArray();
        bldWidth.put(17.392230495361243);
        building.put("BldWidth",bldWidth);
        JSONArray bldAngle= new JSONArray();
        bldAngle.put(42.510447078000844);
        building.put("BldAngle",bldAngle);

        String buildingInString= building.toString().replace("\"","'");

        agent.createEmissionInput(requestParams, buildingInString,region, targetCRSName, fullPath);
        File aplFile= new File(fullPath+"/test.apl");
        Assert.assertTrue(aplFile.exists());//check if the file is created
        Assert.assertTrue(aplFile.length()>0);//check if there is data inside the file

        /*
            Once all the input files have been generated
            call the executeModel method and check if a gst file was
            generated
         */
        agent.executeModel(fullPath);
        File gstFile= new File(fullPath+"/test.levels.gst");
        Assert.assertTrue(gstFile.exists());//check if the file is created
        Assert.assertTrue(gstFile.length()>0);//check if there is data inside the file
    }

    //case where key:"plant" is present
    @Test
    public void testExecuteModel2() throws IOException {
        ADMSAgent agent = new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String fullPath= tempFolder1.getPath();
        /*
        Strategy for the testing the method:
        Firstly, generate all the input files needed to
        run ADMS. Verify if the files are present in the
        directory. Finally, run executeModel in the very same
        directory and see if any output file is generated.
         */


        /*
            Generate met file in the appropriate directory
            by calling the writeMetFile method.
            Check if indeed the met file was generated
         */

        //create weather data in JSON format
        JSONObject weatherInJSON= new JSONObject();
        JSONObject wind = new JSONObject();
        JSONObject temperature = new JSONObject();
        JSONObject relativehumidity= new JSONObject();
        JSONObject cloudcover= new JSONObject();
        JSONObject precipation = new JSONObject();

        wind.put("hasspeed",12.0);
        wind.put("hasdirection",200.0);
        temperature.put("hasvalue",12.0);
        relativehumidity.put("hasvalue",0.2);
        cloudcover.put("hascloudcovervalue",1.0);
        precipation.put("hasintensity",50);

        weatherInJSON.put("haswind",wind);
        weatherInJSON.put("hasexteriortemperature",temperature);
        weatherInJSON.put("hashumidity",relativehumidity);
        weatherInJSON.put("hasprecipation",precipation);
        weatherInJSON.put("hascloudcover",cloudcover);

        agent.writeMetFile(weatherInJSON,fullPath);
        File metFile= new File(fullPath+"/test.met");
        Assert.assertTrue(metFile.exists());//check if the file is created
        Assert.assertTrue(metFile.length()>0);//check if there is data inside the file

        /*
            Generate bkg file in the appropriate directory
            by calling the writeBkgFile method.
            Check if indeed the bgd file was generated
         */

        //create mock region object
        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",834498.5457081277);
        upperCorner.put("uppery",817460.3860207011);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",833044.9253603141);
        lowerCorner.put("lowery",816015.5674630373);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","testSrsName");

        agent.writeBkgFile(fullPath);
        File bgdFile= new File(fullPath+"/testbackgrnd.bgd");
        Assert.assertTrue(bgdFile.exists());//check if the file is created
        Assert.assertTrue(bgdFile.length()>0);//check if there is data inside the file

        /*
            Generate apl file in the appropriate directory
            by calling the createEmissionInput method.
            Check if indeed an apl file was generated
         */

        //create mock stationiri array
        JSONArray stationIRI= new JSONArray();
        stationIRI.put("testStationIRI1");
        stationIRI.put("testStationIRI2");

        JSONObject requestParams= new JSONObject();
        requestParams.put("region",region);
        requestParams.put("city","http://dbpedia.org/resource/Hong_Kong");
        requestParams.put("agent","testAgent");
        requestParams.put("stationiri",stationIRI);
        requestParams.put("plant","myTestPlantIRI");
        requestParams.put("precipitation","50.0");
        String targetCRSName="EPSG:2326";


        //create mock building object
        JSONObject building= new JSONObject();
        JSONArray bldIRI= new JSONArray();
        bldIRI.put("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/HongkongDistrict02.owl#BuildingB09332fb1-0b21-4bca-a52c-c71f8cd0e5a1");
        building.put("BldIRI",bldIRI);
        JSONArray bldName= new JSONArray();
        bldName.put("a-a52c-c71f8cd0e5a1");
        building.put("BldName",bldName);
        JSONArray bldType= new JSONArray();
        bldType.put(0);
        building.put("BldType",bldType);
        JSONArray bldX= new JSONArray();
        bldX.put(30283.28271214908);
        building.put("BldX",bldX);
        JSONArray bldY= new JSONArray();
        bldY.put(816155.3357251927);
        building.put("BldY",bldY);
        JSONArray bldHeight= new JSONArray();
        bldHeight.put(130.79999999999998);
        building.put("BldHeight",bldHeight);
        JSONArray bldLength= new JSONArray();
        bldLength.put(16.278820596099706);
        building.put("BldLength",bldLength);
        JSONArray bldWidth= new JSONArray();
        bldWidth.put(17.392230495361243);
        building.put("BldWidth",bldWidth);
        JSONArray bldAngle= new JSONArray();
        bldAngle.put(42.510447078000844);
        building.put("BldAngle",bldAngle);

        String buildingInString= building.toString().replace("\"","'");


        agent.createEmissionInput(requestParams, buildingInString,region, targetCRSName, fullPath);
        File aplFile= new File(fullPath+"/test.apl");
        Assert.assertTrue(aplFile.exists());//check if the file is created
        Assert.assertTrue(aplFile.length()>0);//check if there is data inside the file

        /*
            Once all the input files have been generated
            call the executeModel method and check if a gst file was
            generated
         */
        agent.executeModel(fullPath);
        File gstFile= new File(fullPath+"/test.levels.gst");
        Assert.assertTrue(gstFile.exists());//check if the file is created
        Assert.assertTrue(gstFile.length()>0);//check if there is data inside the file
    }

    @Test
    public void testProcessRequestParameters() throws IOException {
        ADMSAgent admsAgent = new ADMSAgent();

        //create temp folder
        File tempFolder1 = folder.newFolder( "tempFolder");
        String dataPath= tempFolder1.getPath();

        File tempFolder2 = folder.newFolder( "tempFolder/JPS_ADMS");
        String fullPath=tempFolder2.getPath().replace("\\JPS_ADMS","/JPS_ADMS");

        JSONObject requestParams= new JSONObject();

        //create mock region object
        JSONObject region = new JSONObject();
        JSONObject upperCorner= new JSONObject();
        upperCorner.put("upperx",834498.5457081277);
        upperCorner.put("uppery",817460.3860207011);
        JSONObject lowerCorner= new JSONObject();
        lowerCorner.put("lowerx",833044.9253603141);
        lowerCorner.put("lowery",816015.5674630373);
        region.put("uppercorner",upperCorner);
        region.put("lowercorner",lowerCorner);
        region.put("srsname","EPSG:3857");

        //create mock stationiri array
        JSONArray stationIRI= new JSONArray();
        stationIRI.put("testStationIRI1");
        stationIRI.put("testStationIRI2");

        //create ship object
        JSONObject ship= new JSONObject();
        JSONObject collection= new JSONObject();
        JSONArray items= new JSONArray();
        JSONObject obj = new JSONObject();
        obj.put("mmsi","563009850");
        obj.put("lat","22.28822");
        obj.put("lon","114.15338");
        items.put(obj);
        collection.put("items",items);
        ship.put("collection",collection);

        requestParams.put("region",region);
        requestParams.put("city","http://dbpedia.org/resource/Hong_Kong");
        requestParams.put("agent","testAgent");
        requestParams.put("stationiri",stationIRI);
        requestParams.put("ship",ship);
        requestParams.put("precipitation","50.0");

        String target = fullPath + "/test.levels.gst";
        List<String> topics = new ArrayList<String>();
        topics.add(requestParams.getString("city"));
        String agent= requestParams.getString("agent");
        JSONObject expected= new JSONObject();
        expected.put("folder",fullPath);

        try(MockedStatic<QueryBroker> qb = Mockito.mockStatic(QueryBroker.class)){
           try(MockedStatic<MetaDataAnnotator> mda = Mockito.mockStatic(MetaDataAnnotator.class)){
                qb.when(()->QueryBroker.getLocalDataPath()).thenReturn(dataPath);
                mda.when(()-> MetaDataAnnotator.annotate(target, null, agent, true, topics)).thenAnswer((Answer<Void>) invocation -> null);
                JSONObject actual=(JSONObject) admsAgent.processRequestParameters(requestParams);
                Assert.assertEquals(expected.toString(),actual.toString());
           }
        }
    }
}
