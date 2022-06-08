package uk.ac.cam.cares.jps.admsagent;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class ADMSAgentTest {

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
        Assert.assertEquals("false",val);

        //check case with empty value
        region.put("srsname","");
        requestParams.put("region",region);
        val= checkSrsname.invoke(admsAgent,requestParams).toString();
        Assert.assertEquals("false",val);

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
    public void testCheckItems(){

    }
}
