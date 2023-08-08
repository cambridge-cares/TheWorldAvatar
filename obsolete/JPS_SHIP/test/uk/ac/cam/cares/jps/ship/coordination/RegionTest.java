package uk.ac.cam.cares.jps.ship.coordination;

import org.json.JSONObject;
import org.junit.Assert;
import uk.ac.cam.cares.jps.coordination.Region;
import org.junit.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class RegionTest {

    @Test
    public void testGetScope() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Region region = new Region();
        Method getScope= region.getClass().getDeclaredMethod("getScope", int.class);
        getScope.setAccessible(true);

        //case 1
        JSONObject expected= new JSONObject();
        expected.put("srsname","EPSG:3857");
        JSONObject lowercorner= new JSONObject();
        lowercorner.put("lowerx","11560879.832");
        lowercorner.put("lowery","140107.739");
        JSONObject uppercorner= new JSONObject();
        uppercorner.put("upperx","11564077.989");
        uppercorner.put("uppery","143305.896");
        expected.put("lowercorner",lowercorner);
        expected.put("uppercorner",uppercorner);
        JSONObject actual=(JSONObject) getScope.invoke(region,1);
        Assert.assertEquals(expected.toString(),actual.toString());
        //case 2
        lowercorner.put("lowerx","11552101.832");
        lowercorner.put("lowery","131707.739");
        uppercorner.put("upperx","11572101.89");
        uppercorner.put("uppery","151860.32");

        actual=(JSONObject) getScope.invoke(region,2);
        Assert.assertEquals(expected.toString(),actual.toString());
        //case 3
        lowercorner.put("lowerx","12706653.262");
        lowercorner.put("lowery","2545200.172");
        uppercorner.put("upperx","12711879.81");
        uppercorner.put("uppery","2550426.72");

        actual=(JSONObject) getScope.invoke(region,3);
        Assert.assertEquals(expected.toString(),actual.toString());
        //case 4
        lowercorner.put("lowerx","12694101.21");
        lowercorner.put("lowery","2534900.06");
        uppercorner.put("upperx","12720578.56");
        uppercorner.put("uppery","2562555.26");

        actual=(JSONObject) getScope.invoke(region,4);
        Assert.assertEquals(expected.toString(),actual.toString());
        //case 5
        lowercorner.put("lowerx","237044.13");
        lowercorner.put("lowery","44991.65");
        uppercorner.put("upperx","257044.13");
        uppercorner.put("uppery","64991.65");
        expected.put("srsname","EPSG:27700");

        actual=(JSONObject) getScope.invoke(region,5);
        Assert.assertEquals(expected.toString(),actual.toString());
    }

    @Test
    public void testPutRegion() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Region region = new Region();
        Method putRegion= region.getClass().getDeclaredMethod("putRegion", org.json.JSONObject.class, int.class);
        putRegion.setAccessible(true);

        JSONObject expected= new JSONObject();
        JSONObject jsonObject= new JSONObject();
        jsonObject.put("srsname","EPSG:3857");
        JSONObject lowercorner= new JSONObject();
        lowercorner.put("lowerx","11560879.832");
        lowercorner.put("lowery","140107.739");
        JSONObject uppercorner= new JSONObject();
        uppercorner.put("upperx","11564077.989");
        uppercorner.put("uppery","143305.896");
        jsonObject.put("lowercorner",lowercorner);
        jsonObject.put("uppercorner",uppercorner);

        expected.put("region",jsonObject);
        JSONObject obj= new JSONObject();
        putRegion.invoke("region",obj,1);
        Assert.assertEquals(expected.toString(),obj.toString());
    }

    @Test
    public void testPutRegionAndStation() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Region region = new Region();
        Method putRegionAndStation= region.getClass().getDeclaredMethod("putRegionAndStation", JSONObject.class, int.class);
        putRegionAndStation.setAccessible(true);
        //case 1
        JSONObject expected= new JSONObject();
        JSONObject jsonObject= new JSONObject();
        jsonObject.put("srsname","EPSG:3857");
        JSONObject lowercorner= new JSONObject();
        lowercorner.put("lowerx","11560879.832");
        lowercorner.put("lowery","140107.739");
        JSONObject uppercorner= new JSONObject();
        uppercorner.put("upperx","11564077.989");
        uppercorner.put("uppery","143305.896");
        jsonObject.put("lowercorner",lowercorner);
        jsonObject.put("uppercorner",uppercorner);

        expected.put("region",jsonObject);
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
        expected.put("city","http://dbpedia.org/resource/Singapore");

        JSONObject obj= new JSONObject();
        putRegionAndStation.invoke(region,obj, 1);
        Assert.assertEquals(expected.toString(),obj.toString());

        //case 2
        lowercorner.put("lowerx","11552101.832");
        lowercorner.put("lowery","131707.739");
        uppercorner.put("upperx","11572101.89");
        uppercorner.put("uppery","151860.32");
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
        obj= new JSONObject();
        putRegionAndStation.invoke(region,obj, 2);
        Assert.assertEquals(expected.toString(),obj.toString());

        //case3
        lowercorner.put("lowerx","12706653.262");
        lowercorner.put("lowery","2545200.172");
        uppercorner.put("upperx","12711879.81");
        uppercorner.put("uppery","2550426.72");
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-001.owl#AirQualityStation-001");
        expected.put("city","http://dbpedia.org/resource/Hong_Kong");
        obj= new JSONObject();
        putRegionAndStation.invoke(region,obj, 3);
        Assert.assertEquals(expected.toString(),obj.toString());

        //case 4
        lowercorner.put("lowerx","12694101.21");
        lowercorner.put("lowery","2534900.06");
        uppercorner.put("upperx","12720578.56");
        uppercorner.put("uppery","2562555.26");
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
        obj= new JSONObject();
        putRegionAndStation.invoke(region,obj, 4);
        Assert.assertEquals(expected.toString(),obj.toString());

        //case 5
        lowercorner.put("lowerx","237044.13");
        lowercorner.put("lowery","44991.65");
        uppercorner.put("upperx","257044.13");
        uppercorner.put("uppery","64991.65");
        jsonObject.put("srsname","EPSG:27700");
        expected.put("city","http://dbpedia.org/resource/Plymouth");

        obj= new JSONObject();
        putRegionAndStation.invoke(region,obj, 5);
        Assert.assertEquals(expected.toString(),obj.toString());
    }

    @Test
    public void testGetTargetCRSName(){
        //case Berlin
        String cityIRI="http://dbpedia.org/resource/Berlin";
        String expected="EPSG:25833";
        String actual= Region.getTargetCRSName(null,cityIRI);
        Assert.assertEquals(expected,actual);
        //case TheHague
        cityIRI="http://dbpedia.org/resource/The_Hague";
        expected="EPSG:28992";
        actual= Region.getTargetCRSName(null,cityIRI);
        Assert.assertEquals(expected,actual);
        //case Singapore with ADMS Agent
        cityIRI="http://dbpedia.org/resource/Singapore";
        String agentIRI="http://agentName/ADMS";
        expected="EPSG:3414";
        actual= Region.getTargetCRSName(agentIRI,cityIRI);
        Assert.assertEquals(expected,actual);
        //case Singapore with EPISODE Agent
        agentIRI="http://agentName/EPISODE";
        expected="EPSG:32648";
        actual= Region.getTargetCRSName(agentIRI,cityIRI);
        Assert.assertEquals(expected,actual);
        //case HongKong with ADMS Agent
        cityIRI="http://dbpedia.org/resource/Hong_Kong";
        agentIRI="http://agentName/ADMS";
        expected="EPSG:2326";
        actual= Region.getTargetCRSName(agentIRI,cityIRI);
        Assert.assertEquals(expected,actual);
        //case HongKong with EPISODE Agent
        agentIRI="http://agentName/EPISODE";
        expected="EPSG:32650";
        actual= Region.getTargetCRSName(agentIRI,cityIRI);
        Assert.assertEquals(expected,actual);
        //case Plymouth
        cityIRI="http://dbpedia.org/resource/Plymouth";
        expected="EPSG:32630";
        actual= Region.getTargetCRSName(null,cityIRI);
        Assert.assertEquals(expected,actual);
    }

    @Test
    public void testGetSRTM(){
        //case Singapore
        String cityIRI="http://dbpedia.org/resource/Singapore";
        List<String> expected = new ArrayList();
        expected.add("N01E103");
        expected.add("N01E104");
        List<String>actual=Region.getSRTM(cityIRI);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i< actual.size();i++){
            Assert.assertEquals(expected.get(i),actual.get(i));
        }
        //case HongKong
        cityIRI="http://dbpedia.org/resource/Hong_Kong";
        expected.remove(1);
        expected.remove(0);
        actual.remove(1);
        actual.remove(0);

        expected.add("N22E114");
        actual=Region.getSRTM(cityIRI);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i< actual.size();i++){
            Assert.assertEquals(expected.get(i),actual.get(i));
        }
        //case Plymouth
        cityIRI="http://dbpedia.org/resource/Plymouth";
        expected.remove(0);
        actual.remove(0);

        expected.add("N50W005");
        actual=Region.getSRTM(cityIRI);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i< actual.size();i++){
            Assert.assertEquals(expected.get(i),actual.get(i));
        }
    }
}
