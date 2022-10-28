package uk.ac.cam.cares.jps.ship.coordination;

import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.coordination.CoordinationDataCollection;

import javax.ws.rs.BadRequestException;
import java.util.concurrent.ExecutionException;

public class CoordinationDataCollectionTest {

    @Test
    public void testValidateInput(){
        CoordinationDataCollection cdataCollection= new CoordinationDataCollection();
        //case when requestParams is empty
        JSONObject jsonObject = new JSONObject();
        try{
            cdataCollection.validateInput(jsonObject);
            Assert.fail();
        }catch(BadRequestException e){
            Assert.assertEquals("RequestParam is empty.",e.getMessage());
        }
        //case when key:scenarioname is missing
        jsonObject.put("key","value");
        try{
            cdataCollection.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam either does not contain key:scenarioname or key:scenarioname is null.",e.getMessage());
        }
        //case when key:scenarioname is null
        jsonObject.put("scenarioname",JSONObject.NULL);
        try{
            cdataCollection.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam either does not contain key:scenarioname or key:scenarioname is null.",e.getMessage());
        }
        //case when key:scenarioname is empty
        jsonObject.put("scenarioname","");
        try{
            cdataCollection.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("The value of key:scenarioname is empty.",e.getMessage());
        }
        //case with valid key:value pair
        jsonObject.put("scenarioname","testScenarioName");
        boolean validate=cdataCollection.validateInput(jsonObject);
        Assert.assertTrue(validate);
    }

    @Test
    public void testExecuteSGDataADMS() throws NoSuchMethodException {
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();

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
        expected.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");

        JSONObject actual=cDataCollection.executeSGDataADMS();
        Assert.assertEquals(expected.toString(),actual.toString());
    }

    @Test
    public void testExecuteSGDataEPISODE(){
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();

        JSONObject expected= new JSONObject();
        JSONObject jsonObject= new JSONObject();
        jsonObject.put("srsname","EPSG:3857");
        JSONObject lowercorner= new JSONObject();
        lowercorner.put("lowerx","11552101.832");
        lowercorner.put("lowery","131707.739");
        JSONObject uppercorner= new JSONObject();
        uppercorner.put("upperx","11572101.89");
        uppercorner.put("uppery","151860.32");
        jsonObject.put("lowercorner",lowercorner);
        jsonObject.put("uppercorner",uppercorner);

        expected.put("region",jsonObject);
        expected.put("city","http://dbpedia.org/resource/Singapore");
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
        expected.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");

        JSONObject actual=cDataCollection.executeSGDataEPISODE();
        Assert.assertEquals(expected.toString(),actual.toString());
    }

    @Test
    public void testExecuteHKDataADMS(){
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();
        JSONObject expected= new JSONObject();
        JSONObject jsonObject= new JSONObject();
        jsonObject.put("srsname","EPSG:3857");
        JSONObject lowercorner= new JSONObject();
        lowercorner.put("lowerx","12706653.262");
        lowercorner.put("lowery","2545200.172");
        JSONObject uppercorner= new JSONObject();
        uppercorner.put("upperx","12711879.81");
        uppercorner.put("uppery","2550426.72");
        jsonObject.put("lowercorner",lowercorner);
        jsonObject.put("uppercorner",uppercorner);

        expected.put("region",jsonObject);
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-001.owl#AirQualityStation-001");
        expected.put("city","http://dbpedia.org/resource/Hong_Kong");
        expected.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");

        JSONObject actual=cDataCollection.executeHKDataADMS();
        Assert.assertEquals(expected.toString(),actual.toString());
    }

    @Test
    public void testExecuteHKDataEPISODE(){
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();
        JSONObject expected= new JSONObject();
        JSONObject jsonObject= new JSONObject();
        jsonObject.put("srsname","EPSG:3857");
        JSONObject lowercorner= new JSONObject();
        lowercorner.put("lowerx","12706653.262");
        lowercorner.put("lowery","2545200.172");
        JSONObject uppercorner= new JSONObject();
        uppercorner.put("upperx","12711879.81");
        uppercorner.put("uppery","2550426.72");
        jsonObject.put("lowercorner",lowercorner);
        jsonObject.put("uppercorner",uppercorner);

        lowercorner.put("lowerx","12694101.21");
        lowercorner.put("lowery","2534900.06");
        uppercorner.put("upperx","12720578.56");
        uppercorner.put("uppery","2562555.26");
        expected.put("region",jsonObject);
        expected.put("airStationIRI","http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
        expected.put("city","http://dbpedia.org/resource/Hong_Kong");
        expected.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");

        JSONObject actual=cDataCollection.executeHKDataEPISODE();
        Assert.assertEquals(expected.toString(),actual.toString());
    }

    @Test
    //callAgent with one argument
    public void testCallAgent1(){
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();
        JSONObject obj= new JSONObject();
        obj.put("key1","value1");
        obj.put("key2","value2");
        JSONObject expected= new JSONObject();
        expected.put("key1","value1");
        expected.put("key2","value2");
        expected.put("reactionmechanism","none");
        String str="testString";
        try(MockedStatic<AgentCaller> agentCaller= Mockito.mockStatic(AgentCaller.class)){
            agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/dispersion/coordination",expected.toString())).thenReturn(str);
            cDataCollection.callAgent(obj);
            Assert.assertEquals(expected.toString(),obj.toString());
        }
    }

    @Test
    //callAgent with two arguments
    public void testCallAgent2() throws ExecutionException, InterruptedException{
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();
        JSONObject obj= new JSONObject();
        obj.put("key1","value1");
        obj.put("key2","value2");

        JSONObject obj1= new JSONObject();
        obj1.put("pet","dog");
        obj1.put("fruit","banana");

        JSONObject expected1= new JSONObject();
        expected1.put("key1","value1");
        expected1.put("key2","value2");
        expected1.put("reactionmechanism","none");

        JSONObject expected2= new JSONObject();
        expected2.put("pet","dog");
        expected2.put("fruit","banana");
        expected2.put("reactionmechanism","none");
        String str1="testString1";
        String str2="testString2";
        try(MockedStatic<AgentCaller> agentCaller= Mockito.mockStatic(AgentCaller.class)){
            agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/dispersion/coordination",expected2.toString())).thenReturn(str2);
            agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/adms/dispersion/coordination",expected1.toString())).thenReturn(str1);
            cDataCollection.callAgent(obj,obj1);
            Assert.assertEquals(expected1.toString(),obj.toString());
            Assert.assertEquals(expected2.toString(),obj1.toString());
        }
    }

    @Test
    public void testProcessRequestParameters(){
        CoordinationDataCollection cDataCollection= new CoordinationDataCollection();
        //create spy in order to monitor the number of times certain methods are invoked.
        CoordinationDataCollection spyDataColl = Mockito.spy(cDataCollection);

        JSONObject jsonObject = new JSONObject();
        jsonObject.put("scenarioname","testScenarioName");
        String scenarioName= jsonObject.optString("scenarioname");
        String scenarioUrl="basepath/scenarioValue/";

        JSONObject expected = new JSONObject();
        expected.put("scenarioname","testScenarioName");
        JSONObject jpsContext = new JSONObject();
        jpsContext.put("scenariourl",scenarioUrl);
        expected.put("jpscontext",jpsContext);

        String str1="testString1";
        String str2="testString2";

        JSONObject episode= new JSONObject();
        JSONObject jsonObject1= new JSONObject();
        jsonObject1.put("srsname","EPSG:3857");
        JSONObject lowercorner1= new JSONObject();
        lowercorner1.put("lowerx","11552101.832");
        lowercorner1.put("lowery","131707.739");
        JSONObject uppercorner1= new JSONObject();
        uppercorner1.put("upperx","11572101.89");
        uppercorner1.put("uppery","151860.32");
        jsonObject1.put("lowercorner",lowercorner1);
        jsonObject1.put("uppercorner",uppercorner1);

        episode.put("region",jsonObject1);
        episode.put("city","http://dbpedia.org/resource/Singapore");
        episode.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
        episode.put("agent", "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service");

        JSONObject adms= new JSONObject();
        JSONObject jsonObject2= new JSONObject();
        jsonObject2.put("srsname","EPSG:3857");
        JSONObject lowercorner2= new JSONObject();
        lowercorner2.put("lowerx","11560879.832");
        lowercorner2.put("lowery","140107.739");
        JSONObject uppercorner2= new JSONObject();
        uppercorner2.put("upperx","11564077.989");
        uppercorner2.put("uppery","143305.896");
        jsonObject2.put("lowercorner",lowercorner2);
        jsonObject2.put("uppercorner",uppercorner2);

        adms.put("region",jsonObject2);
        adms.put("airStationIRI","http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
        adms.put("city","http://dbpedia.org/resource/Singapore");
        adms.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");

        try(MockedStatic<BucketHelper>buckHelp=Mockito.mockStatic(BucketHelper.class)){
            try (MockedStatic<AgentCaller> agentCaller = Mockito.mockStatic(AgentCaller.class)) {
                buckHelp.when(()->BucketHelper.getScenarioUrl(scenarioName)).thenReturn(scenarioUrl);
                agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/dispersion/coordination",episode.toString())).thenReturn(str2);
                agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/adms/dispersion/coordination",adms.toString())).thenReturn(str1);
                JSONObject actual= spyDataColl.processRequestParameters(jsonObject);
                Mockito.verify(spyDataColl).executeSGDataADMS();
                Mockito.verify(spyDataColl).executeSGDataEPISODE();
                Assert.assertEquals(expected.toString(),actual.toString());
            }
        }
    }

}
