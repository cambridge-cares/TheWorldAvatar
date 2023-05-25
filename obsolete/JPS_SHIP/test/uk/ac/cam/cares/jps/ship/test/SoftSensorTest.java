package uk.ac.cam.cares.jps.ship.test;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.ship.SoftSensor;

import javax.ws.rs.BadRequestException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class SoftSensorTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testCheckAgent() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkAgent= softSensor.getClass().getDeclaredMethod("checkAgent", JSONObject.class);
        checkAgent.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkAgent.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("agent","");
        val= checkAgent.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("agent",JSONObject.NULL);
        val= checkAgent.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        requestParams.put("agent","testAgent");
        val= checkAgent.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckCity() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkCity= softSensor.getClass().getDeclaredMethod("checkCity", JSONObject.class);
        checkCity.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkCity.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("cityname","");
        val=checkCity.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("cityname",JSONObject.NULL);
        val= checkCity.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.put("cityname","Singapore");
        val= checkCity.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckTime() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkTime= softSensor.getClass().getDeclaredMethod("checkTime", JSONObject.class);
        checkTime.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkTime.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        JSONObject timeinterval=new JSONObject();
        requestParams.put("timeinterval",timeinterval);
        val=checkTime.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("timeinterval",JSONObject.NULL);
        val= checkTime.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        timeinterval.put("to","23:59");
        timeinterval.put("from","00:00");
        requestParams.put("timeinterval",timeinterval);
        val= checkTime.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);

    }

    @Test
    public void testCheckToAndFrom() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkToAndFrom= softSensor.getClass().getDeclaredMethod("checkTime", JSONObject.class);
        checkToAndFrom.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject time = new JSONObject();
        requestParams.put("timeinterval",time);
        String val= checkToAndFrom.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        time.put("to","");
        time.put("from","00:00");
        requestParams.put("timeinterval",time);
        val=checkToAndFrom.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        time.put("to","23:59");
        time.put("from","");
        requestParams.put("timeinterval",time);
        val=checkToAndFrom.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        time.put("to",JSONObject.NULL);
        time.put("from","00:00");
        requestParams.put("timeinterval",time);
        val=checkToAndFrom.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        time.put("to","23:59");
        time.put("from",JSONObject.NULL);
        requestParams.put("timeinterval",time);
        val=checkToAndFrom.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        time.put("to","23:59");
        time.put("from","00:00");
        requestParams.put("timeinterval",time);
        val= checkToAndFrom.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);

    }

    @Test
    public void testCheckCoordinateList() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkCoordinateList= softSensor.getClass().getDeclaredMethod("checkCoordinateList", JSONObject.class);
        checkCoordinateList.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkCoordinateList.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        JSONArray coordinates=new JSONArray();
        requestParams.put("coordinates",coordinates);
        val=checkCoordinateList.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("coordinates",JSONObject.NULL);
        val= checkCoordinateList.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid a non empty JSON Array
        coordinates.put(0,223);
        coordinates.put(1,225);
        requestParams.put("coordinates",coordinates);
        val= checkCoordinateList.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckX() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkX= softSensor.getClass().getDeclaredMethod("checkX", JSONObject.class);
        checkX.setAccessible(true);

        //check case with missing key
        JSONObject requestParams= new JSONObject();
        JSONArray coordinates=new JSONArray();
        requestParams.put("coordinates",coordinates);
        JSONObject j1= new JSONObject();
        j1.put("xx","23");
        coordinates.put(j1);
        String val= checkX.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        j1.remove("xx");
        //check case with empty string value

        j1.put("x","");
        coordinates.put(j1);
        val=checkX.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        j1.put("x",JSONObject.NULL);
        coordinates.put(j1);
        val= checkX.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid a key:value pair
        j1.put("x","123");
        coordinates.put(j1);
        requestParams.put("coordinates",coordinates);
        val= checkX.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckY() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkY= softSensor.getClass().getDeclaredMethod("checkY", JSONObject.class);
        checkY.setAccessible(true);

        //check case with missing key
        JSONObject requestParams= new JSONObject();
        JSONArray coordinates=new JSONArray();
        requestParams.put("coordinates",coordinates);
        JSONObject j1= new JSONObject();
        j1.put("yy","23");
        coordinates.put(j1);
        String val= checkY.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        j1.remove("yy");
        //check case with empty string value

        j1.put("y","");
        coordinates.put(j1);
        val=checkY.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        j1.put("y",JSONObject.NULL);
        coordinates.put(j1);
        val= checkY.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid a key:value pair
        j1.put("y","123");
        coordinates.put(j1);
        requestParams.put("coordinates",coordinates);
        val= checkY.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckZ() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method checkZ= softSensor.getClass().getDeclaredMethod("checkZ", JSONObject.class);
        checkZ.setAccessible(true);

        //check case with missing key
        JSONObject requestParams= new JSONObject();
        JSONArray coordinates=new JSONArray();
        requestParams.put("coordinates",coordinates);
        JSONObject j1= new JSONObject();
        j1.put("zz","23");
        coordinates.put(j1);
        String val= checkZ.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        j1.remove("zz");
        //check case with empty string value

        j1.put("z","");
        coordinates.put(j1);
        val=checkZ.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        j1.put("z",JSONObject.NULL);
        coordinates.put(j1);
        val= checkZ.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid a key:value pair
        j1.put("z","123");
        coordinates.put(j1);
        requestParams.put("coordinates",coordinates);
        val= checkZ.invoke(softSensor,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testValidateInput(){
        SoftSensor softSensor= new SoftSensor();

        //check case with no key
        JSONObject requestParams= new JSONObject();
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam is empty.",e.getMessage());
        }

        //check case where keys are null
        // 1) key:agent is null
        requestParams.put("agent",JSONObject.NULL);
        requestParams.put("cityname","Singapore");
        JSONObject time= new JSONObject();
        time.put("to","23:59");
        time.put("from","00:00");
        JSONArray coordinates= new JSONArray();
        JSONObject j1= new JSONObject();
        j1.put("x",221);
        j1.put("y",223);
        j1.put("z",224);
        coordinates.put(j1);
        requestParams.put("coordinates",coordinates);
        requestParams.put("timeinterval",time);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:agent is missing or is null or is empty.",e.getMessage());
        }
        //2)key:cityname is null
        requestParams.put("agent","testAgent");
        requestParams.put("cityname",JSONObject.NULL);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:city is missing or is null or is empty.",e.getMessage());
        }
        //3)key:timeInterval is null
        //First key:to is null
        requestParams.put("cityname","Singapore");
        time.put("to",JSONObject.NULL);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:timeinterval is missing or is null or is empty or similar problems are present" +
                                        "for key:to and/or key:from found within the key:timeinterval object.",e.getMessage());
        }
        //Second key:from is null
        time.put("to","23:59");
        time.put("from",JSONObject.NULL);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:timeinterval is missing or is null or is empty or similar problems are present"+
                                        "for key:to and/or key:from found within the key:timeinterval object.",e.getMessage());
        }
        //4)key:coordinates is null
        time.put("from","00:00");
        //x is null
        j1.put("x",JSONObject.NULL);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the coordinates array the key:x is either not present, null or empty.",e.getMessage());
        }
        //y is null
        j1.put("x",221);
        j1.put("y",JSONObject.NULL);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the coordinates array the key:y is either not present, null or empty.",e.getMessage());
        }
        //z is null
        j1.put("y",222);
        j1.put("z",JSONObject.NULL);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the coordinates array the key:z is either not present, null or empty.",e.getMessage());
        }
        //final case where coordinate list is null
        j1.remove("x");
        j1.remove("y");
        j1.remove("z");

        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:coordinates is missing or is null or is empty.",e.getMessage());
        }
        //check case where keys are empty
        //1) key:agent is empty
        requestParams.put("agent","");
        requestParams.put("cityname","Singapore");

        time.put("to","23:59");
        time.put("from","00:00");

        coordinates.put(j1);
        requestParams.put("coordinates",coordinates);
        requestParams.put("timeinterval",time);
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:agent is missing or is null or is empty.",e.getMessage());
        }
        //2)key:cityname is empty
        requestParams.put("agent","testAgent");
        requestParams.put("cityname","");
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:city is missing or is null or is empty.",e.getMessage());
        }
        //3)key:timeinterval is empty
        //First key:to is empty
        requestParams.put("cityname","Singapore");
        time.put("to","");
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:timeinterval is missing or is null or is empty or similar problems are present"+
                                        "for key:to and/or key:from found within the key:timeinterval object.",e.getMessage());
        }
        //Second key:from is empty
        time.put("to","23:59");
        time.put("from","");
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:timeinterval is missing or is null or is empty or similar problems are present"+
                    "for key:to and/or key:from found within the key:timeinterval object.",e.getMessage());
        }
        //4)key:coordinates is empty
        time.put("from","00:00");
        //x is empty
        j1.put("x","");
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the coordinates array the key:x is either not present, null or empty.",e.getMessage());
        }
        //y is empty
        j1.put("x",221);
        j1.put("y","");
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the coordinates array the key:y is either not present, null or empty.",e.getMessage());
        }
        //z is empty
        j1.put("y",222);
        j1.put("z","");
        try{
            softSensor.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the coordinates array the key:z is either not present, null or empty.",e.getMessage());
        }
    }

    @Test
    public void testConstructContent() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method constructContent= softSensor.getClass().getDeclaredMethod("constructContent", String.class, double.class, double.class, double.class, List.class, int.class);
        constructContent.setAccessible(true);

        String timeinst="just a general timestamp";
        double x=2.0;
        double y=5.0;
        double z=6.0;
        List<String> concentration= new ArrayList<String>();
        concentration.add("conc1\\|conc1Name\\|conc1Value");
        concentration.add("testConcentration");

        String expected[] = new String[9];
        expected[0] = timeinst;
        expected[1] = "" + x;
        expected[2] = "" + y;
        expected[3] = "" + z;
        expected[4] = "EPSG:2326";
        expected[5] = concentration.get(0).split("\\|")[2]; // later need to be mapped to iri
        expected[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        expected[7] = concentration.get(1);
        expected[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
        String actual[]=(String[]) constructContent.invoke(softSensor,timeinst,x,y,z,concentration,0);

        Assert.assertEquals(expected.length,actual.length);
        for (int i=0;i<expected.length;i++){
            Assert.assertEquals(expected[i],actual[i]);
        }
    }

    @Test
    public void testConstructContentUpdated() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method constructContentUpdated= softSensor.getClass().getDeclaredMethod("constructContentUpdated", String.class, double.class, double.class, double.class, double.class, double.class);
        constructContentUpdated.setAccessible(true);

        String timeinst="just a general timestamp";
        double x=2.0;
        double y=5.0;
        double z=6.0;
        double sumpm10=2.5;
        double sumpm25=3.5;

        String expected[] = new String[9];
        expected[0] = timeinst;
        expected[1] = "" + x;
        expected[2] = "" + y;
        expected[3] = "" + z;
        expected[4] = "EPSG:2326";
        expected[5] = "PM10"; // later need to be mapped to iri
        expected[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        expected[7] = String.valueOf(sumpm10 + sumpm25);
        expected[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
        String actual[]=(String[]) constructContentUpdated.invoke(softSensor,timeinst,x,y,z,sumpm10,sumpm25);
        Assert.assertEquals(expected.length,actual.length);
        for (int i=0;i<expected.length;i++){
            Assert.assertEquals(expected[i],actual[i]);
        }
    }

    @Test
    public void testConstructContent2() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method constructContent2= softSensor.getClass().getDeclaredMethod("constructContent2", String.class, double.class, double.class, double.class, double.class);
        constructContent2.setAccessible(true);

        String timeinst="just a general timestamp";
        double x=2.0;
        double y=5.0;
        double z=6.0;
        double sumpm25=3.5;

        String expected[] = new String[9];
        expected[0] = timeinst;
        expected[1] = "" + x;
        expected[2] = "" + y;
        expected[3] = "" + z;
        expected[4] = "EPSG:2326";
        expected[5] = "PM2.5"; // later need to be mapped to iri
        expected[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        expected[7] = "" + sumpm25;
        expected[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
        String actual[]=(String[]) constructContent2.invoke(softSensor,timeinst,x,y,z,sumpm25);
        Assert.assertEquals(expected.length,actual.length);
        for (int i=0;i<expected.length;i++){
            Assert.assertEquals(expected[i],actual[i]);
        }
    }

    @Test
    public void testAddContentToCSV() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method addContentToCSV= softSensor.getClass().getDeclaredMethod("addContentToCSV", double.class, double.class, double.class, String.class, List.class, List.class);
        addContentToCSV.setAccessible(true);

        String timeinst="just a general timestamp";
        double x=2.0;
        double y=5.0;
        double z=6.0;
        List<String[]> propercsv= new ArrayList<>();

        //Case where PM2.5 is zero
        List<String> concentration= new ArrayList<String>();
        concentration.add("concentration\\|particulateMatterName\\|PM2.5Value");
        concentration.add("unknown");

        List<String[]>expected=new ArrayList<>();

        double sumpm10 = 0;
        double sumpm25 = 0;

        String content[] = new String[9];
        content[0] = timeinst;
        content[1] = "" + x;
        content[2] = "" + y;
        content[3] = "" + z;
        content[4] = "EPSG:2326";
        content[5] = "PM10"; // later need to be mapped to iri
        content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content[7] = String.valueOf(sumpm10 + sumpm25);
        content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        String content2[] = new String[9];
        content2[0] = timeinst;
        content2[1] = "" + x;
        content2[2] = "" + y;
        content2[3] = "" + z;
        content2[4] = "EPSG:2326";
        content2[5] = "PM2.5"; // later need to be mapped to iri
        content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content2[7] = "" + sumpm25;
        content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        expected.add(content);
        expected.add(content2);

        List<String[]>actual=(List<String[]>)addContentToCSV.invoke(softSensor,x,y,z,timeinst,concentration,propercsv);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            for (int j=0;j<actual.get(i).length;j++){
                Assert.assertEquals(expected.get(i)[j],actual.get(i)[j]);
            }
        }

        //Case where PM10 is zero
        concentration.remove(0);
        concentration.add(0,"concentration\\|particulateMatterName\\|PM10Value");
        actual.remove(0);
        actual.remove(0);

        actual=(List<String[]>)addContentToCSV.invoke(softSensor,x,y,z,timeinst,concentration,propercsv);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            for (int j=0;j<actual.get(i).length;j++){
                Assert.assertEquals(expected.get(i)[j],actual.get(i)[j]);
            }
        }

        //Case where the concentrations are not PM related
        concentration.remove(1);
        concentration.remove(0);

        concentration.add(0,"concentration\\|pollutantName\\|SO2");
        concentration.add(1,"4.5");

        actual.remove(1);
        actual.remove(0);

        content[0] = timeinst;
        content[1] = "" + x;
        content[2] = "" + y;
        content[3] = "" + z;
        content[4] = "EPSG:2326";
        content[5] = concentration.get(0).split("\\|")[2]; // later need to be mapped to iri
        content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content[7] = concentration.get(1);
        content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        expected.remove(1);
        expected.remove(0);
        expected.add(content);

        String []content1= new String[9];
        content1[0] = timeinst;
        content1[1] = "" + x;
        content1[2] = "" + y;
        content1[3] = "" + z;
        content1[4] = "EPSG:2326";
        content1[5] = "PM10"; // later need to be mapped to iri
        content1[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content1[7] = String.valueOf(sumpm10 + sumpm25);
        content1[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        expected.add(content1);
        expected.add(content2);

        actual=(List<String[]>)addContentToCSV.invoke(softSensor,x,y,z,timeinst,concentration,propercsv);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            for (int j=0;j<actual.get(i).length;j++){
                Assert.assertEquals(expected.get(i)[j],actual.get(i)[j]);
            }
        }
    }

    @Test
    public void testConvertArrayToStringMethod(){
        double x=0.1;
        double y=0.2;
        double z=0.3;
        String timeinst="timestamp";
        double sumpm25=1.4;

        String content2[] = new String[9];
        content2[0] = timeinst;
        content2[1] = "" + x;
        content2[2] = "" + y;
        content2[3] = "" + z;
        content2[4] = "EPSG:2326";
        content2[5] = "PM2.5"; // later need to be mapped to iri
        content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content2[7] = "" + sumpm25;
        content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        String expected=content2[0]+","+content2[1]+","+content2[2]+","+content2[3]+","+content2[4]+","+content2[5]+","+content2[6]+","+content2[7]+","+content2[8];
        String actual=(String) SoftSensor.convertArrayToStringMethod(content2);
        Assert.assertEquals(expected,actual);
    }

    @Test
    public void testCreateDataSet() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor= new SoftSensor();
        Method createDataSet= softSensor.getClass().getDeclaredMethod("createDataSet", List.class);
        createDataSet.setAccessible(true);

        double x=0.1;
        double y=0.2;
        double z=0.3;
        String timeinst="timestamp";
        double sumpm25=1.4;

        String content2[] = new String[9];
        content2[0] = timeinst;
        content2[1] = "" + x;
        content2[2] = "" + y;
        content2[3] = "" + z;
        content2[4] = "EPSG:2326";
        content2[5] = "PM2.5"; // later need to be mapped to iri
        content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content2[7] = "" + sumpm25;
        content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        List<String[]> propercsv= new ArrayList<>();
        propercsv.add(content2);
        String str="{\"head\":{\"vars\":[\"timestamp\",\"0.1\",\"0.2\",\"0.3\",\"EPSG:2326\",\"PM2.5\",\"http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration\",\"1.4\",\"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m\"]},\"results\":{\"bindings\":[]}}";
        JSONObject expected= new JSONObject(str);

        JSONObject actual=(JSONObject) createDataSet.invoke(softSensor,propercsv);
        Assert.assertEquals(expected.toString(),actual.toString());
    }

    @Test
    //case where fromSim Time is before fromRdf4J
    public void testGetMetadata1() throws NoSuchMethodException, ParseException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();
        Method getMetaData = softSensor.getClass().getDeclaredMethod("getMetadata", MediaType.class, String.class, String.class, String.class, String.class, String.class, String.class, List.class);
        getMetaData.setAccessible(true);

        String iriCreatingAgent = "testIriCreatingAgent";

        String fromSimulationTime = "2019-12-10T01:00:00";
        String toSimulationTime = "2019-12-10T02:00:00";


        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        Date fromRdf4J = formatter.parse("2019-12-10T01:57:26");
        Date from = formatter.parse(fromSimulationTime);
        Date to = formatter.parse(toSimulationTime);
        String[] keys = new String[1];
        keys[0] = "testKey";
        List<String[]> expected = new ArrayList<>();
        String[] arr = new String[1];
        arr[0] = "TestComplete!";
        expected.add(arr);

        String metaDataResult = "testMetaResult";
        try (MockedStatic<MetaDataQuery> mtq = Mockito.mockStatic(MetaDataQuery.class)) {
            try (MockedStatic<JenaResultSetFormatter> jenaResultSetFormatter = Mockito.mockStatic(JenaResultSetFormatter.class)) {
                mtq.when(() -> MetaDataQuery.queryOldResources(iriCreatingAgent, fromSimulationTime, toSimulationTime, null)).thenReturn(metaDataResult);
                jenaResultSetFormatter.when(() -> JenaResultSetFormatter.getKeys(metaDataResult)).thenReturn(keys);
                jenaResultSetFormatter.when(() -> JenaResultSetFormatter.convertToListofStringArrays(metaDataResult, keys)).thenReturn(expected);
                List<String[]> actual = (List<String[]>) getMetaData.invoke(softSensor, null, null, null,
                        iriCreatingAgent, fromSimulationTime, toSimulationTime, null, null);
                Assert.assertEquals(expected.size(), actual.size());
                for (int i = 0; i < actual.size(); i++) {
                    for (int j = 0; j < actual.get(i).length; j++) {
                        Assert.assertEquals(expected.get(i)[j], actual.get(i)[j]);
                    }
                }
            }
        }
    }

    @Test
    //case where both fromSim Time and toSim Time are after fromRdf4J
    public void testGetMetaData2()throws NoSuchMethodException, ParseException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();
        Method getMetaData = softSensor.getClass().getDeclaredMethod("getMetadata", MediaType.class, String.class, String.class, String.class, String.class, String.class, String.class, List.class);
        getMetaData.setAccessible(true);

        String iriCreatingAgent = "testIriCreatingAgent";

        String fromSimulationTime="2019-12-10T02:00:00";
        String toSimulationTime="2019-12-10T03:00:00";
        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        Date fromRdf4J = formatter.parse("2019-12-10T01:57:26");

        Date from = formatter.parse(fromSimulationTime);
        Date to = formatter.parse(toSimulationTime);
        String[] keys = new String[1];
        keys[0] = "testKey";
        List<String[]> expected = new ArrayList<>();
        String[] arr = new String[1];
        arr[0] = "TestComplete!";
        expected.add(arr);

        String metaDataResult = "testMetaResult";
        try(MockedStatic<MetaDataQuery>mtq=Mockito.mockStatic(MetaDataQuery.class)){
            try (MockedStatic<JenaResultSetFormatter>jenaResultSetFormatter=Mockito.mockStatic(JenaResultSetFormatter.class)){
                mtq.when(()->MetaDataQuery.queryResources(null, null, null,iriCreatingAgent, fromSimulationTime, toSimulationTime,null,null)).thenReturn(metaDataResult);
                jenaResultSetFormatter.when(()->JenaResultSetFormatter.getKeys(metaDataResult)).thenReturn(keys);
                jenaResultSetFormatter.when(()->JenaResultSetFormatter.convertToListofStringArrays(metaDataResult,keys)).thenReturn(expected);
                List<String[]> actual= (List<String[]>) getMetaData.invoke(softSensor,null, null, null,
                        iriCreatingAgent,fromSimulationTime, toSimulationTime,  null, null);
                Assert.assertEquals(expected.size(),actual.size());
                for(int i=0;i<actual.size();i++){
                    for(int j=0; j<actual.get(i).length;j++) {
                        Assert.assertEquals(expected.get(i)[j], actual.get(i)[j]);
                    }
                }
            }
        }
    }

    @Test
    public void testFindTheConcentration() throws NoSuchFieldException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();

        double datanumberx=20;
        double datanumbery= 16;
        double datanumberz= .9;
        //first case testing if block

        String []arr0= new String[6];
        arr0[0]="PlaceHolder0";
        arr0[1]="The value of Z=.4 m delimitter";
        arr0[2]="PlaceHolder2";
        arr0[3]="The value of Z=.6 m delimitter";
        arr0[4]="PlaceHolder3";
        arr0[5]="The value of Z=.9 m delimitter";

        String []arr= new String[6];
        arr[0]="PlaceHolder0";
        arr[1]="PlaceHolder1";
        arr[2]="PlaceHolder2";
        arr[3]="PlaceHolder3";
        arr[4]="9";
        arr[5]="10";

        String []arr1= new String[6];
        arr1[0]="PlaceHolder0";
        arr1[1]="PlaceHolder1";
        arr1[2]="PlaceHolder2";
        arr1[3]="PlaceHolder3";
        arr1[4]="10";
        arr1[5]="10";

        String []arr2= new String[6];
        arr2[0]="PlaceHolder0";
        arr2[1]="PlaceHolder1";
        arr2[2]="PlaceHolder2";
        arr2[3]="PlaceHolder3";
        arr2[4]="15";
        arr2[5]="14";

        String []arr3= new String[6];
        arr3[0]="PlaceHolder0";
        arr3[1]="PlaceHolder1";
        arr3[2]="PlaceHolder2";
        arr3[3]="PlaceHolder3";
        arr3[4]="20.0";
        arr3[5]="16.0";

        String []arr4= new String[6];
        arr4[0]="PlaceHolder0";
        arr4[1]="PlaceHolder1";
        arr4[2]="PlaceHolder2";
        arr4[3]="PlaceHolder3";
        arr4[4]="25";
        arr4[5]="18";

        String []arr5= new String[6];
        arr5[0]="PlaceHolder0";
        arr5[1]="PlaceHolder1";
        arr5[2]="PlaceHolder2";
        arr5[3]="PlaceHolder3";
        arr5[4]="9";
        arr5[5]="20";

        List<String[]> simulationresults= new ArrayList<>();
        simulationresults.add(arr0);
        simulationresults.add(arr);
        simulationresults.add(arr1);
        simulationresults.add(arr2);
        simulationresults.add(arr3);
        simulationresults.add(arr4);
        simulationresults.add(arr5);

        List<String> expected = new ArrayList<String>();
        expected.add(simulationresults.get(0)[5]);
        expected.add(simulationresults.get(4)[5]);

        List<String>actual=softSensor.findtheconcentration(simulationresults,datanumberx,datanumbery,datanumberz);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i< actual.size();i++){
            Assert.assertEquals(expected.get(i),actual.get(i));
        }

        //second case testing else block
        Field zamount=softSensor.getClass().getDeclaredField("zamount");
        zamount.setAccessible(true);
        zamount.set(softSensor,3);
        datanumberx=-999.0;
        datanumbery=-999.0;
        datanumberz=-999.0;
        expected.remove(1);
        expected.remove(0);
        expected.add(simulationresults.get(0)[3]);
        expected.add("unknown");

        actual.remove(1);
        actual.remove(0);
        actual=softSensor.findtheconcentration(simulationresults,datanumberx,datanumbery,datanumberz);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i< actual.size();i++){
            Assert.assertEquals(expected.get(i),actual.get(i));
        }
    }

    @Test
    public void testClosestIndex(){
        SoftSensor softSensor= new SoftSensor();
        List<Double>group= new ArrayList<>();
        group.add(4d);
        group.add(6d);
        group.add(10d);
        group.add(14d);
        double number = 7;
        int expected=1;
        int actual=(int)softSensor.closestIndex(number,group);
        Assert.assertEquals(expected,actual);
    }

    @Test
    public void testConstructClosest() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();
        Method constructClosest= softSensor.getClass().getDeclaredMethod("constructClosest", double.class, double.class, double.class, List.class, List.class, List.class, double.class, double.class, double.class, double.class, double.class, double.class, List.class);
        constructClosest.setAccessible(true);

        List<Double>xgroup= new ArrayList<>();
        xgroup.add(4d);
        xgroup.add(6d);
        xgroup.add(10d);
        xgroup.add(14d);

        List<Double>ygroup= new ArrayList<>();
        ygroup.add(24d);
        ygroup.add(26d);
        ygroup.add(30d);
        ygroup.add(44d);

        List<Double>zgroup= new ArrayList<>();
        zgroup.add(.4d);
        zgroup.add(.6d);
        zgroup.add(.9d);
        zgroup.add(.2d);

        double numberx=11d;
        double numbery=22d;
        double numberz=.57d;

        List<String>closest= new ArrayList<>();
        List<String>expected= new ArrayList<>();
        expected.add("-999");
        expected.add("-999");
        expected.add("-999");

        //first case when the provided number is outside the minimum or maximum range
        double minx=12d;
        double maxx=20d;
        double miny=10d;
        double maxy=15d;
        double minz=.1d;
        double maxz=.3d;
        List<String>actual=(List<String>) constructClosest.invoke(softSensor,numberx, numbery, numberz, xgroup, ygroup, zgroup, minx, maxx, miny, maxy, minz, maxz,closest);
        Assert.assertEquals(expected.size(),actual.size());
        for(int i=0;i<actual.size();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }

        //second case when the provided number is within the minimum and maximum range
        minx=9d;
        maxx=20d;
        miny=10d;
        maxy=25d;
        minz=.1d;
        maxz=.7d;
        expected.remove(2);
        expected.remove(1);
        expected.remove(0);

        expected.add(""+xgroup.get(2));
        expected.add(""+ygroup.get(0));
        expected.add(""+zgroup.get(1));

        actual.remove(2);
        actual.remove(1);
        actual.remove(0);

        actual=(List<String>) constructClosest.invoke(softSensor,numberx, numbery, numberz, xgroup, ygroup, zgroup, minx, maxx, miny, maxy, minz, maxz,closest);
        Assert.assertEquals(expected.size(),actual.size());
        for(int i=0;i<actual.size();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }
    }

    @Test
    public void testAddToXGroup() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException{
        SoftSensor softSensor = new SoftSensor();
        Method addToXGroup= softSensor.getClass().getDeclaredMethod("addToXGroup", List.class, int.class, double.class, List.class);
        addToXGroup.setAccessible(true);

        List<Double>xGroup= new ArrayList<>();
        List<Double>expected= new ArrayList<>();
        expected.add(9d);
        expected.add(10d);
        expected.add(11d);

        double minx=12d;

        String []arr= new String[5];
        arr[0]="PlaceHolder0";
        arr[1]="PlaceHolder1";
        arr[2]="PlaceHolder2";
        arr[3]="PlaceHolder3";
        arr[4]="9";

        String []arr1= new String[5];
        arr1[0]="PlaceHolder0";
        arr1[1]="PlaceHolder1";
        arr1[2]="PlaceHolder2";
        arr1[3]="PlaceHolder3";
        arr1[4]="10";

        String []arr2= new String[5];
        arr2[0]="PlaceHolder0";
        arr2[1]="PlaceHolder1";
        arr2[2]="PlaceHolder2";
        arr2[3]="PlaceHolder3";
        arr2[4]="11";

        String []arr3= new String[5];
        arr3[0]="PlaceHolder0";
        arr3[1]="PlaceHolder1";
        arr3[2]="PlaceHolder2";
        arr3[3]="PlaceHolder3";
        arr3[4]="12";

        List<String[]> simulationResult= new ArrayList<>();
        simulationResult.add(arr);
        simulationResult.add(arr1);
        simulationResult.add(arr2);
        simulationResult.add(arr3);

        List<Double> actual=(List<Double>) addToXGroup.invoke(softSensor,simulationResult,0,minx,xGroup);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }
    }

    @Test
    public void testAddToYGroup() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();
        Method addToYGroup= softSensor.getClass().getDeclaredMethod("addToYGroup", List.class, double.class, List.class);
        addToYGroup.setAccessible(true);

        List<Double>yGroup= new ArrayList<>();
        List<Double>expected= new ArrayList<>();

        double miny=10d;

        String []arr0= new String[6];
        arr0[0]="PlaceHolder0";
        arr0[1]="PlaceHolder1";
        arr0[2]="PlaceHolder2";
        arr0[3]="PlaceHolder3";
        arr0[4]="8";
        arr0[5]="8";

        String []arr= new String[6];
        arr[0]="PlaceHolder0";
        arr[1]="PlaceHolder1";
        arr[2]="PlaceHolder2";
        arr[3]="PlaceHolder3";
        arr[4]="9";
        arr[5]="9";

        String []arr1= new String[6];
        arr1[0]="PlaceHolder0";
        arr1[1]="PlaceHolder1";
        arr1[2]="PlaceHolder2";
        arr1[3]="PlaceHolder3";
        arr1[4]="10";
        arr1[5]="10";

        String []arr2= new String[6];
        arr2[0]="PlaceHolder0";
        arr2[1]="PlaceHolder1";
        arr2[2]="PlaceHolder2";
        arr2[3]="PlaceHolder3";
        arr2[4]="10";
        arr2[5]="10";

        String []arr3= new String[6];
        arr3[0]="PlaceHolder0";
        arr3[1]="PlaceHolder1";
        arr3[2]="PlaceHolder2";
        arr3[3]="PlaceHolder3";
        arr3[4]="10";
        arr3[5]="10";

        String []arr4= new String[6];
        arr4[0]="PlaceHolder0";
        arr4[1]="PlaceHolder1";
        arr4[2]="PlaceHolder2";
        arr4[3]="PlaceHolder3";
        arr4[4]="12";
        arr4[5]="12";

        String []arr5= new String[6];
        arr5[0]="PlaceHolder0";
        arr5[1]="PlaceHolder1";
        arr5[2]="PlaceHolder2";
        arr5[3]="PlaceHolder3";
        arr5[4]="13";
        arr5[5]="13";

        List<String[]> simulationResult= new ArrayList<>();
        simulationResult.add(arr0);
        simulationResult.add(arr);
        simulationResult.add(arr1);
        simulationResult.add(arr2);
        simulationResult.add(arr3);
        simulationResult.add(arr4);
        simulationResult.add(arr5);

        expected.add(9d);
        expected.add(13d);

        List<Double> actual=(List<Double>) addToYGroup.invoke(softSensor,simulationResult,miny,yGroup);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }
    }

    @Test
    public void testConstructZGroup() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();
        Method constructZGroup= softSensor.getClass().getDeclaredMethod("constructZGroup", int.class, List.class, List.class);
        constructZGroup.setAccessible(true);

        List<Double>zGroup= new ArrayList<>();
        List<Double>expected= new ArrayList<>();
        List<String[]>simulationResult= new ArrayList<>();

        String []arr0= new String[11];
        arr0[0]="PlaceHolder0";
        arr0[1]="PlaceHolder1";
        arr0[2]="PlaceHolder2";
        arr0[3]="PlaceHolder3";
        arr0[4]="8";
        arr0[5]="8";
        arr0[6]="The value of Z=.4 m delimitter";
        arr0[7]="PlaceHolder4";
        arr0[8]="The value of Z=.6 m delimitter";
        arr0[9]="PlaceHolder5";
        arr0[10]="The value of Z=.9 m delimitter";
        simulationResult.add(arr0);

        expected.add(.4d);
        expected.add(.6d);
        expected.add(.9d);

        List<Double> actual=(List<Double>) constructZGroup.invoke(softSensor,simulationResult.get(0).length,simulationResult,zGroup);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }
    }

    @Test
    public void testFindTheClosest() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        SoftSensor softSensor = new SoftSensor();
        Method findtheclosest= softSensor.getClass().getDeclaredMethod("findtheclosest", List.class, double.class, double.class, double.class);
        findtheclosest.setAccessible(true);

        String []arr0= new String[6];
        arr0[0]="PlaceHolder0";
        arr0[1]="The value of Z=.4 m delimitter";
        arr0[2]="PlaceHolder2";
        arr0[3]="The value of Z=.6 m delimitter";
        arr0[4]="PlaceHolder3";
        arr0[5]="The value of Z=.9 m delimitter";

        String []arr= new String[6];
        arr[0]="PlaceHolder0";
        arr[1]="PlaceHolder1";
        arr[2]="PlaceHolder2";
        arr[3]="PlaceHolder3";
        arr[4]="9";
        arr[5]="10";

        String []arr1= new String[6];
        arr1[0]="PlaceHolder0";
        arr1[1]="PlaceHolder1";
        arr1[2]="PlaceHolder2";
        arr1[3]="PlaceHolder3";
        arr1[4]="10";
        arr1[5]="10";

        String []arr2= new String[6];
        arr2[0]="PlaceHolder0";
        arr2[1]="PlaceHolder1";
        arr2[2]="PlaceHolder2";
        arr2[3]="PlaceHolder3";
        arr2[4]="15";
        arr2[5]="14";

        String []arr3= new String[6];
        arr3[0]="PlaceHolder0";
        arr3[1]="PlaceHolder1";
        arr3[2]="PlaceHolder2";
        arr3[3]="PlaceHolder3";
        arr3[4]="20";
        arr3[5]="16";

        String []arr4= new String[6];
        arr4[0]="PlaceHolder0";
        arr4[1]="PlaceHolder1";
        arr4[2]="PlaceHolder2";
        arr4[3]="PlaceHolder3";
        arr4[4]="25";
        arr4[5]="18";

        String []arr5= new String[6];
        arr5[0]="PlaceHolder0";
        arr5[1]="PlaceHolder1";
        arr5[2]="PlaceHolder2";
        arr5[3]="PlaceHolder3";
        arr5[4]="9";
        arr5[5]="20";

        List<String[]> simulationresults= new ArrayList<>();
        simulationresults.add(arr0);
        simulationresults.add(arr);
        simulationresults.add(arr1);
        simulationresults.add(arr2);
        simulationresults.add(arr3);
        simulationresults.add(arr4);
        simulationresults.add(arr5);

        double x=21d;
        double y=11d;
        double z=0.7d;

        List<String> expected= new ArrayList<>();
        expected.add(""+20d);
        expected.add(""+10d);
        expected.add(""+.6d);

        List<String>actual=(List<String>) findtheclosest.invoke(softSensor,simulationresults,x,y,z);
        Assert.assertEquals(expected.size(),actual.size());
        for(int i=0;i<actual.size();i++){
            Assert.assertEquals(expected.get(i).toString(),actual.get(i).toString());
        }
    }

    @Test
    public void testAddConcentrationToCSV() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        SoftSensor softSensor= new SoftSensor();
        Method addConcentrationToCSV=softSensor.getClass().getDeclaredMethod("addConcentrationToCSV", int.class, JSONArray.class, List.class, List.class, List.class);
        addConcentrationToCSV.setAccessible(true);

        Field logger=softSensor.getClass().getDeclaredField("logger");
        logger.setAccessible(true);
        logger.set(softSensor, LoggerFactory.getLogger(softSensor.getClass()));

        JSONArray coordinatelist= new JSONArray();
        JSONObject j0= new JSONObject();
        j0.put("x",21.0);
        j0.put("y",11d);
        j0.put("z",0.7);
        coordinatelist.put(j0);

        String []arr0= new String[6];
        arr0[0]="PlaceHolder0";
        arr0[1]="The value of Z=.4 m delimitter";
        arr0[2]="PlaceHolder2";
        arr0[3]="The value of Z=.6 m delimitter conc1\\|conc1Name\\|conc1Value";
        arr0[4]="PlaceHolder3";
        arr0[5]="The value of Z=.9 m delimitter";

        String []arr= new String[6];
        arr[0]="PlaceHolder0";
        arr[1]="PlaceHolder1";
        arr[2]="PlaceHolder2";
        arr[3]="PlaceHolder3";
        arr[4]="9";
        arr[5]="10";

        String []arr1= new String[6];
        arr1[0]="PlaceHolder0";
        arr1[1]="PlaceHolder1";
        arr1[2]="PlaceHolder2";
        arr1[3]="PlaceHolder3";
        arr1[4]="10";
        arr1[5]="10";

        String []arr2= new String[6];
        arr2[0]="PlaceHolder0";
        arr2[1]="PlaceHolder1";
        arr2[2]="PlaceHolder2";
        arr2[3]="PlaceHolder3";
        arr2[4]="15";
        arr2[5]="14";

        String []arr3= new String[6];
        arr3[0]="PlaceHolder0";
        arr3[1]="PlaceHolder1";
        arr3[2]="PlaceHolder2";
        arr3[3]="PlaceHolder3";
        arr3[4]="20";
        arr3[5]="16";

        String []arr4= new String[6];
        arr4[0]="PlaceHolder0";
        arr4[1]="PlaceHolder1";
        arr4[2]="PlaceHolder2";
        arr4[3]="PlaceHolder3";
        arr4[4]="25";
        arr4[5]="18";

        String []arr5= new String[6];
        arr5[0]="PlaceHolder0";
        arr5[1]="PlaceHolder1";
        arr5[2]="PlaceHolder2";
        arr5[3]="PlaceHolder3";
        arr5[4]="9";
        arr5[5]="20";

        List<String[]> simulationresults= new ArrayList<>();
        simulationresults.add(arr0);
        simulationresults.add(arr);
        simulationresults.add(arr1);
        simulationresults.add(arr2);
        simulationresults.add(arr3);
        simulationresults.add(arr4);
        simulationresults.add(arr5);

        List<String[]> listmap= new ArrayList<>();
        String[] array= new String[5];
        array[0]="filename";
        array[1]="Placeholder";
        array[2]="Placeholder";
        array[3]="Placeholder";
        array[4]="Sample time instant";

        listmap.add(array);
        List<String[]> propercsv= new ArrayList<>();

        List<String[]> expected= new ArrayList<>();
        String content[] = new String[9];
        content[0] = listmap.get(0)[4];
        content[1] = "" + 21.0;
        content[2] = "" + 11.0;
        content[3] = "" + 0.7;
        content[4] = "EPSG:2326";
        content[5] = arr0[3].split("\\|")[2]; // later need to be mapped to iri
        content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content[7] = arr0[4];
        content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        double sumpm10=0.0;
        double sumpm25=0.0;
        String content1[] = new String[9];
        content1[0] = listmap.get(0)[4];
        content1[1] = "" + 21.0;
        content1[2] = "" + 11.0;
        content1[3] = "" + 0.7;
        content1[4] = "EPSG:2326";
        content1[5] = "PM10"; // later need to be mapped to iri
        content1[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content1[7] = String.valueOf(sumpm10 + sumpm25);
        content1[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        String content2[] = new String[9];
        content2[0] = listmap.get(0)[4];
        content2[1] = "" + 21.0;
        content2[2] = "" + 11.0;
        content2[3] = "" + 0.7;
        content2[4] = "EPSG:2326";
        content2[5] = "PM2.5"; // later need to be mapped to iri
        content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content2[7] = "" + sumpm25;
        content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";

        expected.add(content);
        expected.add(content1);
        expected.add(content2);

        List<String[]> actual=(List<String[]>)addConcentrationToCSV.invoke(softSensor,0,coordinatelist,simulationresults,listmap,propercsv);
        Assert.assertEquals(expected.size(),actual.size());
        for (int i=0;i<actual.size();i++){
            for (int j=0;j<actual.get(i).length;j++){
                Assert.assertEquals(expected.get(i)[j],actual.get(i)[j]);
            }
        }
    }

    @Test
    public void testProcessRequestParameters() throws IOException {
        SoftSensor softSensor= new SoftSensor();

        /**
         * create requestParams JSONObject
         */
        String iriCreatingAgent="testIriCreatingAgent";

        String fromSimulationTime="2019-12-10T01:00:00";
        String toSimulationTime="2019-12-10T02:00:00";
        List<String> topics = new ArrayList<String>();
        topics.add("http://dbpedia.org/resource/Singapore");

        JSONArray coordinatelist= new JSONArray();
        JSONObject j0= new JSONObject();
        j0.put("x",21.0);
        j0.put("y",11d);
        j0.put("z",0.7);
        coordinatelist.put(j0);
        JSONObject j1= new JSONObject();
        j1.put("to",toSimulationTime);
        j1.put("from",fromSimulationTime);
        JSONObject requestParams= new JSONObject();
        requestParams.put("timeinterval",j1);
        requestParams.put("agent",iriCreatingAgent);
        requestParams.put("cityname","Singapore");
        requestParams.put("coordinates",coordinatelist);

        /**
         *need to create a temporary file and write to it in order to enter the
         *if (name.exists() && name.length() != 0) block
         */
        File tempFile = folder.newFile("filename");
        FileUtils.writeStringToFile(tempFile, "testString");

        /**
         * create List<String[]>listmap which contains the filename
         */
        String metaDataResult="testMetaDataResult";
        String [] keys= new String[1];
        keys[0]="testKey";
        List<String[]> listmap= new ArrayList<>();
        String[] array= new String[5];
        array[0]=tempFile.toString();
        array[1]="Placeholder";
        array[2]="Placeholder";
        array[3]="Placeholder";
        array[4]="Sample time instant";
        listmap.add(array);

        /**
         * Create List<String[]> simulationresults
         */
        String []arr0= new String[6];
        arr0[0]="PlaceHolder0";
        arr0[1]="The value of Z=.4 m delimitter";
        arr0[2]="PlaceHolder2";
        arr0[3]="The value of Z=.6 m delimitter conc1\\|conc1Name\\|conc1Value";
        arr0[4]="PlaceHolder3";
        arr0[5]="The value of Z=.9 m delimitter";

        String []arr= new String[6];
        arr[0]="PlaceHolder0";
        arr[1]="PlaceHolder1";
        arr[2]="PlaceHolder2";
        arr[3]="PlaceHolder3";
        arr[4]="9";
        arr[5]="10";

        String []arr1= new String[6];
        arr1[0]="PlaceHolder0";
        arr1[1]="PlaceHolder1";
        arr1[2]="PlaceHolder2";
        arr1[3]="PlaceHolder3";
        arr1[4]="10";
        arr1[5]="10";

        String []arr2= new String[6];
        arr2[0]="PlaceHolder0";
        arr2[1]="PlaceHolder1";
        arr2[2]="PlaceHolder2";
        arr2[3]="PlaceHolder3";
        arr2[4]="15";
        arr2[5]="14";

        String []arr3= new String[6];
        arr3[0]="PlaceHolder0";
        arr3[1]="PlaceHolder1";
        arr3[2]="PlaceHolder2";
        arr3[3]="PlaceHolder3";
        arr3[4]="20";
        arr3[5]="16";

        String []arr4= new String[6];
        arr4[0]="PlaceHolder0";
        arr4[1]="PlaceHolder1";
        arr4[2]="PlaceHolder2";
        arr4[3]="PlaceHolder3";
        arr4[4]="25";
        arr4[5]="18";

        String []arr5= new String[6];
        arr5[0]="PlaceHolder0";
        arr5[1]="PlaceHolder1";
        arr5[2]="PlaceHolder2";
        arr5[3]="PlaceHolder3";
        arr5[4]="9";
        arr5[5]="20";

        List<String[]> simulationresults= new ArrayList<>();
        simulationresults.add(arr0);
        simulationresults.add(arr);
        simulationresults.add(arr1);
        simulationresults.add(arr2);
        simulationresults.add(arr3);
        simulationresults.add(arr4);
        simulationresults.add(arr5);


        /**
         * create expected JSONObject
         *
         */
        JSONObject expected= new JSONObject();

        JSONObject observes= new JSONObject();
        observes.put("type","uri");
        observes.put("value","http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration");
        JSONObject xx= new JSONObject();
        xx.put("type","xsd:number");
        xx.put("value","21.0");
        JSONObject yy= new JSONObject();
        yy.put("type","xsd:number");
        yy.put("value","11.0");
        JSONObject zz= new JSONObject();
        zz.put("type","xsd:number");
        zz.put("value","0.7");
        JSONObject pol1= new JSONObject();
        pol1.put("type","literal");
        pol1.put("value","conc1Value");
        JSONObject pol2= new JSONObject();
        pol2.put("type","literal");
        pol2.put("value","PM10");
        JSONObject pol3= new JSONObject();
        pol3.put("type","literal");
        pol3.put("value","PM2.5");
        JSONObject time= new JSONObject();
        time.put("type","xsd:dateTime");
        time.put("value","Sample time instant");
        JSONObject unit= new JSONObject();
        unit.put("type","uri");
        unit.put("value","http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m");
        JSONObject crs= new JSONObject();
        crs.put("type","literal");
        crs.put("value","EPSG:2326");
        JSONObject value1= new JSONObject();
        value1.put("type","xsd:number");
        value1.put("value","PlaceHolder3");
        JSONObject value2= new JSONObject();
        value2.put("type","xsd:number");
        value2.put("value","0.0");

        JSONObject jj1= new JSONObject();
        jj1.put("observes",observes);
        jj1.put("x",xx);
        jj1.put("y",yy);
        jj1.put("z",zz);
        jj1.put("crs",crs);
        jj1.put("time",time);
        jj1.put("value",value1);
        jj1.put("unit",unit);
        jj1.put("pollutant",pol1);
        JSONObject jj2= new JSONObject();
        jj2.put("observes",observes);
        jj2.put("x",xx);
        jj2.put("y",yy);
        jj2.put("z",zz);
        jj2.put("crs",crs);
        jj2.put("time",time);
        jj2.put("value",value2);
        jj2.put("unit",unit);
        jj2.put("pollutant",pol2);
        JSONObject jj3= new JSONObject();
        jj3.put("observes",observes);
        jj3.put("x",xx);
        jj3.put("y",yy);
        jj3.put("z",zz);
        jj3.put("crs",crs);
        jj3.put("time",time);
        jj3.put("value",value2);
        jj3.put("unit",unit);
        jj3.put("pollutant",pol3);
        JSONArray bindings= new JSONArray();
        bindings.put(jj1);
        bindings.put(jj2);
        bindings.put(jj3);
        JSONObject results= new JSONObject();
        results.put("bindings",bindings);
        expected.put("results",results);
        JSONArray var = new JSONArray();
        var.put("time");
        var.put("x");
        var.put("y");
        var.put("z");
        var.put("crs");
        var.put("pollutant");
        var.put("observes");
        var.put("value");
        var.put("unit");
        JSONObject head = new JSONObject();
        head.put("vars",var);
        expected.put("head",head);

        try(MockedStatic<MetaDataQuery>mtq=Mockito.mockStatic(MetaDataQuery.class)){
            try (MockedStatic<JenaResultSetFormatter>jenaResultSetFormatter=Mockito.mockStatic(JenaResultSetFormatter.class)){
                mtq.when(()->MetaDataQuery.queryOldResources(iriCreatingAgent,fromSimulationTime, toSimulationTime,topics)).thenReturn(metaDataResult);
                jenaResultSetFormatter.when(()->JenaResultSetFormatter.getKeys(metaDataResult)).thenReturn(keys);
                jenaResultSetFormatter.when(()->JenaResultSetFormatter.convertToListofStringArrays(metaDataResult,keys)).thenReturn(listmap);
                MockedStatic<FileUtil> fileUtils= Mockito.mockStatic(FileUtil.class);
                fileUtils.when(()->FileUtil.readFileLocally(tempFile.toString())).thenReturn(tempFile.toString());
                MockedStatic<MatrixConverter> matrixConv= Mockito.mockStatic(MatrixConverter.class);
                matrixConv.when(()->MatrixConverter.fromCsvToArray(tempFile.toString())).thenReturn(simulationresults);
                JSONObject actual= softSensor.processRequestParameters(requestParams);
                Assert.assertEquals(expected.toString(),actual.toString());
            }
        }
    }
}

