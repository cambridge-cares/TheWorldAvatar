package uk.ac.cam.cares.jps.ship.test;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.ship.SpeedLoadMapWrapper;
import static org.mockito.Mockito.*;


import java.io.File;
import javax.ws.rs.BadRequestException;
import java.io.*;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

public class SpeedLoadMapWrapperTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testCheckSpeed() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SpeedLoadMapWrapper speedLoadMapWrapper= new SpeedLoadMapWrapper();
        Method checkSpeed= speedLoadMapWrapper.getClass().getDeclaredMethod("checkSpeed", JSONObject.class);
        checkSpeed.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkSpeed.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null key
        requestParams.put("speed",JSONObject.NULL);
        val=checkSpeed.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty key
        requestParams.put("speed","");
        val=checkSpeed.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        requestParams.put("speed",23.d);
        val=checkSpeed.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckType() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        SpeedLoadMapWrapper speedLoadMapWrapper= new SpeedLoadMapWrapper();
        Method checkType= speedLoadMapWrapper.getClass().getDeclaredMethod("checkType", JSONObject.class);
        checkType.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkType.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null key
        requestParams.put("type",JSONObject.NULL);
        val=checkType.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty key
        requestParams.put("type","");
        val=checkType.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        requestParams.put("type","tanker");
        val=checkType.invoke(speedLoadMapWrapper,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testValidateInput(){
        SpeedLoadMapWrapper speedLoadMapWrapper= new SpeedLoadMapWrapper();

        //check case with no key
        JSONObject requestParams= new JSONObject();
        try{
            speedLoadMapWrapper.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam is empty.",e.getMessage());
        }
        //check case with null key
        requestParams.put("type","tanker");
        requestParams.put("speed",JSONObject.NULL);
        try{
            speedLoadMapWrapper.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:speed is not present or it is null or it is empty.",e.getMessage());
        }

        requestParams.put("type",JSONObject.NULL);
        requestParams.put("speed",23.2d);
        try{
            speedLoadMapWrapper.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:type is not present or it is null or it is empty.",e.getMessage());
        }

        //check case with empty case
        requestParams.put("type","tanker");
        requestParams.put("speed","");
        try{
            speedLoadMapWrapper.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:speed is not present or it is null or it is empty.",e.getMessage());
        }

        requestParams.put("type","");
        requestParams.put("speed",23.2d);
        try{
            speedLoadMapWrapper.validateInput(requestParams);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("In the requestParam object either the key:type is not present or it is null or it is empty.",e.getMessage());
        }

        //check valid key:value pair
        requestParams.put("type","tanker");
        requestParams.put("speed",23.2d);
        boolean validate= speedLoadMapWrapper.validateInput(requestParams);
        Assert.assertTrue(validate);
    }

    @Test
    //case where isWindows is false
    public void TestGetSurogateValues1() throws NoSuchMethodException, IOException ,InvocationTargetException, IllegalAccessException{
        SpeedLoadMapWrapper speedLoadMapWrapper= new SpeedLoadMapWrapper();
        Method getSurrogateValues= speedLoadMapWrapper.getClass().getDeclaredMethod("getSurogateValues", String.class);
        getSurrogateValues.setAccessible(true);

        String slmDir = "\\python\\ADMS-speed-load-map";
        String slmScript = "ADMS-Map-SpeedTorque-NOxSoot.py";
        String pypathUnix = "bin/python";
        String slmPyvenv = "speed.load.map.venv.dir";
        String venDir="\\place\\ven.dir";

        JSONObject in= new JSONObject();
        JSONObject speedob= new JSONObject();
        speedob.put("value", 1000); //600-2500
        speedob.put("unit", "RPM");

        JSONObject torob= new JSONObject();
        torob.put("value", 250); //50-550 range
        torob.put("unit", "Nm");
        in.put("speed", speedob);
        in.put("torque", torob);
        String inputs=in.toString();

        ArrayList<String> args= new ArrayList<>();

        File tempFolder1 = folder.newFolder( "tempFolder");
        //Even though tempFolder2 is not used it is needed for the system to detect path for the temporary file created below
        File tempFolder2= folder.newFolder("tempFolder","WEB-INF","classes","resources");

        Path venPath= Paths.get(tempFolder1.getPath(),venDir);

        //Create temporary file
        File tempFile = folder.newFile("tempFolder/WEB-INF/classes/resources/jpsship.properties");
        // Write something to it.
        FileWriter myWriter = new FileWriter(tempFile);
        myWriter.write(slmPyvenv+"=value");
        myWriter.close();

        List<String> prop= new ArrayList<>();
        String keyValue=slmPyvenv+"=testVal";
        prop.add(keyValue);

        InputStream is= new FileInputStream(tempFile);
        Properties jpsProperties = new Properties();
        jpsProperties.load(is);

        args.add(venPath.toString());
        args.add(slmScript);
        args.add(inputs);

        JSONObject expected= new JSONObject();
        expected.put("key1","value1");
        expected.put("key2","value2");
        String modifiedSlmWorkingDir=tempFolder1.getAbsolutePath()+slmDir.replace("\\", "/");//needed in order to pass as argument to Command Helper

        try(MockedStatic<AgentLocator>agentLocator= Mockito.mockStatic(AgentLocator.class)){
            try(MockedStatic<Paths> p= Mockito.mockStatic(Paths.class)){
                try(MockedStatic<CommandHelper> ch = Mockito.mockStatic(CommandHelper.class)){
                    agentLocator.when(() -> AgentLocator.getCurrentJpsAppDirectory(any())).thenReturn(tempFolder1.getAbsolutePath());
                    p.when(() -> Paths.get(jpsProperties.getProperty(slmPyvenv), pypathUnix)).thenReturn(venPath);
                    ch.when(() -> CommandHelper.executeCommands(modifiedSlmWorkingDir, args)).thenReturn(expected.toString());
                    Object actual= getSurrogateValues.invoke(speedLoadMapWrapper, inputs);
                    Assert.assertEquals(expected.toString(),actual.toString());
                }
            }
        }
    }

    @Test
    //case where isWindows is true
    public void TestGetSurogateValues2() throws NoSuchMethodException, IOException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        SpeedLoadMapWrapper speedLoadMapWrapper= new SpeedLoadMapWrapper();
        Method getSurrogateValues= speedLoadMapWrapper.getClass().getDeclaredMethod("getSurogateValues", String.class);
        getSurrogateValues.setAccessible(true);

        String slmDir = "\\python\\ADMS-speed-load-map";
        String slmScript = "ADMS-Map-SpeedTorque-NOxSoot.py";
        String pypathWindows = "\\Scripts\\python.exe";
        String slmPyvenv = "speed.load.map.venv.dir";
        String venDir="\\place\\ven.dir";

        JSONObject in= new JSONObject();
        JSONObject speedob= new JSONObject();
        speedob.put("value", 1000); //600-2500
        speedob.put("unit", "RPM");

        JSONObject torob= new JSONObject();
        torob.put("value", 250); //50-550 range
        torob.put("unit", "Nm");
        in.put("speed", speedob);
        in.put("torque", torob);
        String inputs=in.toString();

        ArrayList<String> args= new ArrayList<>();

        File tempFolder1 = folder.newFolder( "tempFolder");
        //Even though tempFolder2 is not used it is needed for the system to detect path for the temporary file created below
        File tempFolder2= folder.newFolder("tempFolder","WEB-INF","classes","resources");

        Path venPath= Paths.get(tempFolder1.getPath(),venDir);

        //Create temporary file
        File tempFile = folder.newFile("tempFolder\\WEB-INF\\classes\\resources\\jpsship.properties");
        // Write something to it.
        FileWriter myWriter = new FileWriter(tempFile);
        myWriter.write(slmPyvenv+"=value");
        myWriter.close();


        List<String> prop= new ArrayList<>();
        String keyValue=slmPyvenv+"=testVal";
        prop.add(keyValue);

        InputStream is= new FileInputStream(tempFile);
        Properties jpsProperties = new Properties();
        jpsProperties.load(is);

        args.add(venPath.toString());
        args.add(slmScript);
        args.add(inputs);

        JSONObject expected= new JSONObject();
        expected.put("key1","value1");
        expected.put("key2","value2");
        String modifiedSlmWorkingDir=tempFolder1.getAbsolutePath()+slmDir;

        try(MockedStatic<AgentLocator>agentLocator= Mockito.mockStatic(AgentLocator.class)){
            try(MockedStatic<Paths> p= Mockito.mockStatic(Paths.class)) {
                try (MockedStatic<CommandHelper> ch = Mockito.mockStatic(CommandHelper.class)) {
                    ch.when(() -> CommandHelper.isWindows()).thenReturn(true);
                    agentLocator.when(() -> AgentLocator.getCurrentJpsAppDirectory(any())).thenReturn(tempFolder1.getAbsolutePath());
                    p.when(() -> Paths.get(jpsProperties.getProperty(slmPyvenv), pypathWindows)).thenReturn(venPath);
                    ch.when(() -> CommandHelper.executeCommands(modifiedSlmWorkingDir, args)).thenReturn(expected.toString());
                    Object actual = getSurrogateValues.invoke(speedLoadMapWrapper, inputs);
                    Assert.assertEquals(expected.toString(), actual.toString());
                }
            }
        }
    }

    @Test
    public void TestConsiderShipTypeForMass() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SpeedLoadMapWrapper speedLoadMapWrapper = new SpeedLoadMapWrapper();
        Method considerShipTypeForMass= speedLoadMapWrapper.getClass().getDeclaredMethod("considerShipTypeForMass", String.class, double.class, JSONObject.class);
        considerShipTypeForMass.setAccessible(true);

        JSONObject jsonObject= new JSONObject();
        double oldvaluemixmass=2d;
        String type="cargo";

        JSONObject result= (JSONObject) considerShipTypeForMass.invoke(speedLoadMapWrapper,type,oldvaluemixmass,jsonObject);
        Assert.assertEquals(2*322,result.getDouble("value"),1e-6);

        type="tanker";
        result= (JSONObject)considerShipTypeForMass.invoke(speedLoadMapWrapper,type,oldvaluemixmass,jsonObject);
        Assert.assertEquals(2*430,result.getDouble("value"),1e-6);

        type="container";
        result= (JSONObject)considerShipTypeForMass.invoke(speedLoadMapWrapper,type,oldvaluemixmass,jsonObject);
        Assert.assertEquals(2*580,result.getDouble("value"),1e-6);

        type="passenger";
        result= (JSONObject)considerShipTypeForMass.invoke(speedLoadMapWrapper,type,oldvaluemixmass,jsonObject);
        Assert.assertEquals(2*697,result.getDouble("value"),1e-6);

        type="NavalCraft";
        result= (JSONObject)considerShipTypeForMass.invoke(speedLoadMapWrapper,type,oldvaluemixmass,jsonObject);
        Assert.assertEquals(2*300,result.getDouble("value"),1e-6);
    }

    @Test
    public void TestCrankUpRealShipModel() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        SpeedLoadMapWrapper speedLoadMapWrapper = new SpeedLoadMapWrapper();
        Method crankUpRealShipModel= speedLoadMapWrapper.getClass().getDeclaredMethod("crankUpRealShipModel",String.class,String.class);
        crankUpRealShipModel.setAccessible(true);

        JSONObject input= new JSONObject();

        //pollutants array
        JSONArray pollutants= new JSONArray();
        JSONObject jo1= new JSONObject();
        jo1.put("value","25");
        pollutants.put(jo1);

        //particle array
        JSONArray particle= new JSONArray();
        JSONObject emm1= new JSONObject();
        JSONObject diam1= new JSONObject();

        JSONObject jop1= new JSONObject();
        emm1.put("value","50");
        diam1.put("value","1000");
        jop1.put("emission_rate",emm1);
        jop1.put("diameter",diam1);
        particle.put(jop1);

        JSONObject mixture = new JSONObject();
        JSONObject massflux= new JSONObject();

        massflux.put("value","10");
        mixture.put("massflux",massflux);
        input.put("mixture",mixture);
        input.put("pollutants",pollutants);
        input.put("particle",particle);

        String type="cargo";
        JSONObject actual=(JSONObject)crankUpRealShipModel.invoke(speedLoadMapWrapper,type,input.toString());
        double actualPollutantValue= actual.getJSONArray("pollutants").getJSONObject(0).getDouble("value");
        double actualDiamterValue= actual.getJSONArray("particle").getJSONObject(0).getJSONObject("diameter").getDouble("value");
        double actualParicleEmissionRateValue= actual.getJSONArray("particle").getJSONObject(0).getJSONObject("emission_rate").getDouble("value");
        double actualMassFluxValue= actual.getJSONObject("mixture").getJSONObject("massflux").getDouble("value");
        Assert.assertEquals(25d*322,actualPollutantValue,1e-6);
        Assert.assertEquals(Math.round(1000*1000d)/1000d,actualDiamterValue,1e-6);
        Assert.assertEquals(50d*322,actualParicleEmissionRateValue,1e-6);
        Assert.assertEquals(10d*322,actualMassFluxValue,1e-6);
    }

    @Test
    public void testProcessRequestParameters() throws IOException {
        SpeedLoadMapWrapper speedLoadMapWrapper= new SpeedLoadMapWrapper();
        /**
         * Block associated with validateInput
         */
        JSONObject requestParams= new JSONObject();
        requestParams.put("type","cargo");
        requestParams.put("speed",23.2d);

        /**
         * Block associated with
         * getSurrogateValues method
         */
        String slmDir = "\\python\\ADMS-speed-load-map";
        String slmScript = "ADMS-Map-SpeedTorque-NOxSoot.py";
        String jpsShipProperties = "\\WEB-INF\\classes\\resources\\jpsship.properties";
        String pypathUnix = "bin/python";
        String pypathWindows = "\\Scripts\\python.exe";
        String slmPyvenv = "speed.load.map.venv.dir";
        String venDir="\\place\\ven.dir";

        JSONObject in= new JSONObject();
        JSONObject speedob= new JSONObject();
        speedob.put("value", (requestParams.getDouble("speed")*2500)/(58.1));
        speedob.put("unit", "RPM");

        JSONObject torob= new JSONObject();
        torob.put("value", 250); //50-550 range
        torob.put("unit", "Nm");
        in.put("speed", speedob);
        in.put("torque", torob);
        String inputs=in.toString();

        ArrayList<String> args= new ArrayList<>();

        File tempFolder1 = folder.newFolder( "tempFolder");
        //Even though tempFolder2 is not used it is needed for the system to detect path for the temporary file created below
        File tempFolder2= folder.newFolder("tempFolder","WEB-INF","classes","resources");

        Path venPath= Paths.get(tempFolder1.getPath(),venDir);
        //create temporary file
        File tempFile = folder.newFile("tempFolder/WEB-INF/classes/resources/jpsship.properties");
        // Write something to it.
        FileWriter myWriter = new FileWriter(tempFile);
        myWriter.write(slmPyvenv+"=value");
        myWriter.close();

        InputStream is= new FileInputStream(tempFile);
        Properties jpsProperties = new Properties();
        jpsProperties.load(is);

        args.add(venPath.toString());
        args.add(slmScript);
        args.add(inputs.replace("\"", "'"));// the replace method is used in order to match the arguments passed when getSurrogateValues is called in crankUpRealShipMethod

        String modifiedSlmWorkingDir=tempFolder1.getAbsolutePath()+slmDir.replace("\\", "/");//needed in order to pass as argument to Command Helper
        /**
         * Input to crankUpRealShipModel method
         */
        JSONObject input= new JSONObject();
        //pollutants array
        JSONArray pollutants= new JSONArray();
        JSONObject jo1= new JSONObject();
        jo1.put("value","25");
        pollutants.put(jo1);

        //particle array
        JSONArray particle= new JSONArray();
        JSONObject emm1= new JSONObject();
        JSONObject diam1= new JSONObject();

        JSONObject jop1= new JSONObject();
        emm1.put("value","50");
        diam1.put("value","1000");
        jop1.put("emission_rate",emm1);
        jop1.put("diameter",diam1);
        particle.put(jop1);

        JSONObject mixture = new JSONObject();
        JSONObject massflux= new JSONObject();

        massflux.put("value","10");
        mixture.put("massflux",massflux);
        input.put("mixture",mixture);
        input.put("pollutants",pollutants);
        input.put("particle",particle);

        try(MockedStatic<AgentLocator>agentLocator= Mockito.mockStatic(AgentLocator.class)){
            try(MockedStatic<Paths> p= Mockito.mockStatic(Paths.class)){
                try(MockedStatic<CommandHelper> ch = Mockito.mockStatic(CommandHelper.class)) {
                    agentLocator.when(() -> AgentLocator.getCurrentJpsAppDirectory(any())).thenReturn(tempFolder1.getAbsolutePath());
                    p.when(() -> Paths.get(jpsProperties.getProperty(slmPyvenv), pypathUnix)).thenReturn(venPath);
                    ch.when(() -> CommandHelper.executeCommands(modifiedSlmWorkingDir, args)).thenReturn(input.toString());
                    JSONObject actual = speedLoadMapWrapper.processRequestParameters(requestParams);
                    double actualPollutantValue = actual.getJSONArray("pollutants").getJSONObject(0).getDouble("value");
                    double actualDiamterValue = actual.getJSONArray("particle").getJSONObject(0).getJSONObject("diameter").getDouble("value");
                    double actualParicleEmissionRateValue = actual.getJSONArray("particle").getJSONObject(0).getJSONObject("emission_rate").getDouble("value");
                    double actualMassFluxValue = actual.getJSONObject("mixture").getJSONObject("massflux").getDouble("value");
                    Assert.assertEquals(25d * 322, actualPollutantValue, 1e-6);
                    Assert.assertEquals(Math.round(1000 * 1000d) / 1000d, actualDiamterValue, 1e-6);
                    Assert.assertEquals(50d * 322, actualParicleEmissionRateValue, 1e-6);
                    Assert.assertEquals(10d * 322, actualMassFluxValue, 1e-6);
                }
            }
        }
    }
}
