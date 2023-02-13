package uk.ac.cam.cares.jps.powsys.retrofit.test;

import org.json.JSONArray;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.powsys.retrofit.BatteryRetrofit;
import org.json.JSONObject;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;


public class TestBatteryRetrofit {
    private String ENIRI;
    private JSONArray batteryiris = new JSONArray();
    private List<String> batteryList = new ArrayList<>();

    @Before
    public void setUp(){
        ENIRI = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
        batteryiris.put("http://localhost:8080/jps/kb/f288d618-e936-448b-8582-57cfc1ca827f/sgp/jurongisland/jurongislandpowernetwork/VRB-059.owl#VRB-059")
                .put("http://localhost:8080/jps/kb/f288d618-e936-448b-8582-57cfc1ca827f/sgp/jurongisland/jurongislandpowernetwork/VRB-011.owl#VRB-011.owl");
        batteryList.add("http://localhost:8080/jps/kb/f288d618-e936-448b-8582-57cfc1ca827f/sgp/jurongisland/jurongislandpowernetwork/VRB-059.owl#VRB-059");
        batteryList.add("http://localhost:8080/jps/kb/f288d618-e936-448b-8582-57cfc1ca827f/sgp/jurongisland/jurongislandpowernetwork/VRB-011.owl#VRB-011.owl");
    }

    @Test
    public void testSetLogger() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.powsys.retrofit.BatteryRetrofit");
        Method setLoggerMethod = TargetClass.getDeclaredMethod("setLogger") ;
        BatteryRetrofit brClass = mock(BatteryRetrofit.class,CALLS_REAL_METHODS);
        setLoggerMethod.setAccessible(true);
        setLoggerMethod.invoke(brClass);

        Field logger = TargetClass.getDeclaredField("logger") ;
        logger.setAccessible(true);
        Logger log = (Logger) logger.get(brClass);
        assertNotNull(log);
        assertTrue(log.isErrorEnabled());
    }

    @Test
    public void testValidateInput() throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.powsys.retrofit.BatteryRetrofit");
        Method validateInputMethod = TargetClass.getDeclaredMethod("validateInput", JSONObject.class);

        JSONObject joValid = new JSONObject();
        joValid.put("electricalnetwork",ENIRI);
        joValid.put("batterylist", batteryiris);
        JSONObject joInvalid = new JSONObject();
        joInvalid.put("electricalnetwork", ENIRI + "@&@)#!(*");
        joInvalid.put("batterylist", batteryiris + "&(*%*$&^");

        BatteryRetrofit brClass = mock(BatteryRetrofit.class,CALLS_REAL_METHODS);
        validateInputMethod.setAccessible(true);
        assertFalse((boolean)validateInputMethod.invoke(brClass, joInvalid));
        assertTrue((boolean)validateInputMethod.invoke(brClass, joValid));
    }

    /**
     * This test does not run unless we expect JPSRuntimeException because we were unable to properly set up JPS_POWSYS
     */
    @Test(expected = JPSRuntimeException.class)
    public void testRetrofitEnergyStorage()  {
        //A JPSRuntimeException will be thrown here as a 404-NOT FOUND is returned after POST request via a valid input
        new BatteryRetrofit().retrofitEnergyStorage(ENIRI, batteryList);
    }

    /**
     * This test does not run unless we expect JPSRuntimeException because we were unable to properly set up JPS_POWSYS
     */
    @Test(expected = JPSRuntimeException.class)
    public void testProcessRequestParameters() {
        JSONObject joValid = new JSONObject();
        joValid.put("electricalnetwork",ENIRI);
        joValid.put("batterylist", batteryiris);
        new BatteryRetrofit().processRequestParameters(joValid);
    }

}