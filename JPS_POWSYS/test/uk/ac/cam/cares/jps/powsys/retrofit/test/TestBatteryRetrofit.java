package uk.ac.cam.cares.jps.powsys.retrofit.test;

import org.json.JSONArray;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.powsys.retrofit.BatteryRetrofit;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.powsys.retrofit.GeneralRetrofitAgent;

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
import static org.mockito.Mockito.*;


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
     * JPSRuntimeException is expected to be thrown as the unit test cannot be done without a complete setup of POWSYS
     */
    @Test(expected = JPSRuntimeException.class)
    public void testRetrofitEnergyStorage() {
        BatteryRetrofit br = new BatteryRetrofit();
        br.retrofitEnergyStorage(ENIRI, batteryList);
    }

    /**
     * The retrofitEnergyStorage() method does not run without proper setup of the application.
     * We mock the behaviour of retrofitEnergyStorage() to do nothing so we can complete the unit test for the rest.
     */
    @Test
    public void testProcessRequestParameters() {
        JSONObject joValid = new JSONObject();
        joValid.put("electricalnetwork",ENIRI);
        joValid.put("batterylist", batteryiris);
        JSONObject joInValid = new JSONObject();
        joInValid.put("electricalnetwork",ENIRI + "&*(&)(^(*");
        joInValid.put("batterylist", batteryiris);
        BatteryRetrofit br = spy(BatteryRetrofit.class);
        doNothing().when(br).retrofitEnergyStorage(any(), any());
        JSONObject returnedValue = br.processRequestParameters(joValid);
        assertEquals(returnedValue, joValid);
        assertNotEquals(returnedValue, joInValid);
    }

}