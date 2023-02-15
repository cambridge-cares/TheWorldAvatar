package uk.ac.cam.cares.jps.powsys.retrofit.test;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import org.slf4j.Logger;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.powsys.retrofit.RenewableGeneratorRetrofit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.powsys.util.Util;

public class TestRenewableGeneratorRetrofit {

    private String ENIRI;
    private JSONArray pvgeniris = new JSONArray();
    private List<String> renewableGeneratorList = new ArrayList<>();

    @Before
    public void setUp(){
        ENIRI = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
        pvgeniris.put("http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001");
        renewableGeneratorList.add("http://localhost:8080/jps/kb/337ad6e8-6e9b-4d30-b0aa-dfda02e80a1f/nuclearpowerplants/NucGenerator_3_B3.owl#NucGenerator_3_B3");
        renewableGeneratorList.add("http://localhost:8080/jps/kb/337ad6e8-6e9b-4d30-b0aa-dfda02e80a1f/nuclearpowerplants/NucGenerator_3_B0.owl#NucGenerator_3_B0");
    }

    @Test
    public void testSetLogger() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.powsys.retrofit.RenewableGeneratorRetrofit");
        Method setLoggerMethod = TargetClass.getDeclaredMethod("setLogger") ;
        RenewableGeneratorRetrofit rgrClass = mock(RenewableGeneratorRetrofit.class,CALLS_REAL_METHODS);
        setLoggerMethod.setAccessible(true);
        setLoggerMethod.invoke(rgrClass);

        Field logger = TargetClass.getDeclaredField("logger") ;
        logger.setAccessible(true);
        Logger log = (Logger) logger.get(rgrClass);
        assertNotNull(log);
        assertTrue(log.isErrorEnabled());
    }

    /**
     * The retrofitGenerator() method does not run without proper setup of the application.
     * We mock the behaviour of retrofitGenerator() to do nothing so we can complete the unit test for the rest.
     */
    @Test
    public void testProcessRequestParameters() {

        RenewableGeneratorRetrofit rgr = spy(RenewableGeneratorRetrofit.class);
        doNothing().when(rgr).retrofitGenerator(any(), any());
        JSONObject joValid = new JSONObject();
        joValid.put("electricalnetwork",ENIRI);
        joValid.put("RenewableEnergyGenerator", pvgeniris);
        JSONObject returnedValue = rgr.processRequestParameters(joValid);
        assertEquals(returnedValue.getString("electricalnetwork"), ENIRI);
        assertEquals(returnedValue.getJSONArray("RenewableEnergyGenerator"), pvgeniris);

    }

    @Test
    public void testValidateInput() throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.powsys.retrofit.RenewableGeneratorRetrofit");
        Method validateInputMethod = TargetClass.getDeclaredMethod("validateInput", JSONObject.class);

        JSONObject joValid = new JSONObject();
        joValid.put("electricalnetwork",ENIRI);
        joValid.put("RenewableEnergyGenerator", pvgeniris);
        JSONObject joInvalid = new JSONObject();
        joInvalid.put("electricalnetwork", ENIRI + "@&@)#!(*");
        joInvalid.put("RenewableEnergyGenerator", pvgeniris + "&(*%*$&^");

        RenewableGeneratorRetrofit brClass = mock(RenewableGeneratorRetrofit.class,CALLS_REAL_METHODS);
        validateInputMethod.setAccessible(true);
        assertFalse((boolean)validateInputMethod.invoke(brClass, joInvalid));
        assertTrue((boolean)validateInputMethod.invoke(brClass, joValid));
    }


    /**
     * JPSRuntimeException is expected to throw as the unit test cannot be done without a complete setup of POWSYS
     */
    @Test(expected = JPSRuntimeException.class)
    public void testRetrofitGenerator() {
        new RenewableGeneratorRetrofit().retrofitGenerator(ENIRI, renewableGeneratorList);
    }
}