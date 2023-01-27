package uk.ac.cam.cares.jps.agent.heat.test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;
import uk.ac.cam.cares.jps.agent.heat.HeatNetworkInputAgent;
import uk.ac.cam.cares.jps.agent.heat.HeatNetworkInputAgentLauncher;

public class HeatNetworkInputAgentTest {
		
	@Test
	public void testDataInstantiation () throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		String endpoint = "endpoint";
		HeatNetworkInputAgent agent = spy(new HeatNetworkInputAgent());
	    Method dataInstantiation =  agent.getClass().getDeclaredMethod("dataInstantiation",String.class);
	    assertNotNull(dataInstantiation);
	    doNothing().when(agent).dataInstantiation(anyString());
	    dataInstantiation.invoke(agent,endpoint);
        verify(agent, times(1)).dataInstantiation(anyString());
	}
	
	
	@Test
	public void testHeatGeneratorUpdate () throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		HeatNetworkInputAgent agent1 = spy(new HeatNetworkInputAgent());
	    Method HeatGeneratorUpdate =  agent1.getClass().getDeclaredMethod("HeatGeneratorUpdate",String.class,float.class,float.class, float.class,float.class);
	    assertNotNull(HeatGeneratorUpdate);
	    String HeatGenerator_instance = "Instance";
	    float ThermalLoad = 10;
	    float Value_HCV = 9;
	    float Value_LCV = 8;
	    float Value_CO2Factor =7;
	    doNothing().when(agent1).HeatGeneratorUpdate(anyString(),anyFloat(),anyFloat(),anyFloat(),anyFloat());
	    HeatGeneratorUpdate.invoke(agent1, HeatGenerator_instance,ThermalLoad,Value_HCV,Value_LCV,Value_CO2Factor);
        verify(agent1, times(1)).HeatGeneratorUpdate(anyString(),anyFloat(),anyFloat(),anyFloat(),anyFloat());
	}
	
	
	@Test
	public void testOmHasValueNonTS () throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		HeatNetworkInputAgent agent = spy(new HeatNetworkInputAgent());
	    Method omHasValueNonTS =  agent.getClass().getDeclaredMethod("omHasValueNonTS", String.class, String.class,float.class);
	    assertNotNull(omHasValueNonTS);
	    String Instance = "Instance";
	    String Unit = "Unit";
	    float NumericalValue = 10;
	    doNothing().when( agent).omHasValueNonTS(anyString(), anyString(), anyFloat());
	    omHasValueNonTS.invoke( agent, Instance, Unit, NumericalValue);
        verify( agent, times(1)).omHasValueNonTS(anyString(), anyString(),  anyFloat());
	}
	
	
	@Test
	public void testOmHasValueTS () throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		HeatNetworkInputAgent agent = spy(new HeatNetworkInputAgent());
	    Method omHasValueTS = agent.getClass().getDeclaredMethod("omHasValueTS", String.class, String.class);
	    assertNotNull(omHasValueTS);
	    String Instance = "Instance";
	    String Unit = "Unit";
	    doNothing().when(agent).omHasValueTS(anyString(), anyString());
	    omHasValueTS.invoke(agent, Instance, Unit);
        verify(agent, times(1)).omHasValueTS(anyString(), anyString());
	}
	
	@Test 
	public void testReadColInput () {
		int input_col = 10;
		String filepath = "filepath";
		String delimiter = "delimiter";
		String[] expected = new String[] {"Result1", "Result2", "Result3"};
		try (MockedStatic<HeatNetworkInputAgent> heq= Mockito.mockStatic(HeatNetworkInputAgent.class)){
            heq.when(()->HeatNetworkInputAgent.ReadColInput(input_col,filepath,delimiter)).thenReturn(expected);
            String[] actual= (String[]) HeatNetworkInputAgent.ReadColInput(input_col,filepath,delimiter);
            assertEquals(expected,actual);
        }  
		
	}
	
}