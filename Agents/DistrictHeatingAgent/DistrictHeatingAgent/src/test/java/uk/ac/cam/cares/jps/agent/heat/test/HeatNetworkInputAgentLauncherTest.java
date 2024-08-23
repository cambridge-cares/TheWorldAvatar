package uk.ac.cam.cares.jps.agent.heat.test;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.heat.HeatNetworkInputAgentLauncher;
import uk.ac.cam.cares.jps.agent.heat.HeatNetworkInputAgent;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class HeatNetworkInputAgentLauncherTest {	
	
	
	@Test 
	public void testReadCol () {
		int input_col = 10;
		String filepath = "filepath_exampple";
		String delimiter = "delimiter_example";
		String[] expected = new String[] {"ResultA", "ResultB", "ResultC"};
		try (MockedStatic<HeatNetworkInputAgentLauncher> heq= Mockito.mockStatic(HeatNetworkInputAgentLauncher.class)){
            heq.when(()->HeatNetworkInputAgentLauncher.ReadCol(input_col,filepath,delimiter)).thenReturn(expected);
            String[] actual= (String[]) HeatNetworkInputAgentLauncher.ReadCol(input_col,filepath,delimiter);
            assertEquals(expected,actual);
        } 
		}
	
	@Test
	public void testColNum () throws IOException {
		String filepath = "filepath_exampple";
		String delimiter = "delimiter_example";
		int expected = 10;
		try (MockedStatic<HeatNetworkInputAgentLauncher> heq= Mockito.mockStatic(HeatNetworkInputAgentLauncher.class)){
            heq.when(()->HeatNetworkInputAgentLauncher.ColNum(filepath,delimiter)).thenReturn(expected);
            int actual= (int) HeatNetworkInputAgentLauncher.ColNum(filepath,delimiter);
            assertEquals(expected,actual);
        } 
		
	}
	
	@Test
	public void testFormatDate () {
		HeatNetworkInputAgentLauncher agent = new HeatNetworkInputAgentLauncher();
		String input_example = "5/1/20202:00:00";
		@SuppressWarnings("static-access")
		String result = agent.formatDate(input_example);
		String expected = "2020-01-05T02:00:00";
		assertEquals(expected,result);
		
	}
}
