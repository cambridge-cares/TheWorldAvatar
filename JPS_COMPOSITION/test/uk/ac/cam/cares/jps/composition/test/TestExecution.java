package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

public class TestExecution {

	final String test_case_4_string = "{\"executionChain\":[{\"taskList\":[{\"targetHttpUrl\":[\"http://www.theworldavatar.com/JPS/PowerPlant\",\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\",\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\",\"http://www.theworldavatar.com/JPS/PowerPlant\",\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\",\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\"],\"inputIndexArray\":[[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0]],\"httpToNameMapping\":{\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\":{\"city\":{\"city\":\"city\"}}},\"httpUrl\":\"http://www.theworldavatar.com/JPS/RegionToCity\",\"keysArray\":[[\"city\"],[\"city\"],[\"city\"],[\"city\"],[\"city\"],[\"city\"]]}]},{\"taskList\":[{\"targetHttpUrl\":[\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\",\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\"],\"inputIndexArray\":[[0,0],[-1,0],[-1,0],[-1,0],[0,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0]],\"httpUrl\":\"http://www.theworldavatar.com/JPS/PowerPlant\",\"keysArray\":[[\"waste\"],[\"waste\"]]}]},{\"taskList\":[{\"inputIndexArray\":[[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[0,2],[0,2]],\"httpUrl\":\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\"},{\"targetHttpUrl\":[\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\",\"http://www.theworldavatar.com/JPS/GetBuildingListFromRegion\"],\"inputIndexArray\":[[0,2],[0,2]],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CityToWeather\",\"keysArray\":[[\"weatherstate\"],[\"weatherstate\"]]}]},{\"taskList\":[{\"inputIndexArray\":[[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[0,2],[0,2],[0,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[0,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[-1,0],[0,1],[0,1],[1,1],[1,1]],\"httpUrl\":\"http://www.theworldavatar.com/JPS/ADMSAgent\"}]}],\"myPort\":8080,\"myHost\":\"localhost\"}";
			
			
	final String value_case_4_string = "{\"region\":{\"lowercorner\":{\"lowerx\":\"4.287894360665291\",\"lowery\":\"52.07188455940009\"},\"uppercorner\":{\"upperx\":\"4.294267289284676\",\"uppery\":\"52.07792491310677\"},\"srsname\":\"EPSG:4326\"},\"reactionmechanism\":\"https://como.cheng.cam.ac.uk/kb/Reduced_PRF_ERC.owl#ReactionMechanism_4909454516579602\",\"plant\":\"http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001\"}";
	  
	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() {
		 try {
			clean_up(test_case_4_string);
		} catch (JSONException | URISyntaxException | IOException e) {
			e.printStackTrace();
		}
	}

	private String clean_up(String test_case_string) throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		
		
		JSONObject executorInJSON = new JSONObject(test_case_string);
		ExecutorNew executor = FormatTranslator.convertJSONTOExecutor(executorInJSON.toString());
		String result = executor.execute(new JSONObject(value_case_4_string));
		
		return null;
	}
	
	
}
