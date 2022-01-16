package uk.ac.cam.cares.jps.setupjob.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.dispersion.episode.EpisodeAgent;

public class SetupJobTest {
	
	

	@Test
	public void testjob() {
		JSONObject jo= new JSONObject();
		jo.put("runWholeScript",true);
		String datapath="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\21afbf75-1d47-407e-9569-505c684f7385";
		try {
			new EpisodeAgent().setUpJob(jo.toString(),datapath);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		};
		
	}
	
	

}
