package uk.ac.cam.cares.jps.setupjob.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.dispersion.episode.EpisodeAgent;

public class SetupJobTest {
	
	

	@Test
	public void testjob() {
		JSONObject jo= new JSONObject();
		jo.put("runWholeScript",true);
		String datapath="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\2cb1f568-f60f-456b-a0d6-be9dbf5ca549";
		try {
			new EpisodeAgent().setUpJob(jo.toString(),datapath);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		};
		
	}
	
	

}
