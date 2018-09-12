package uk.ac.cam.cares.jps.servicespool.test;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

public class TestWriteMet {

	@Test
	public void test() throws JSONException {

		String WeatherString = " {\"precitipation\":\"0.0\",\"weatherIRI\":\"http://www.theworldavatar.com/WeatherOfDen_Haag934759441\",\"cloudCover\":\"20\",\"windDirection\":\"250\",\"windSpeed\":\"7.7\",\"temperature\":\"31\"}\r\n";
		JSONObject WeatherInJSON = new JSONObject(WeatherString);
		//TODO: Write a met file from this JSON (Done by 2018.8.25)
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);

		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsMetWriter.py"); 
		args.add(fullPath);
		args.add(WeatherInJSON.toString().replace("\"", "\\\""));
		String result = CommandHelper.executeCommands(targetFolder, args);
		assertEquals(result, "SUCCESS: MET File is Created");
		
		
	}

}
