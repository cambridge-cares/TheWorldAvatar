package uk.ac.cam.cares.jps.dispersion.coordination;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet("/AQMeshCollector")
public class PeriodicAQMeshCall extends HttpServlet {
	public void executeSGAQMeshData(){

		JSONObject jo = new JSONObject();
		jo.put("city", "http://dbpedia.org/resource/Singapore");
		String resultStart = AgentCaller.executeGetWithJsonParameter("/JPS_DISPERSION/AQMeshCoordinationAgent", jo.toString());
		System.out.println("done calling aqmesh");
	}


protected void doGet(HttpServletRequest req, HttpServletResponse res) throws IOException {
	System.out.println("executing AQMEsh COordination");
	executeSGAQMeshData();
}
}
