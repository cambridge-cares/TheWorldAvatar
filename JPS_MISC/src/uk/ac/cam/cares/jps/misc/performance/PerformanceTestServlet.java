package uk.ac.cam.cares.jps.misc.performance;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.FileUtil;

@WebServlet(urlPatterns = {"/testperformance/*"})
public class PerformanceTestServlet extends HttpServlet {

	private static final long serialVersionUID = 4191745357341834904L;
	private static Logger logger = LoggerFactory.getLogger(PerformanceTestServlet.class);
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		boolean printtoconsole = jo.getBoolean("printtoconsole");
		if (printtoconsole) {
			System.out.println("PerformanceTestServlet millis=" + System.currentTimeMillis() + ", value=" + jo.getString("testkey"));
		}
		
		boolean writetodisk = jo.getBoolean("writetodisk");
		if (writetodisk) {
			String path =  AgentLocator.getPathToJpsWorkingDir() + "/JPS_MISC/test.txt";
			String value = jo.getString("testkey");
			FileUtil.writeFileLocally(path, value);
		}

		AgentCaller.printToResponse("Success", response);
	}
}
