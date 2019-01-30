package uk.ac.cam.cares.jps.base.test;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet(urlPatterns = {"/test/AgentBase/*"})
public class AgentBase extends HttpServlet {
	
	Logger logger = LoggerFactory.getLogger(AgentBase.class);

	private static final long serialVersionUID = 1L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
			
		response.setContentType("text/plain");
		response.setCharacterEncoding("UTF-8");

		JSONObject json = AgentCaller.readJsonParameter(request);
		logger.info("input: = " + json);
		AgentCaller.writeJsonParameter(response, json);
	}
}