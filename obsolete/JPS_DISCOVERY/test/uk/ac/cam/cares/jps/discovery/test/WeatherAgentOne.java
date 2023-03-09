package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.Agent;

@WebServlet(urlPatterns = {"/DiscoveryTest/AgentOne"})
public class WeatherAgentOne extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	Logger logger = LoggerFactory.getLogger(WeatherAgentOne.class);

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		
		logger.info("WeatherAgentOne start");
		
		response.setContentType("text/plain");
		response.setCharacterEncoding("UTF-8");

		PrintWriter out = response.getWriter();
		String message = "I'm weather agent one";
		out.print(message);
		logger.info(message);
	}
	
	public Agent getAgent() {
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		
		return DescriptionFactory.createAgent("http://localhost:8080/JPS_DISCOVERY/DiscoveryTest/AgentOne", general, input, output);
	}
}