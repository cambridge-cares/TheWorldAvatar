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

import uk.ac.cam.cares.jps.base.config.AgentLocator;

@WebServlet(urlPatterns = {"/Configtest/AgentOne/*"})
public class AgentOne extends HttpServlet {
	
	Logger logger = LoggerFactory.getLogger(AgentOne.class);

	private static final long serialVersionUID = 1L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
			
		response.setContentType("text/plain");
		response.setCharacterEncoding("UTF-8");

		PrintWriter out = response.getWriter();
		String message = getAgentText();
		out.print(message);
		logger.info(message);
		
		String responseAgentTwo = AgentLocator.callAgent("agent.test.agenttwo");
		out.print(responseAgentTwo);
		
		// alternative way to call
		// https://stackoverflow.com/questions/3035656/communication-between-remote-servlets/
		// request.getRequestDispatcher("/secondServletURL").include(request, response);
		// response.sendRedirect(request.getContextPath() +
		// "/ExamplesTomCat/myservlets/AgentTwo");
	}
	
	protected String getAgentText() {
		return "I am AgentOne ";
	}
}