package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.config.AgentLocator;

@WebServlet(urlPatterns = {"/Configtest/AgentOne/*"})
public class AgentOne extends HttpServlet {

	private static final long serialVersionUID = 1L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		response.setContentType("text/plain");
		response.setCharacterEncoding("UTF-8");

		PrintWriter out = response.getWriter();
		out.print(getAgentText());

		// https://stackoverflow.com/questions/3035656/communication-between-remote-servlets/

		// Class Not Found Exception --> copy libraries of Apache HC to lib directory
		// of tomcat under Eclipse: double click server and see server path
		String responseAgentTwo = AgentLocator.callAgent("agent.test.agenttwo");
		out.print(responseAgentTwo);

		// request.getRequestDispatcher("/secondServletURL").include(request, response);

		// response.sendRedirect(request.getContextPath() +
		// "/ExamplesTomCat/myservlets/AgentTwo");
	}
	
	protected String getAgentText() {
		return "I am AgentOne ";
	}
}