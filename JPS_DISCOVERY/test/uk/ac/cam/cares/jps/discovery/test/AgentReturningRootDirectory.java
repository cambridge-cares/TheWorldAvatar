package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;

@WebServlet(urlPatterns = {"/Configtest/AgentReturningRootDirectory"})
public class AgentReturningRootDirectory extends HttpServlet {

	private static final long serialVersionUID = 1L;

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		response.setContentType("text/plain");
		response.setCharacterEncoding("UTF-8");

		PrintWriter out = response.getWriter();
		String message = AgentLocator.getCurrentJpsAppDirectory(this);
		out.print(message);
	}
}
