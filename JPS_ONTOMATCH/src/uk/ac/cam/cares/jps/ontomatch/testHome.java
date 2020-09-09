package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
@WebServlet(urlPatterns = { "/index" })
public class testHome extends HttpServlet{
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
	{
	    response.setContentType("text/html;charset=UTF-8");

	    try {
	        RequestDispatcher view = request.getRequestDispatcher("/index.html");
	        view.forward(request, response);
	    }
	    catch(Exception ex) {

	    }
	}
}
