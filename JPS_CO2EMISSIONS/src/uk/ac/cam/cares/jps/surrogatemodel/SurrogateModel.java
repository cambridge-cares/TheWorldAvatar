package uk.ac.cam.cares.jps.surrogatemodel;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class SurrogateModel
 */
@WebServlet("/SurrogateModel")
public class SurrogateModel extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    public SurrogateModel() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String powerplantJson = request.getParameter("query");
		System.out.println("INSIDE SURROGATE MODEL: " + powerplantJson);
	}
}
