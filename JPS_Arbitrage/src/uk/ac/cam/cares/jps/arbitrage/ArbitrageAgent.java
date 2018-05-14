package uk.ac.cam.cares.jps.arbitrage;


import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
/**
 * Servlet implementation class ArbitrageAgent
 */
@WebServlet("/ArbitrageAgent")

public class ArbitrageAgent extends HttpServlet {
	private static final long serialVersionUID = 2L; //??
    
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ArbitrageAgent() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("MoDS_input");
		
		try {
			String result = Arbitrage.Running_analysis_MoDS(jsonString);
			response.setContentType("application/json");
			response.getWriter().write(result);
//		} catch (PythonException e) {
//			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}
}
