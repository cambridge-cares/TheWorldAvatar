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
@WebServlet(urlPatterns = {"/hardcode", "/KB"})

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

	/**this function is a servlet for calling functions in uk.ac.cam.cares.jps.arbitrage package and returning their results;
	 * it discriminates between "/hardcode", "/KB" URL patterns and calls Arbitrage.Running_analysis_MoDS and 
	 * Arbitrage.Running_analysis_MoDS2, respectively
	 */
	
	String path = request.getServletPath();
	
	if ("/hardcode".equals(path)) {
	
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("MoDS_input");
		
		try {
			String result = Arbitrage.Running_analysis_MoDS(jsonString);
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	} else if ("/KB".equals(path)) {
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("MoDS_input");
		
		try {
			String result = Arbitrage.Running_analysis_MoDS2(jsonString);
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
	}
}
}
}

