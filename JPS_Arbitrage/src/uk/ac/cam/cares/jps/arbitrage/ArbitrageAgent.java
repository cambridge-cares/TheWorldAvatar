package uk.ac.cam.cares.jps.arbitrage;


import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

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
		System.out.println(1);
		String jsonString = request.getParameter("MoDS_input");
		System.out.println(jsonString);
		
		try {
			//result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString));
			System.out.println(2);
			String result = Arbitrage.Running_analysis_MoDS(jsonString);	
			System.out.println(result);
			response.setContentType("application/json");
			response.getWriter().write(result);
			System.out.println(3);
//		} catch (PythonException e) {
//			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.out.println(4);
		}	
	}
}
