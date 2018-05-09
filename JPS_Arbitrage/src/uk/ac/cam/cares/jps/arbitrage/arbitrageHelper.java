package uk.ac.cam.cares.jps.arbitrage;


import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.adms.PythonException;
import uk.ac.cam.cares.jps.util.PythonHelper;
import uk.ac.cam.cares.jps.arbitrage.arbitrage;
import com.google.gson.Gson;

/**
 * Servlet implementation class arbitrageHelper
 */
@WebServlet("/arbitrageHelper")

public class arbitrageHelper extends HttpServlet {
	private static final long serialVersionUID = 2L; //??
    
    /**
     * @see HttpServlet#HttpServlet()
     */
    public arbitrageHelper() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("listOfIRIs");
		
		Gson g = new Gson();
		
		String result;
		try {
			//result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString));
			result = arbitrage.Running_analysis_MoDS(g.toJson(jsonString));			
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (PythonException e) {
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}
}
