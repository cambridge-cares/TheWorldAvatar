package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.util.PythonHelper;

/**
 * Servlet implementation class ADMSHelper
 */
@WebServlet("/ADMSHelper")
public class ADMSHelper extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSHelper() {
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
			result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString));
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (PythonException e) {
			e.printStackTrace();
		}	
	}

}
