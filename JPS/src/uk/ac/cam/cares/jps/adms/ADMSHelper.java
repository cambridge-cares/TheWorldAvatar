package uk.ac.cam.cares.jps.adms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.config.AgentLocator;
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
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("listOfIRIs");
		
		Gson g = new Gson();
		
		// -- Convert from String to Array of Strings -- //
		// String[] stringArray = g.fromJson(jsonString, String[].class);
		
		String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString));
		
//		String s = "";
//		String result = "";
//		
//		System.out.println("\nStandard Output of Command:");
//		while((s = stdInput.readLine()) != null) {
//			System.out.println(s);
//			result += s + "\n";
//		}
		
		response.setContentType("application/json");
		response.getWriter().write(result);
	}

}
