package uk.ac.cam.cares.jps.adms;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/**
 * Servlet implementation class ADMSOutput
 */
@WebServlet("/ADMSOutput")
public class ADMSOutput extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSOutput() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("coordinatesLonLat");
		
		Gson g = new Gson();
		
		String outputFile = "C:/TOMCAT/webapps/JPS/workingdir/test.levels.gst";
		
		String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSOutput.py", outputFile , g.toJson(jsonString), this);
		
		response.setContentType("application/json");
		response.getWriter().write(result);
	}

}
