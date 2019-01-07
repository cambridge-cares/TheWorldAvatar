package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

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
		String listOfIRIs = request.getParameter("listOfIRIs");
		String cityiri = request.getParameter("cityiri");

		Gson g = new Gson();
		String result;
		try {
			
			String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);

			ArrayList<String> args = new ArrayList<String>();
			args.add("python");
			args.add("ADMSGeoJsonGetter.py"); 
			args.add(g.toJson(listOfIRIs));
			args.add(cityiri);

			result = CommandHelper.executeCommands(targetFolder, args);

			response.setContentType("application/json");
			response.getWriter().write(result);				
			
		} catch (PythonException e) {
			e.printStackTrace();
		}	
	}

}
