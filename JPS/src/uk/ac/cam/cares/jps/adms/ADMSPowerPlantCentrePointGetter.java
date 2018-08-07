package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/**
 * Servlet implementation class ADMSPowerPlantCentrePointGetter
 */
@WebServlet("/ADMSPowerPlantCentrePointGetter")
public class ADMSPowerPlantCentrePointGetter extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSPowerPlantCentrePointGetter() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String location = request.getParameter("location");
		String powerPlantIRI = "";
		String epsg = "";
		
		if (location.equals("The Hague")) {
			powerPlantIRI = "http://www.theworldavatar.com/Plant-001.owl#Plant-001";
			epsg = "epsg:28992";
		} else if (location.equals("Berlin")) {
			powerPlantIRI = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
			epsg = "epsg:25833";
		}
		
		String result;
		try {
			result = PythonHelper.callPython("caresjpsadmsinputs/ADMSPowerPlantCentrePointGetter.py", powerPlantIRI, epsg, this);
			System.out.println("result: " + result);
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (PythonException e) {
			e.printStackTrace();
		}
	}

}
