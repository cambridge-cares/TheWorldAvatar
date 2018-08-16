package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

/**
 * Servlet implementation class ADMSPowerPlantGetter
 */
@WebServlet("/ADMSPowerPlantGetter")
public class ADMSPowerPlantGetter extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSPowerPlantGetter() {
        super();
    }
    
    private final String WORKINGDIR_ADMS_PATH = AgentLocator.getPathToWorkingDir(this) + "/ADMS";

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String location = request.getParameter("location");
		String powerPlantIRI = "";
		String epsg = "";
		String powerplantKnowledgeBase = "";
		
		if (location.equals("The Hague")) {
			powerPlantIRI = "http://www.theworldavatar.com/Plant-001.owl#Plant-001";
			epsg = "epsg:28992";
			powerplantKnowledgeBase = WORKINGDIR_ADMS_PATH + "/Plant-001.owl";
		} else if (location.equals("Berlin")) {
			powerPlantIRI = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
//			epsg = "epsg:25833";
			epsg = "epsg:28992";
			powerplantKnowledgeBase = WORKINGDIR_ADMS_PATH + "/Heizkraftwerk_Mitte.owl";
		}
		
		String result;
		
		try {
			result = PythonHelper.callPython("caresjpsadmsinputs/ADMSPowerPlantGetter.py", 
					powerPlantIRI, epsg, powerplantKnowledgeBase,
					"",
					this);
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (PythonException e) {
			e.printStackTrace();
		}
	}

}
