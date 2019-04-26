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
 * Servlet implementation class ADMSPowerPlantGetter
 * Accepts location (Berlin or The Hague) in URL query string
 * Returns GeoJSON object, representing a selected powerplant in The Hague or Berlin
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
    
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String location = request.getParameter("location");
		
		System.out.println("location: " + location);
		
		String powerPlantIRI = "";
		String epsg = "";
		
		// TODO-AE URGENT hardcoded city and plant
		if (location.equals("The Hague")) {
			powerPlantIRI = "http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001";
			epsg = "epsg:28992";
			
		} else if (location.equals("Berlin")) {
			powerPlantIRI = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
			epsg = "epsg:25833";
			//epsg = "epsg:28992"; 23/4 changes
		}
		
		String result;
		
		try {
			System.out.println("Trying to get powerplants data");
			System.out.println(powerPlantIRI + "|" + epsg);
			result = PythonHelper.callPython("caresjpsadmsinputs/ADMSPowerPlantGetter.py", 
					powerPlantIRI, epsg, this);
			response.setContentType("application/json");
			response.getWriter().write(result);
		} catch (PythonException e) {
			e.printStackTrace();
		}
	}

}
