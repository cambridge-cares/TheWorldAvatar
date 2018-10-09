package uk.ac.cam.cares.jps.worldpowerplant;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

import com.google.gson.Gson;
/**
 * Servlet implementation class WorldPowerPlant
 */
@WebServlet("/WorldPowerPlant")
public class WorldPowerPlant extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    public WorldPowerPlant() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//		response.getWriter().append("Served at: ").append(request.getContextPath());
		request.setCharacterEncoding("UTF-8");
				
		try {
			String stringArrayOfPowerplantIRI = PythonHelper.callPython("world_powerplants_sparql.py", 
					"", this);
			
			Gson g = new Gson();
			String[] arrayOfPowerplantIRI = g.fromJson(stringArrayOfPowerplantIRI, String[].class);
			
			
			String path = "/JPS_CO2EMISSIONS/SurrogateModel";
			String key = "query";
			for(int i = 0; i < arrayOfPowerplantIRI.length; i++) {
				String value = arrayOfPowerplantIRI[i];
				AgentCaller.executeGet(path, key, value);
			}
			
		} catch (PythonException e) {
			e.printStackTrace();
		}
	}

}
