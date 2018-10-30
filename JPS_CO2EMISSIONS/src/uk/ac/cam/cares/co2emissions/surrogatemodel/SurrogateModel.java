package uk.ac.cam.cares.co2emissions.surrogatemodel;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

class QueryParams {
	private final String plant;
	
	public QueryParams(String plant) {
		this.plant = plant;
	}
	
	public String getPlant() {
		return this.plant;
	}
	
	@Override
	public String toString() {
		return new StringBuilder().append("QueryParams { ")
				.append("plant: ").append(plant)
				.append(" }").toString();
	}
}

/**
 * Servlet implementation class SurrogateModel
 */
@WebServlet("/SurrogateModel")
public class SurrogateModel extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Gson g = new Gson();
	private final String WORKINGDIR_ADMS_PATH = AgentLocator.getPathToWorkingDir(this);
       
    public SurrogateModel() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String powerplantJson = request.getParameter("query");
//		Gson g = new Gson();
		
//		Map<String, String> queryParamsJsonObj = new HashMap<String, String>();
//		queryParamsJsonObj = (Map<String, String>) g.fromJson(powerplantJson, queryParamsJsonObj.getClass());
//		String plantIRI = queryParamsJsonObj.get("plant");
		
		QueryParams queryParams = g.fromJson(powerplantJson, QueryParams.class);
		String plantIRI = queryParams.getPlant();
//		String plantIRI = "http://www.theworldavatar.com/kb/powerplants/Hamitabat_II_CCGT_Power_Plant_Turkey.owl#Hamitabat_II_CCGT_Power_Plant_Turkey";
		
		try {
			long startTime = System.currentTimeMillis();
			
			
			String plantInfo = PythonHelper.callPython("powerplant_sparql_read.py", plantIRI, this);
			
			// Put plantInfo into surrogate model and get new emission back
//			String co2EmissionRate = PythonHelper.callPython("surrogate_model.py", g.toJson(plantInfo), WORKINGDIR_ADMS_PATH, this);
			String co2EmissionRate = "100.0";
			
			// Store new emission
			String result = PythonHelper.callPython("powerplant_sparql_update.py", plantIRI, co2EmissionRate, this);
			
			long stopTime = System.currentTimeMillis();
			long elapsedTime = stopTime - startTime;
			System.out.println(elapsedTime);
			
//			String plantInfoAfter = PythonHelper.callPython("powerplant_sparql_read.py", plantIRI, this);
//			String plantInfoAfter = PythonHelper.callPython("powerplant_sparql_read_local.py", plantIRI, this);
		} catch (PythonException e) {
//			e.printStackTrace();
			System.out.println(plantIRI);
		}
	}
}
