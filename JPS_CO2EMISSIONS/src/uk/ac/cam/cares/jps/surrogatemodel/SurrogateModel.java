package uk.ac.cam.cares.jps.surrogatemodel;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

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
       
    public SurrogateModel() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String powerplantJson = request.getParameter("query");
		Gson g = new Gson();
		
//		Map<String, String> queryParamsJsonObj = new HashMap<String, String>();
//		queryParamsJsonObj = (Map<String, String>) g.fromJson(powerplantJson, queryParamsJsonObj.getClass());
//		String plantIRI = queryParamsJsonObj.get("plant");
		
		QueryParams queryParams = g.fromJson(powerplantJson, QueryParams.class);
		String plantIRI = queryParams.getPlant();
//		String plantIRI = "http://www.theworldavatar.com/kb/powerplants/Hamitabat_II_CCGT_Power_Plant_Turkey.owl#Hamitabat_II_CCGT_Power_Plant_Turkey";
		
		try {
			String plantInfo = PythonHelper.callPython("powerplant_sparql_read.py", plantIRI, this);
			System.out.println(plantIRI);
			System.out.println(plantInfo);
			
			// Put plantInfo into surrogate model and get new emission back
//			String newEmission = "20";
			
			// Store new emission
//			PythonHelper.callPython("powerplant_sparql_update.py", plantIRI, newEmission, this);
			
//			String plantInfoAfter = PythonHelper.callPython("powerplant_sparql_read.py", plantIRI, this);
//			System.out.println(plantInfoAfter);
		} catch (PythonException e) {
			e.printStackTrace();
		}
	}
}
