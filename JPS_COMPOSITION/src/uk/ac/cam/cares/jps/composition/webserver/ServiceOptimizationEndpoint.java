package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;
import org.openjena.atlas.json.JSON;

import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.util.OptimalPathSearcher;

/**
 * Servlet implementation class ServiceOptimizationEndpoint
 */
@WebServlet("/ServiceOptimizationEndpoint")
public class ServiceOptimizationEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public ServiceOptimizationEndpoint() {
		super();
	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			StringBuilder sb = new StringBuilder();
			String s;
			while ((s = request.getReader().readLine()) != null) {
				sb.append(s);
			}
			JSONObject jsonObject = HTTP.toJSONObject(sb.toString());
			String GraphInString = jsonObject.getString("Method").toString();
			JSONObject GraphInJSON = new JSONObject(GraphInString);
			Graph graph = FormatTranslator.convertGraphJSONTOJavaClass(GraphInJSON.toString());
			ArrayList<Service> servicesToBeDeleted = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
			JSONArray servicesToBeDeletedInJSON = new JSONArray();
			for (Service service : servicesToBeDeleted) {
				servicesToBeDeletedInJSON.put(service.getUri().toASCIIString());
			}
			response.getWriter().write(servicesToBeDeletedInJSON.toString());
		} catch (Exception ex) {

		}
	}

}
