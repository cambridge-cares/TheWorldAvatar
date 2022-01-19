package uk.ac.cares.jps.composition.endpoints;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cares.jps.composition.compositionagent.OptimizationAgent;

/**
 * Servlet implementation class OptimizationEndpoint
 */
@WebServlet("/OptimizationEndpoint")
public class OptimizationEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

	//

	public OptimizationEndpoint() {
		super();
	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		JSONObject composition_result = new JSONObject(request.getParameter("query"));

		OptimizationAgent optimization_agent = new OptimizationAgent();
		composition_result = optimization_agent.optimize_composition_result(composition_result);
		response.getWriter().write(composition_result.toString());

	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

}
