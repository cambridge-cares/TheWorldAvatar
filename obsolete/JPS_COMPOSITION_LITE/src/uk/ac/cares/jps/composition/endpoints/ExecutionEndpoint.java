package uk.ac.cares.jps.composition.endpoints;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cares.jps.composition.compositionagent.CompositionResult;
import uk.ac.cares.jps.composition.utils.Convertor;

/**
 * Servlet implementation class ExecutionEndpoint
 */
@WebServlet("/ExecutionEndpoint")
public class ExecutionEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public ExecutionEndpoint() {
		super();

	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		JSONObject composition_result_in_JSON = new JSONObject(request.getParameter("query"));
		CompositionResult composition_result = Convertor.deserialize_composition_result(composition_result_in_JSON);

	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		//doGet(request, response);
		JSONObject composition_result_in_JSON = new JSONObject(request.getParameter("query"));
		CompositionResult composition_result = Convertor.deserialize_composition_result(composition_result_in_JSON);
	}

}
