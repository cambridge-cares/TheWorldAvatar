package uk.ac.cares.jps.composition.endpoints;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cares.jps.composition.compositionagent.CompositionAgent;

/**
 * Servlet implementation class CompositionEndpoint
 */
@WebServlet(description = "The endpoint that receives the requirements of the composite agent", urlPatterns = {
		"/CompositionEndpoint" })
public class CompositionEndpoint extends HttpServlet {

	public CompositionEndpoint() {
		super();
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		String input_request = request.getParameter("query");
		
		// Display the user requirements from the users ... 
		
		System.out.print("--------------- the input request received  -----------------\n");
		System.out.print(input_request);
		System.out.print("\n-------------------------------------------------------------");
		
		
		JSONObject composition_result = parse_requirements_and_start_composition(new JSONObject(input_request), response);
		
		response.getWriter().write(composition_result.toString());

	}

	private static JSONObject parse_requirements_and_start_composition(JSONObject requirements, HttpServletResponse response)
			throws IOException {

		JSONObject the_operation = requirements.getJSONArray("operations").getJSONObject(0);
		JSONArray inputs_json = the_operation.getJSONArray("inputs");
		JSONArray outputs_json = the_operation.getJSONArray("outputs");

		// Construct two ArrayLists that contains the types of the inputs and outputs
		// respectively...

		JSONArray inputs = new JSONArray();
		// ------------------------- put input types in list ----------------------
		for (int i = 0; i < inputs_json.length(); i++) {
			JSONObject _input = inputs_json.getJSONObject(i);
			JSONArray _mandatory_part_list = _input.getJSONArray("mandatoryParts");
			for (int n = 0; n < _mandatory_part_list.length(); n++) {
				JSONObject _mandoatry_part = _mandatory_part_list.getJSONObject(n);
				String _type = _mandoatry_part.getString("type");
				inputs.put(_type);
			}
		}

		JSONArray outputs = new JSONArray();
		// ------------------------- put output types in list ----------------------
		for (int j = 0; j < outputs_json.length(); j++) {
			JSONObject _output = outputs_json.getJSONObject(j);
			JSONArray _mandatory_part_list_output = _output.getJSONArray("mandatoryParts");
			for (int n = 0; n < _mandatory_part_list_output.length(); n++) {
				JSONObject _mandoatry_part_output = _mandatory_part_list_output.getJSONObject(n);
				String _type = _mandoatry_part_output.getString("type");
				outputs.put(_type);
			}
		}

		System.out.println("---------------------- Requirements Received -----------------------");
		System.out.println(inputs.toString() + "|" + outputs.toString());
		System.out.println("--------------------------------------------------------------------");

		CompositionAgent composition_agent = new CompositionAgent();
		return composition_agent.compose(inputs, outputs);
		
		

	}

}
