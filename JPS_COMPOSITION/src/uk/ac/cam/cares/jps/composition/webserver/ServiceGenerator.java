package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

/**
 * Servlet implementation class ServiceGenerator
 * 
 * This servlet serves as the interface that receives Service in the form of
 * JSON. This very interface is currently only used as a tool to generate mockup
 * services for testing.
 */
@WebServlet("/ServiceGenerator")
public class ServiceGenerator extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public ServiceGenerator() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			StringBuilder sb = new StringBuilder();
			String s;
			while ((s = request.getReader().readLine()) != null) {
				sb.append(s);
			}
			JSONObject jsonObject = HTTP.toJSONObject(sb.toString());
			String AgentInString = jsonObject.getString("Method").toString();
			Service agent = FormatTranslator.convertJSONTOJavaClass(AgentInString);
			FormatTranslator.convertJavaClassTOJSON(agent);
			response.getWriter().write("The response:" + agent.getAllOutputs());
		} catch (Exception ex) {

		}
	}

}
