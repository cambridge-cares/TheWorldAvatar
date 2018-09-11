package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.HTTP;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.ontology.ServiceWriter;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;


@WebServlet("/ServiceConvertToOWL")
public class ServiceConvertToOWL extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public ServiceConvertToOWL() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
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
			ServiceWriter writer = new ServiceWriter();
			response.getWriter().write(writer.generateModel(agent));
		} catch (Exception ex) {

		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

}
