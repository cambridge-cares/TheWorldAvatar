package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.executor.Executor;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

/**
 * Servlet implementation class ServiceExecutionEndpoint
 */
@WebServlet("/ServiceExecutionEndpoint")
public class ServiceExecutionEndpoint extends HttpServlet {
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public ServiceExecutionEndpoint() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			JSONObject executorInJSON = new JSONObject(request.getParameter("executionChain"));
			Executor executor = FormatTranslator.convertJSONTOExecutor(executorInJSON.toString());
			String value = request.getParameter("value");
			String result = executor.execute(value);
			if (result == null) {
				response.getWriter().write("Error");
			} else {
				response.getWriter().write(result.replace("$", "#").replace("@", "#"));
			}
		} catch (JSONException | URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

}
