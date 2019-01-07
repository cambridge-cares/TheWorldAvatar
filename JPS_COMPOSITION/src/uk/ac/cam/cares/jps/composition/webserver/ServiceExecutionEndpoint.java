package uk.ac.cam.cares.jps.composition.WebServer;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.performance.PerformanceMonitor;
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
			ExecutorNew executor = FormatTranslator.convertJSONTOExecutor(executorInJSON.toString());
			String value = request.getParameter("query");
			System.out.println("================ Value ===============");
			System.out.println(value);
			System.out.println("======================================");
			String result = executor.execute(new JSONObject(value));
			if (result == null) {
				response.getWriter().write("Error");
			} else {
				
				PerformanceMonitor.start();
				response.getWriter().write(result.replace("$", "#").replace("@", "#"));
				
//				JSONObject o = new JSONObject(result).getJSONObject("score");
//				System.out.println("================ Hit City To Weather ===============");
//				long time = o.getLong("time");
//				int code = o.getInt("code");
//				long timeStamp = o.getLong("time_stamp");
//				String _result = o.getString("result");
//				double coverage = PerformanceMonitor.checkCoverage(_result);
//				PerformanceMonitor.updateAScoreMatrix(time, code, coverage, timeStamp, PerformanceMonitor.id_map.get("/JPS_COMPOSITION/CityToWeather"));
//				PerformanceMonitor.Make_Payment();
			}
		} catch (JSONException | URISyntaxException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

	
 
	
}
