package uk.ac.cam.cares.jps.arbitrage;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Servlet implementation class ArbitrageAgent
 */
@WebServlet(urlPatterns = {
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent",
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2"})

public class ArbitrageAgent extends HttpServlet {
	private static Logger logger = LoggerFactory.getLogger(ArbitrageAgent.class);	
	
	private static final long serialVersionUID = 2L; // ??

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public ArbitrageAgent() {
		super();
	}
	
	/**
	 * 
	 * this function is a servlet for calling functions in
	 * uk.ac.cam.cares.jps.arbitrage package and returning
	 * their results; it discriminates between "/hardcode",
	 * "/KB" URL patterns and calls
	 * Arbitrage.Running_analysis_MoDS and
	 * Arbitrage.Running_analysis_MoDS2, respectively
	 * 
	 * @see HttpServlet#doGet(HttpServletRequest request,
	 *      HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response)
			throws ServletException, IOException {

		String path = request.getServletPath();
		
		Gson g = new Gson();

		if ("/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent"
				.equals(path)) {

			request.setCharacterEncoding("UTF-8");
			String modsInput = request.getParameter("MoDS_input");
			String choicePlant = request.getParameter("choicePlant");
			
			try {
				String result = "";
				
				if (choicePlant.equals("Biodiesel")) {
					result = g.toJson(Arbitrage
							.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent(
									modsInput));
				} else if (choicePlant.equals("Methanol")) {
					result = g.toJson(Arbitrage
							.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2(
									modsInput));
				}
				
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}
	}
}
