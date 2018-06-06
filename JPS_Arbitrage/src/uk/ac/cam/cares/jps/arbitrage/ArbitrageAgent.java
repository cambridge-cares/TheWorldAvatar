package uk.ac.cam.cares.jps.arbitrage;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Servlet implementation class ArbitrageAgent
 */
@WebServlet(urlPatterns = {
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles",
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent" })

public class ArbitrageAgent extends HttpServlet {
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

		if ("/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("MoDS_input");

			try {
				String result = Arbitrage
						.runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles(
								jsonString);
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent"
				.equals(path)) {
			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("MoDS_input");

			try {
				String result = Arbitrage
						.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent(
								jsonString);
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
