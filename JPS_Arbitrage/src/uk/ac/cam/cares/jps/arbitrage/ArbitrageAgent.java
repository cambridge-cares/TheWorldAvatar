package uk.ac.cam.cares.jps.arbitrage;

import java.io.BufferedWriter;
import java.io.FileWriter;
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
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles",
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent",
		"/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2"})

public class ArbitrageAgent extends HttpServlet {
	private static Logger logger = LoggerFactory
			.getLogger(ArbitrageAgent.class);	
	
	private static final long serialVersionUID = 2L; // ??

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public ArbitrageAgent() {
		super();
	}
	
	// delete later
	public void writeStringUsingBufferedWriter(String function, String result) throws IOException {
		BufferedWriter writer = new BufferedWriter(new FileWriter("C:\\jps\\jps_arbitrage\\consoleOutputArbitrageAgent.txt", true));
		writer.append(function);
		writer.newLine();
		writer.append(result);
		writer.newLine();
		writer.close();
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

		if ("/runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("MoDS_input");

			try {
				String result = g.toJson(Arbitrage
						.runningArbitrageAnalysisUsingMoDSWithMarketDataFromCSVFiles(
								jsonString));
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
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
			
//			delete later
//			System.out.println(path);
//			System.out.println(jsonString);
//			writeStringUsingBufferedWriter(path, jsonString);

			try {
				String result = g.toJson(Arbitrage
						.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent(
								jsonString));
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2"
				.equals(path)) {
			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("MoDS_input");

//			delete later
//			System.out.println(path);
//			System.out.println(jsonString);
			writeStringUsingBufferedWriter(path, jsonString);

			try {
				String result = g.toJson(Arbitrage
						.runningArbitrageAnalysisUsingMoDSWithMarketDataProvidedByDataDownloadAgent2(
								jsonString));
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		}
	}
}
