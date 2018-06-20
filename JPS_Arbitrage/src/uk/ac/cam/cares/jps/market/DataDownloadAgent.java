package uk.ac.cam.cares.jps.market;

//import java.io.BufferedWriter;
//import java.io.FileWriter;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Servlet implementation class DataDownloadAgent
 */
@WebServlet(urlPatterns = {
		"/downloadCPOMarketData",
		"/downloadFAMEMarketData",
		"/downloadZCEMarketData",
		"/downloadHNGMarketData",
		"/downloadingAndSavingMarketDataInTheKnowledgeBase",
		"/downloadingAndSavingExchangeRatesInTheKnowledgeBase",
		"/savingDataInTheKnowledgeBase",
		"/retrieveUtilityPrices",
		"/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase",
		"/retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase"})
public class DataDownloadAgent extends HttpServlet {
	private static Logger logger = LoggerFactory
			.getLogger(DataDownloadAgent.class);

	private static final long serialVersionUID = 2L; // ??

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public DataDownloadAgent() {
		super();
	}

	// delete later
	// public void writeStringUsingBufferedWriter(String
	// function, String result) throws IOException {
	// BufferedWriter writer = new BufferedWriter(new
	// FileWriter("C:\\jps\\jps_arbitrage\\consoleOutputDataDownloadAgent.txt",
	// true));
	// writer.append(function);
	// writer.newLine();
	// writer.append(result);
	// writer.newLine();
	// writer.close();
	// }

	/**
	 * this function is a servlet for calling functions in
	 * uk.ac.cam.cares.jps.market package and returning
	 * their results; it discriminates between "/download",
	 * "/download2" and "/read" URL patterns and calls
	 * DataDownload.Downloading_market_data,
	 * DataDownload.Downloading_currencies and
	 * DataDownload.Call_data, respectively
	 * 
	 * @see HttpServlet#doGet(HttpServletRequest request,
	 *      HttpServletResponse response)
	 */

	protected void doGet(HttpServletRequest request,
			HttpServletResponse response)
			throws ServletException, IOException {

		String path = request.getServletPath();

		Gson g = new Gson();
		
		if ("/downloadCPOMarketData"
				.equals(path)) {
			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadCPOMarketData());
				
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/downloadFAMEMarketData"
				.equals(path)) {
			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadFAMEMarketData());
				
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}

		} else if ("/downloadZCEMarketData"
				.equals(path)) {
			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadZCEMarketData());
				
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}

		} else if ("/downloadHNGMarketData"
				.equals(path)) {
			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadHNGMarketData());
				
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}

		} else if ("/downloadingAndSavingMarketDataInTheKnowledgeBase"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadingAndSavingMarketDataInTheKnowledgeBase());
				// String result = DataDownload
				// .downloadingAndSavingMarketDataInTheKnowledgeBase();
				// delete later
				// writeStringUsingBufferedWriter(path,
				// g.fromJson(result, String.class));

				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}

		} else if ("/retrieveUtilityPrices"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("individuals");

			try {
				String result = g.toJson(DataDownload.retrieveUtilityPrices(jsonString.split(",")));
				
				// delete later
//				writeStringUsingBufferedWriter(path, result);
				
				response.setContentType("application/json");
				response.getWriter()
						.write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("individuals");

			try {
				String result = g.toJson(DataDownload
						.retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase(
								jsonString.split(",")));

				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("individuals");

			try {
				String result = g.toJson(DataDownload
						.retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase(
								jsonString.split(",")));

				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/downloadingAndSavingExchangeRatesInTheKnowledgeBase"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadingAndSavingExchangeRatesInTheKnowledgeBase());

				// delete later
				// writeStringUsingBufferedWriter(path,
				// g.fromJson(result, String.class));

				response.setContentType("application/json");
				response.getWriter()
						.write(result.toString());
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/savingDataInTheKnowledgeBase".equals(path)) {
			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("arrayHeaderPrices");
			
			try {
				String result = g.toJson(DataDownload.savingDataInTheKnowledgeBase(jsonString));
								
//				delete later
//				writeStringUsingBufferedWriter(path, g.fromJson(result, String.class));
				
				response.setContentType("application/json");
				response.getWriter()
						.write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		}

	}
}
