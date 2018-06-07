package uk.ac.cam.cares.jps.market;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Servlet implementation class DataDownloadAgent
 */
@WebServlet(urlPatterns = {
		"/downloadingAndSavingMarketDataInTheKnowledgeBase",
		"/downloadingAndSavingExchangeRatesInTheKnowledgeBase",
		"/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase" })
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

		if ("/downloadingAndSavingMarketDataInTheKnowledgeBase"
				.equals(path)) {

			// -- Get String formatted in Array of Strings
			// -- //
			request.setCharacterEncoding("UTF-8");

			try {
				String result = DataDownload
						.downloadingAndSavingMarketDataInTheKnowledgeBase();
				response.setContentType("application/json");
				response.getWriter().write(result);
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
				String result = DataDownload
						.retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase(
								jsonString.split(","));
				response.setContentType("application/json");
				response.getWriter()
						.write(result.toString());
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
				String result = DataDownload
						.downloadingAndSavingExchangeRatesInTheKnowledgeBase();
				response.setContentType("application/json");
				response.getWriter()
						.write(result.toString());
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		}

	}
}
