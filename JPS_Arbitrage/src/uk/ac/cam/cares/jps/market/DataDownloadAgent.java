package uk.ac.cam.cares.jps.market;

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
		"/downloadingAndSavingMarketDataInTheKnowledgeBase",
		"/downloadingAndSavingExchangeRatesInTheKnowledgeBase",
		"/savingDataInTheKnowledgeBase",
		"/retrieveUtilityPrices",
		"/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase",
		"/retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase"})
public class DataDownloadAgent extends HttpServlet {
	private static Logger logger = LoggerFactory.getLogger(DataDownloadAgent.class);

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

		Gson g = new Gson();
		
		if ("/downloadingAndSavingMarketDataInTheKnowledgeBase"
				.equals(path)) {

			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("choicePlant");

			try {
				String result = g.toJson(DataDownload.downloadingAndSavingMarketDataInTheKnowledgeBase(jsonString));

				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}

		} else if ("/downloadingAndSavingExchangeRatesInTheKnowledgeBase"
				.equals(path)) {

			request.setCharacterEncoding("UTF-8");

			try {
				String result = g.toJson(DataDownload
						.downloadingAndSavingExchangeRatesInTheKnowledgeBase());

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
				
				response.setContentType("application/json");
				response.getWriter()
						.write(result);
			} catch (Exception e) {
				logger.error(e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		} else if ("/retrieveUtilityPrices"
				.equals(path)) {

			request.setCharacterEncoding("UTF-8");
			String jsonString = request
					.getParameter("individuals");

			try {
				String result = g.toJson(DataDownload.retrieveUtilityPrices(jsonString.split(",")));
				
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
		}
	}
}
