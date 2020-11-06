package uk.ac.cam.cares.jps.market;

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
 * Servlet implementation class DataDownloadAgent
 */
@WebServlet(urlPatterns = {
		"/downloadingAndSavingMarketDataInTheKnowledgeBase",
		"/downloadingAndSavingExchangeRatesInTheKnowledgeBase",
		"/savingDataInTheKnowledgeBase",
		"/retrievePlantSpecificParam",
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
		
		//condition 1 and write the result in json format and show it
		if ("/downloadingAndSavingMarketDataInTheKnowledgeBase"
				.equals(path)) {

			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("choicePlant");

			try {
				String result = g.toJson(DataDownload.downloadingAndSavingMarketDataInTheKnowledgeBase(jsonString));
				System.out.println("result of downloadingAndSavingMarketDataInTheKnowledgeBase= "+result);
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error("error in download saving market data: "+e.getMessage());
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
				response.getWriter().write(result.toString());
				
			} catch (Exception e) {
				
				logger.error("error in download and saving exchange rate "+e.getMessage());
				throw new JPSRuntimeException(e.getMessage(), e);
				
			}
		} else if ("/savingDataInTheKnowledgeBase".equals(path)) {
			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("arrayHeaderPrices");
			
			try {
				System.out.println("resultfromdatadownload= "+jsonString);
				System.out.println("resultfromdatadownload2= "+DataDownload.savingDataInTheKnowledgeBase(jsonString));
				String result = g.toJson(DataDownload.savingDataInTheKnowledgeBase(jsonString));	
				System.out.println("resultinjson= "+result);
				response.setContentType("application/json");
				response.getWriter().write(result);
				
			} catch (Exception e) {
				
				logger.error("error in saving data in kb: "+e.getMessage());
				throw new JPSRuntimeException(e.getMessage(), e);
				
			}
		} else if ("/retrieveUtilityPrices".equals(path) 
				|| "/retrievePlantSpecificParam".equals(path)) {

			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("individuals");

			try {
				String result = g.toJson(DataDownload.retrievePrices(jsonString.split(",")));
				response.setContentType("application/json");
				response.getWriter().write(result);
				
			} catch (Exception e) {
				
				logger.error("error in retrieving utility: "+e.getMessage());
				throw new JPSRuntimeException(e.getMessage(), e);
				
			}
		} else if ("/retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase"
				.equals(path)) {

			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("individuals");

			try {
				String result = g.toJson(DataDownload
						.retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase(
								jsonString.split(",")));

				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error("error in retrieving utility: "+e.getMessage());
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
				logger.info(result);
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				logger.error("error in retrieving utility: "+e.getMessage());
				throw new JPSRuntimeException(
						e.getMessage(), e);
			}
		}
	}
}
