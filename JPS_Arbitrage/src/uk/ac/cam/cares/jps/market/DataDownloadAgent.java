package uk.ac.cam.cares.jps.market;


import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;
/**
 * Servlet implementation class DataDownloadAgent
 */
@WebServlet(urlPatterns = {"/download", "/download2", "/read"})
public class DataDownloadAgent extends HttpServlet {
	private static final long serialVersionUID = 2L; //??
    
    /**
     * @see HttpServlet#HttpServlet()
     */
    public DataDownloadAgent() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			
		/**this function is a servlet for calling functions in uk.ac.cam.cares.jps.market package and returning their results;
		 * it discriminates between "/download", "/download2" and "/read" URL patterns and calls DataDownload.Downloading_market_data,
		 * DataDownload.Downloading_currencies and DataDownload.Call_data, respectively
		 */
		
		Gson gson = new Gson();
		
		String path = request.getServletPath();
	
		if ("/download".equals(path)) {
		
			// -- Get String formatted in Array of Strings -- //
			request.setCharacterEncoding("UTF-8");
			
			try {
				String result = gson.toJson(DataDownload.Downloading_market_data());
				response.setContentType("application/json");
				response.getWriter().write(result);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}	
		
		} else if ("/read".equals(path)) {
			
			// -- Get String formatted in Array of Strings -- //
			request.setCharacterEncoding("UTF-8");
			String jsonString = request.getParameter("individuals");
			
			try {
				String result = DataDownload.Call_data(jsonString.split(","));
				response.setContentType("application/json");
				response.getWriter().write(result.toString());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}	
		} else if ("/download2".equals(path)) {
			
			// -- Get String formatted in Array of Strings -- //
			request.setCharacterEncoding("UTF-8");
			
			try {
				String result = DataDownload.Downloading_currencies();
				response.setContentType("application/json");
				response.getWriter().write(result.toString());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}	
		}
		
	}
}
