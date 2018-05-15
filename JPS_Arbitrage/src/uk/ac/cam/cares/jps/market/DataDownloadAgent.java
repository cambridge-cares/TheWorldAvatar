package uk.ac.cam.cares.jps.market;


import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
/**
 * Servlet implementation class DataDownloadAgent
 */
@WebServlet(urlPatterns = {"/download", "/read"})
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
		
	
	String path = request.getServletPath();

	if ("/download".equals(path)) {
	
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("CPO_page");
		
		try {
			String result = DataDownload.Downloading_market_data(jsonString);
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
	}
	
	}
}
