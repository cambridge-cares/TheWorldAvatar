package uk.ac.cam.cares.jps.ship;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet("/GetHKUPollutionData")
public class HKUPollutionRetriever extends JPSHttpServlet  {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		
		String value = request.getParameter("query");
		JSONObject input = new JSONObject(value);
		
		
		
		JSONObject resultoftaking = new JSONObject();
		resultoftaking.put("pollutiondata", requestlatestdata());
		response.getWriter().write(resultoftaking.toString()); 
		
	}
	
	public String requestlatestdata() {
		// it will see from the postgresql which is the latest and give the latest pollution data recorded
	
		String result="0";
	
		return result;
	}
	

}
