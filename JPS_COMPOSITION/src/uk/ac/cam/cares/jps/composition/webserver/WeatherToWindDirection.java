package uk.ac.cam.cares.jps.composition.WebServer;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

@WebServlet("/WeatherToWindDirection")
public class WeatherToWindDirection extends HttpServlet {
	private static final long serialVersionUID = 1L;

    public WeatherToWindDirection() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		try {
			JSONObject input = new JSONObject(request.getParameter("query"));			
			System.out.println("Input : " + input);
			JSONObject currentWeatherState = input.getJSONObject("currentweatherstate");
			JSONObject wind = currentWeatherState.getJSONObject("haswind");
			String direction = wind.getString("direction");
			
			JSONObject result = new JSONObject();
			result.put("winddirection", direction);
			response.getWriter().write(result.toString());
			
			
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
		
		
	}


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
