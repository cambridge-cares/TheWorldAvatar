package uk.ac.cam.cares.jps.ship;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

import com.google.gson.Gson;

/**
 * Servlet implementation class GetShipListFromRegion
 */
@WebServlet("/GetShipListFromRegion")
public class GetShipListFromRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public GetShipListFromRegion() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		response.setContentType("application/json");
		String[] arrayOfShipIRIs = { 
				"http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1",
//		        "http://www.theworldavatar.com/kb/ships/Ship-2.owl#Ship-2",
//		        "http://www.theworldavatar.com/kb/ships/Ship-3.owl#Ship-3",
//		        "http://www.theworldavatar.com/kb/ships/Ship-4.owl#Ship-4",
//		        "http://www.theworldavatar.com/kb/ships/Ship-5.owl#Ship-5",
//		        "http://www.theworldavatar.com/kb/ships/Ship-6.owl#Ship-6",
//		        "http://www.theworldavatar.com/kb/ships/Ship-7.owl#Ship-7",
//		        "http://www.theworldavatar.com/kb/ships/Ship-8.owl#Ship-8",
//		        "http://www.theworldavatar.com/kb/ships/Ship-9.owl#Ship-9",
//		        "http://www.theworldavatar.com/kb/ships/Ship-10.owl#Ship-10"
		};
		
//		String[] arrayOfShipIRIs1 = { 
//				"http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1"
//		};
		
//		Gson g = new Gson();
//		response.getWriter().write(g.toJson(arrayOfShipIRIs));
		
		JSONObject result = new JSONObject();
		try {
			result.put("shipIRIs", arrayOfShipIRIs);
//			result.put("shipIRIs", arrayOfShipIRIs1);
			response.getWriter().write(result.toString());
		} catch (JSONException e) {
			e.printStackTrace();
		}
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
