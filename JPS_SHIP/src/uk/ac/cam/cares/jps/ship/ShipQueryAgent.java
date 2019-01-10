package uk.ac.cam.cares.jps.ship;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

import com.google.gson.Gson;

/**
 * Servlet implementation class ShipQueryAgent
 */
@WebServlet("/ShipQueryAgent")
public class ShipQueryAgent extends HttpServlet {
	private static final long serialVersionUID = 1L;

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");

		String[] arrayOfShipIRIs = { 
			"http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1",
	        "http://www.theworldavatar.com/kb/ships/Ship-2.owl#Ship-2",
	        "http://www.theworldavatar.com/kb/ships/Ship-3.owl#Ship-3",
	        "http://www.theworldavatar.com/kb/ships/Ship-4.owl#Ship-4",
	        "http://www.theworldavatar.com/kb/ships/Ship-5.owl#Ship-5",
	        "http://www.theworldavatar.com/kb/ships/Ship-6.owl#Ship-6",
	        "http://www.theworldavatar.com/kb/ships/Ship-7.owl#Ship-7",
	        "http://www.theworldavatar.com/kb/ships/Ship-8.owl#Ship-8",
	        "http://www.theworldavatar.com/kb/ships/Ship-9.owl#Ship-9",
	        "http://www.theworldavatar.com/kb/ships/Ship-10.owl#Ship-10"
		};
		
		Gson g = new Gson();
		String result;
		
		try {
			result = PythonHelper.callPython("caresjpsship/ShipGeoJSONGetter.py", 
					g.toJson(g.toJson(arrayOfShipIRIs)), this);
			
			response.getWriter().write(result);
		} catch (PythonException e) {
			e.printStackTrace();
		}		
	}
}
