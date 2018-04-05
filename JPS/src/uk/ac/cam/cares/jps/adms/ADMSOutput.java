package uk.ac.cam.cares.jps.adms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.gson.Gson;

/**
 * Servlet implementation class ADMSOutput
 */
@WebServlet("/ADMSOutput")
public class ADMSOutput extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSOutput() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		request.setCharacterEncoding("UTF-8");
		String jsonString = request.getParameter("coordinatesLonLat");
		
		Gson g = new Gson();
		
		String[] cmd = {
			"python",
			"C:\\Users\\WE\\Desktop\\JPS\\JParkSimulator-git\\JPS\\python\\caresjpsadmsinputs\\ADMSOutput.py",
			g.toJson(jsonString)
		};
		
		Process p = Runtime.getRuntime().exec(cmd);
		
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String result = stdInput.readLine();
		
		response.setContentType("application/json");
		response.getWriter().write(result);
	}

}
