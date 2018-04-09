package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.adms.HttpGet;
import uk.ac.cam.cares.jps.adms.HttpResponse;
import uk.ac.cam.cares.jps.adms.HttpUriRequest;

/**
 * Servlet implementation class ADMSCoordinationAgent
 */
@WebServlet("/ADMSCoordinationAgent")
public class ADMSCoordinationAgent extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSCoordinationAgent() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		String iri = request.getParameter("IRI");
		String powerPlantStartUrl = "http://www.theworldavatar.com/JPS/PowerPlant/PowerPlantStarter?IRI=" + iri;
		HttpUriRequest request = new HttpGet(url);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String responseString = EntityUtils.toString(httpResponse.getEntity());
		System.out.println(responseString);

		try {
			response.getWriter().append(responseString) ;
		} catch (IOException e) {
			// TODO Auto-generated catch block
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
