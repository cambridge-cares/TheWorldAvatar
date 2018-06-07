package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;
import java.net.URLEncoder;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

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
		String coordinates = request.getParameter("coordinates");
		String powerPlantStartUrl = "http://localhost/JPS/PowerPlantWrapperAgent";
		HttpUriRequest request1 = new HttpGet(powerPlantStartUrl);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request1);
		String responseString = EntityUtils.toString(httpResponse.getEntity());


		
		String requestToADMSWrapper = "http://localhost/JPS/ADMSWrapper?selectedSource=" + URLEncoder.encode("http://www.theworldavatar.com/Plant-001.owl", "UTF-8") 
																		  + "&buildingTopNode=" 
																		  + "&coordinates="   + URLEncoder.encode(coordinates, "UTF-8") 
																		  + "&substances=" + URLEncoder.encode("CO2", "UTF-8")
																		  + "&buildingLimit=2"
																		  + "&bldNumber=25"
																		  + "&filterSource=false";
		
		HttpUriRequest request2 = new HttpGet(requestToADMSWrapper);
		HttpResponse httpResponse2 = HttpClientBuilder.create().build().execute(request2);
		String responseString2 = EntityUtils.toString(httpResponse2.getEntity());
 
		
		// response.getWriter().write(responseString2);

		
		/*
		 * 'http://localhost:8080/JPS/ADMS/ADMSWrapper?selectedSource=' + encodeURIComponent('http://www.theworldavatar.com/Plant-001.owl') + '&buildingTopNode=' + null + '&coordinates=' + encodeURIComponent(JSON.stringify(coordinate))
+ '&substances=' + encodeURIComponent(substances) + '&buildingLimit=' + encodeURIComponent(buildingLimit) + '&filterSource=' + encodeURIComponent(sourceFilter)
		 */

		
		coordinates = coordinates.replaceAll(",", "#");
		String buildingsIRI = "http://localhost/JPS/ADMSGetBuildingsIRI?coordinates=" +  URLEncoder.encode(coordinates, "UTF-8") ;
		System.out.println(buildingsIRI);
		HttpUriRequest request3 = new HttpGet(buildingsIRI);
		HttpResponse httpResponse3 = HttpClientBuilder.create().build().execute(request3);
		String responseString3 = EntityUtils.toString(httpResponse3.getEntity());
		
		response.getWriter().write(responseString3.replace("'","\""));
		
		String startADMSRequets = "http://localhost/JPS/ADMSStarter";
		HttpUriRequest request4 = new HttpGet(startADMSRequets);
		HttpResponse httpResponse4 = null;
		try {
			httpResponse4 = HttpClientBuilder.create().build().execute(request4);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
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
