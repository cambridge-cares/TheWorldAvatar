package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

/**
 * Servlet implementation class GetLastestPathForSimulation
 */
@WebServlet("/GetLastestPathForSimulation")
public class GetLastestPathForSimulation extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public GetLastestPathForSimulation() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		JSONObject r = AgentCaller.readJsonParameter(request);
		String city_iri = r.getString("city");
		 
		String query_latest_path = "Prefix dcterms:<http://purl.org/dc/terms/>\r\n" + 
				"\r\n" + 
				"Select ?s\r\n" + 
				"Where{\r\n" + 
				"  ?s dcterms:creator ?o .\r\n" + 
				"   ?s dcterms:created ?x .\r\n" + 
				"  ?s dcterms:subject <"+ city_iri + "> .\r\n" + 
				"  \r\n" + 
				"  \r\n" + 
				"} ORDER BY DESC (?x) Limit 1";
		String result = KnowledgeBaseClient.query("http://localhost/rdf4j-server/repositories/jpsmetadata", null, query_latest_path);
		response.getWriter().write(result);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
