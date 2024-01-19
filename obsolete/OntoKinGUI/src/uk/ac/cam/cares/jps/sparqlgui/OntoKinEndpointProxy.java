package uk.ac.cam.cares.jps.sparqlgui;

import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.query.KGRouter;

/**
 * Servlet implementation class OntoKinEndpointProxy
 */
@WebServlet("/OntoKinEndpointProxy")
public class OntoKinEndpointProxy extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final String ONTOKIN_KB = "http://kb/ontokin";
    public OntoKinEndpointProxy() {
        super();
    }
    
    /**
     * Reads data from the JPS knowledge graph.
     * 
     * @param request
     * @param response
     */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String queryString = request.getParameter("queryString");
		String queryResult = null;
		try {
			queryResult = queryKnoweldgeGraph(ONTOKIN_KB, queryString);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		response.getWriter().write(queryResult);
	}

	/**
	 * Updates and inserts data into the JPS knowledge graph.
	 * 
	 * @param request
	 * @param response
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	/**
	 * Performs a query against the JPS knowledge graph.
	 * 
	 * @param KB_IRI permanent IRI of a JPS knowledge base, e.g. for the OntoKin knowledge base it is http://kb/ontokin.
	 * @param mechanismquery
	 * @return
	 * @throws JSONException
	 * @throws UnsupportedEncodingException
	 */
	public static String queryKnoweldgeGraph(String KB_IRI, String mechanismquery) throws JSONException, UnsupportedEncodingException {
		JSONArray result = null;
		System.out.println("Query:"+mechanismquery);
		KnowledgeBaseClientInterface kbClientInterface = KGRouter.getKnowledgeBaseClient(KB_IRI, true, false);
		result = kbClientInterface.executeQuery(mechanismquery);
		if(result==null){
			return null;
		}
		return result.toString();
	}
}
