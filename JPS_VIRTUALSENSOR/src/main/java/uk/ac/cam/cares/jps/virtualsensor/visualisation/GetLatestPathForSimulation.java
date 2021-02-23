package uk.ac.cam.cares.jps.virtualsensor.visualisation;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

/**
 * Servlet implementation class GetLastestPathForSimulation
 */
@WebServlet(urlPatterns = {"/adms/results/latest", "/episode/results/latest"})
public class GetLatestPathForSimulation extends HttpServlet {
    private static final long serialVersionUID = 1L;
    public static final String dataseturl = KeyValueManager.get(IKeys.DATASET_META_URL);
    public String LATEST_RESULTS_PATH = "/results/latest";
    public String ADMS_VENDOR = "ADMS";
    public String EPISODE_VENDOR = "Episode";
    public String ADMS_PATH = "/"+ADMS_VENDOR.toLowerCase() + LATEST_RESULTS_PATH;
    public String EPISODE_PATH = "/"+EPISODE_VENDOR.toLowerCase() + LATEST_RESULTS_PATH;

    /**
     * @see HttpServlet#HttpServlet()
     */
    public GetLatestPathForSimulation() {
        super();
    }


    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String path = request.getServletPath();
        System.out.println("PATH="+path);
        System.out.println("ADMSPATH="+ADMS_PATH);
        System.out.println("EPISODEPATH="+EPISODE_PATH);
        JSONObject r = AgentCaller.readJsonParameter(request);
        String cityIri = r.getString("city");
        String query_latest_path = "";
        if (path.contentEquals(ADMS_PATH)) {
        	System.out.println("goes to adms");
            query_latest_path = getPathQuery(ADMS_VENDOR, cityIri);
        } else if (path.contentEquals(EPISODE_PATH)) {
        	System.out.println("goes to episode");
            query_latest_path = getPathQuery(EPISODE_VENDOR, cityIri);
            
        }
        
        String result = KnowledgeBaseClient.query(dataseturl, null, query_latest_path);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        String directory = listmap.get(0)[0];
        response.getWriter().write(directory);

    }

    private String getPathQuery(String vendor, String cityIri) {        
		String query_latest_path = "Prefix dcterms:<http://purl.org/dc/terms/>\r\n" + "\r\n" 
		+ "Select ?s ?x \r\n"
				+ "Where{\r\n" 
		+ "  ?s dcterms:creator <http://www.theworldavatar.com/kb/agents/Service__"+vendor+".owl#Service> .\r\n" 
		+ "   ?s dcterms:created ?x .\r\n"
				+ "  ?s dcterms:subject <" + cityIri + "> .\r\n" 
		+ "  \r\n" + "  \r\n"
				+ "} ORDER BY DESC (?x) Limit 1";
		System.out.println("latest query= "+query_latest_path);
        return query_latest_path;
    }

    /**
     * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
     */
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // TODO Auto-generated method stub
        doGet(request, response);
    }

}
