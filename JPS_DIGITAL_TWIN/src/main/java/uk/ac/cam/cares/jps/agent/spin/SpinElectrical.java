package uk.ac.cam.cares.jps.agent.spin;

import java.io.File;
import java.util.List;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.matlab.JPSMatlabAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

@WebServlet("/SpinElectrical")
public class SpinElectrical extends JPSAgent {
  private static final long serialVersionUID = 1L;

  public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    JSONObject jo = AgentCaller.readJsonParameter(request);
    String current = System.getProperty("user.home");
    System.out.println("\nThis is Spin agent:" + current);
    String frequencyFilePath = null;
    String agentiri = JPSMatlabAgent.MATLAB_AGENT_URL;
    List<String> lst = null;
    SpinElectrical iri = new SpinElectrical();
    frequencyFilePath = iri.queryRDF4J(agentiri, lst);
    System.out.println("\nThis is frequencyFilePath: " + frequencyFilePath);
    return jo;
  }

  /**
   * Query RDF4J for electrical system output IRI.
   */
  public String queryRDF4J(String agentiri, List<String> lst) {
    String csvFilePath = null;
    String resultFromRDF4J =
        MetaDataQuery.queryResources(null, null, null, agentiri, null, null, null, lst);
    String[] keys = JenaResultSetFormatter.getKeys(resultFromRDF4J);
    List<String[]> listmap =
        JenaResultSetFormatter.convertToListofStringArrays(resultFromRDF4J, keys);
    for (String[] str : listmap) {
      for (String s : str) {
        if (isFile(s)) {
          csvFilePath = s;
          System.out.println("\nThis is csvFilePath: " + csvFilePath);
          break;
        }
      }
      break;
    }
    return (csvFilePath);
  }

  /**
   * Validate file path.
   */
  private boolean isFile(String path) {
    if (path == null) {
      return true;
    }
    return new File(path).isFile();
  }
}
