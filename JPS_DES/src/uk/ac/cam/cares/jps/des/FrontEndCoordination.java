package uk.ac.cam.cares.jps.des;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = { "/showDESResult"})

public class FrontEndCoordination  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	 	JSONObject responseParams = requestParams;
	    	String directorychosen= getLastModifiedDirectory();
	    	logger.info("latest directory= "+directorychosen);
	    	JSONObject jo = new JSONObject();
	    	jo.put("baseUrl", directorychosen);
 			String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
 			System.gc();
 			responseParams = new JSONObject(v);
 	    		 
 			
    	return responseParams;
    }
    /**
     * Gets the latest file created using rdf4j
     * @return last created file
     */
    public String getLastModifiedDirectory() {
    	String agentiri = "http://www.theworldavatar.com/kb/agents/Service__DESAgent.owl#Service";
		List<String> lst = null;
    	String resultfromfuseki = MetaDataQuery.queryResources(null,null,null,agentiri, null, null,null,lst);
		 String[] keys = JenaResultSetFormatter.getKeys(resultfromfuseki);
		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromfuseki, keys);
    	return listmap.get(0)[0];
    }

    
    /*
     * Gets the latest file without using rdf4j. Needs to input the string location of the base folder
     */
    public static String getLastModifiedDirectoryOld(File directory) {
		File[] files = directory.listFiles();
		if (files.length == 0)
			return directory.getAbsolutePath();
		Arrays.sort(files, new Comparator<File>() {
			public int compare(File o1, File o2) {
				return new Long(o2.lastModified()).compareTo(o1.lastModified()); // latest 1st
			}
		});
		File filechosen = new File("");

		outerloop: for (File file : files) {
			String[] x = file.list();
			if (x[0].contentEquals("JPS_DES")) {
				File[] childfile = file.listFiles();
				for (File filex : childfile) {
					String[] y = filex.list();
					List<String> list = Arrays.asList(y);

					// System.out.println("size= "+list.size()+" ,listcontent= "+list.get(0));
					if (list.contains("totgen.csv") && list.contains("rh1.csv")) {
						filechosen = file;
						break outerloop;
					}
					// System.out.println("directory last date="+file.lastModified());

				}
				// break;
			}
		}
		return filechosen.getAbsolutePath() + "/JPS_DES";
	}
    
}
