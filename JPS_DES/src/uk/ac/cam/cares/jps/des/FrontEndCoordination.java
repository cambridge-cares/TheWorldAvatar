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

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/showDESResult"})

public class FrontEndCoordination extends JPSHttpServlet{

	private static final long serialVersionUID = 1L;

	@Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response);
    }

    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	 JSONObject responseParams = requestParams;
    	 String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data";
			String directorychosen= getLastModifiedDirectory(new File(dir));
	    	logger.info("latest directory= "+directorychosen);
	    	DistributedEnergySystem a = new DistributedEnergySystem();
	    	responseParams = a.provideJSONResult(directorychosen);

 	        JSONObject jo = new JSONObject();
 	        String[] types = {"solar", "gridsupply", "industrial", "commercial", "residential"};
 	        List<String> l = Arrays.asList(types);
 	        for (String i: l ) {
 	        	System.out.println(responseParams.get(i));
 	        	JSONArray j =  (JSONArray) responseParams.get(i);
 	        	jo.put(i,j.get(0));
 	        }
 	        System.out.println(jo);
 			String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
 			System.out.println("Called GetBlock" + v);
 			JSONObject tempJO = new JSONObject(v);
 			responseParams.put("txHash", tempJO.get("txHash"));
 			responseParams.put("sandr", tempJO.get("sandr"));
 			System.gc();
 	    		 
 			
    	return responseParams;
    }
    public static String getLastModifiedDirectory(File directory) {
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
						System.out.println("it goes here");
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
