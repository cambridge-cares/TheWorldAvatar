package uk.ac.cam.cares.jps.des;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
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
    	 	
			String directorychosen= getLastModifiedDirectory();
	    	logger.info("latest directory= "+directorychosen);
	    	DistributedEnergySystem a = new DistributedEnergySystem();
	    	responseParams = a.provideJSONResult(directorychosen);
	    	
 	        JSONObject jo = determineValue(responseParams);
 		    System.out.println(jo.toString());
 			String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
 			System.out.println("Called GetBlock" + v);
 			JSONObject tempJO = new JSONObject(v);
 			responseParams.put("txHash", tempJO.get("txHash"));
 			responseParams.put("sandr", tempJO.get("sandr"));
 			System.gc();
 	    		 
 			
    	return responseParams;
    }
    /*
     * Gets the latest file created using rdf4j
     */
    public static String getLastModifiedDirectory() {
    	String agentiri = "http://www.theworldavatar.com/kb/agents/Service__DESAgent.owl#Service";
    	String resultfromfuseki = MetaDataQuery.queryResources(null,null,null,agentiri, null, null,null,null);
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
    /*
     * Derive and estimate the values to be transacted over the blockchain, to the closest half an hour. 
     */
    
    public static JSONObject determineValue (JSONObject responseParams) throws JSONException {

		JSONObject jo = new JSONObject();
    	try {
		SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
		String date = sdf.format(new Date());
		Date date2 = sdf.parse(date);
		//figure out time in which 
		String[] tim = (String[]) responseParams.get("timer");
		for (int i = 0; i< tim.length; i++) {
			Date date1;
				date1 = sdf.parse(tim[i]);
				// TODO Auto-generated catch block
			long difference = date2.getTime() - date1.getTime();
			difference = difference/60000;
			if (difference < 29) {
				//need to figure out the difference gradient
				jo = deriveValue(i, false, responseParams);
				break;
			}
			else if (difference < 60){
				//get the next one. 
				jo = deriveValue(i, true, responseParams);
				break;
			}
		}
    	
    	}catch (Exception ex) {
    		ex.printStackTrace();
    	}

		return jo;
    }
    /*
     * helper function to determineValue()s
     */
    public static JSONObject deriveValue(int index, boolean inbetween, JSONObject responseParams) {

    	JSONObject jo = new JSONObject();
    	String[] types = {"solar", "gridsupply", "industrial", "commercial", "residential"};
    	List<String> l = Arrays.asList(types);
    	if (inbetween) {
    		for (String i: l ) {
    			String[] j =  (String[]) responseParams.get(i);
    			jo.put(i,j[index]);
    		}
        }else {
        	for (String i: l ) {
        		String[] j  =  (String[]) responseParams.get(i);
    			double getAvg = ( Double.parseDouble(j[index]) + Double.parseDouble(j[index+1])) /2;
    			jo.put(i,Double.toString(getAvg));
    		}
        }
        return jo;
    }
}
