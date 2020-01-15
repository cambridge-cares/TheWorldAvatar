package uk.ac.cam.cares.jps.des;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
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

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;




@WebServlet(urlPatterns = { "/DESAgent", "/showDESResult"})
/*
 * Wrapper for the python agent and displaying the result
 */
public class DistributedEnergySystem extends JPSHttpServlet {
	public static final String SIM_START_PATH = "/DESAgent";
	public static final String SIM_SHOW_PATH = "/showDESResult";
	public static String weather="Weather.csv";
	public static String schedule="ApplianceScheduleLoad1.csv";
	
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String bcap="bcap.csv";
	public static String unwill="unwill.csv";
	
	public static String producerdata="PV_parameters.csv";
	public static String consumerdata1="FuelCell.csv";
	
	
    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response);
    }

    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response, JSONObject reqBody) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response, reqBody);
    }

    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	String path = request.getServletPath();
    	 JSONObject responseParams = requestParams;
    	 if (SIM_START_PATH.equals(path)) {
    	    	
    		 JSONObject jo = AgentCaller.readJsonParameter(request);
    		String baseUrl = jo.getString("baseUrl");
    		
    			try {
    				runOptimization(baseUrl);
    			} catch (IOException e) {
    				// TODO Auto-generated catch block
    				logger.error(e.getMessage());
    				e.printStackTrace();
    			} catch (InterruptedException e) {
    				// TODO Auto-generated catch block
    				logger.error(e.getMessage());
    				e.printStackTrace();
    			} catch (Exception e) {
    				// TODO Auto-generated catch block
    				logger.error(e.getMessage());
    				e.printStackTrace();
    			}
    			responseParams = provideJSONResult(baseUrl);
    		 
    	 }
    	 
    	 else if(SIM_SHOW_PATH.contentEquals(path)) {
    		 String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data";
    		 String directorychosen=getLastModifiedDirectory(new File(dir));
    		 logger.info("latest directory= "+directorychosen);
    		 responseParams = provideJSONResult(directorychosen);
    	 }
    		
		return responseParams;
    	
    }
    
    public static String getLastModifiedDirectory(File directory) {
        File[] files = directory.listFiles();
        if (files.length == 0) return directory.getAbsolutePath();
        Arrays.sort(files, new Comparator<File>() {
            public int compare(File o1, File o2) {
                return new Long(o2.lastModified()).compareTo(o1.lastModified()); //latest 1st
            }});
        File filechosen= new File("");

    	outerloop:
        for(File file:files) {
        	String[] x=file.list();
            if(x[0].contentEquals("JPS_DES")) {
            	File[]childfile=file.listFiles();
            	for(File filex:childfile) {
            		String[] y=filex.list();
            		List<String> list = Arrays.asList(y);
            		
            		//System.out.println("size= "+list.size()+"  ,listcontent= "+list.get(0));
            			if(list.contains("totgen.csv")&&list.contains("rh1.csv")) {
            				System.out.println("it goes here");
            				filechosen=file;
                			break outerloop;
            			}
            			//System.out.println("directory last date="+file.lastModified());
            			
            		
            	}
            	//break;
            }
        }
        return filechosen.getAbsolutePath()+"/JPS_DES";
    }

	public JSONObject provideJSONResult(String baseUrl) {
		JSONObject responseParams;
		String weatherdir=baseUrl+"/Weather.csv";
		String content = new QueryBroker().readFileLocal(weatherdir);
		List<String[]> weatherResult = MatrixConverter.fromCsvToArray(content);
		
		String powerdir=baseUrl+"/totgen.csv";
		String content2 = new QueryBroker().readFileLocal(powerdir);
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(content2);
		JSONObject dataresult= new JSONObject();

		String rhdir=baseUrl+"/rh1.csv";
		String content3 = new QueryBroker().readFileLocal(rhdir);
		List<String[]> rhResult = MatrixConverter.fromCsvToArray(content3);
		
		JSONArray temperature=new JSONArray();
		JSONArray irradiation=new JSONArray();
		
		//25-48 (last 24)
		int sizeofweather=weatherResult.size();
		for (int x=sizeofweather-24;x<sizeofweather;x++) {
			temperature.put(weatherResult.get(x)[4]);
			irradiation.put(weatherResult.get(x)[8]);
		}

		//log to check if it's reading the right one. x
		
		dataresult.put("temperature", temperature);
		dataresult.put("irradiation", irradiation);
		dataresult.put("fuelcell", simulationResult.get(3));
		dataresult.put("residential", simulationResult.get(0));
		dataresult.put("industrial", simulationResult.get(2));
		dataresult.put("building", simulationResult.get(1));
		dataresult.put("rh1",rhResult.subList(0, 3).toArray());
		dataresult.put("rh2",rhResult.subList(3, 6).toArray());
		dataresult.put("rh3",rhResult.subList(6, rhResult.size()).toArray());
		
		responseParams=dataresult;
		return responseParams;
	}
    	
	

	public void runOptimization(String baseUrl) throws Exception {


		copyFromPython(baseUrl, "runpy.bat");
		copyFromPython(baseUrl, "Receding_Horizon_Optimization_V.py");

		String startbatCommand = baseUrl + "/runpy.bat";
		String result = executeSingleCommand(baseUrl, startbatCommand);
		logger.info("final after calling: " + result);

	}

	public void copyFromPython(String baseUrl,String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/python/"+filename);
		
		String destinationUrl = baseUrl + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	
	public String executeSingleCommand(String targetFolder, String command) throws InterruptedException {

		//logger.info("In folder: " + targetFolder + " Excuted: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds
			pr.waitFor();														// will be executed within such folder.

		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {

			while ((line = bfr.readLine()) != null) {
				resultString += line;

			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		return resultString;
	}
	
	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
    
	
    
    
    
    

}
