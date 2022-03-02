package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang3.ArrayUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Outstream and Errstream of exe might block each other. Use async to avoid
 * this issue.
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-16
 */
public class AsyncPythonHelper {
	private static Logger logger = LoggerFactory.getLogger(PythonHelper.class);

	private  String ENV_FOLDER; //"jps_ontomatch";
	private String PATH_TO_PY = "python";///Scripts/python.exe
	private boolean useVenv = false;
    private static AsyncPythonHelper pyexe = null;
    
    AsyncPythonHelper(){
    	
    }
    
    AsyncPythonHelper(String venvname){
    	PATH_TO_PY = "/Scripts/python.exe";
    	ENV_FOLDER = venvname;
    	useVenv = true;
    }
	
    public static AsyncPythonHelper getInstance(String venvname) {
    	if(pyexe==null) {//instance not initiated
    		if(venvname == null) {
    			pyexe = new AsyncPythonHelper();
    		} else {
    			pyexe = new AsyncPythonHelper(venvname);    			
    		}
    	} 
    	return pyexe;
	}
	
	
    
    
	 public  String[] callPython(String pythonScriptName, String[] parameters, Object thisObject)
			throws IOException {
		String path = AgentLocator.getNewPathToPythonScript(pythonScriptName, thisObject);
	    String py_exe;
		if(useVenv) {
		String venvLoc = AgentLocator.getNewPathToPythonScript(ENV_FOLDER, thisObject);
		 py_exe = setVirtualEnv(venvLoc);
	    }
		py_exe  = PATH_TO_PY;
		String[] head = { py_exe, path };
		String[] cmd = (String[]) ArrayUtils.addAll(head, parameters);
		for (String c : cmd) {
			System.out.println(c);
		}
		String[] result = null;
		try {
			result = processCommand(cmd);
		} catch (IOException e) {
			throw new JPSRuntimeException(e);
		} catch (InterruptedException e) {
			throw new JPSRuntimeException(e);
		} catch (ExecutionException e) {
			throw new JPSRuntimeException(e);
		}
		return result;
	}

	public  JSONObject callPython(String actionSuccessFlag, String pythonScriptName, String[] parameters,
			Object thisObject) throws Exception {
	    //AsyncPythonHelper.checkCmds(parameters);
		String[] results = callPython(pythonScriptName, parameters, thisObject);
		JSONObject resultObj = new JSONObject();
		if (results[0].contains(actionSuccessFlag)) {
			System.out.println(results[0]);
			resultObj.put("success", true);
		} else {
			resultObj.put("error", results[1]);
		}
		return resultObj;
	}

	//TODO
	
	
	public static String callCommandFrom(String[] cmds, String dir) throws IOException {
		List<String> cmdList = new ArrayList<String>(Arrays.asList(cmds));
		ProcessBuilder pb = new ProcessBuilder(cmdList);
		pb.directory(new File(dir));
		Process pr = pb.start();
		BufferedReader br = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String str = null;
		while ((str = br.readLine()) != null) {
			System.out.println(str);
		}

		return str;
	}

	public static void checkCmds(String[] cmds) {
		String appendix = "file:///";
		for(int i=0; i<cmds.length; i++) {
	        String c  =cmds[i];
			if(c.toLowerCase().contains("c:/") && !c.toLowerCase().contains("file:")) {
				cmds[i] = appendix+c;
			}
		}
	}
	public static String[] processCommand(String[] cmd) throws IOException, InterruptedException, ExecutionException {
		ExecutorService executor = Executors.newFixedThreadPool(2);// excecutor of async reading of output streams
		Process p = Runtime.getRuntime().exec(cmd);// process to execute

		Future<String> promiseErrOutput = executor.submit(getStreamResult(p.getErrorStream()));
		Future<String> promiseInputOutput = executor.submit(getStreamResult(p.getInputStream()));
		String[] results = new String[2];
		results[0] = promiseInputOutput.get();
		results[1] = promiseErrOutput.get();

		return results;
	};
	
   
	public  String setVirtualEnv(String venvloc) {
	String ventScriptLoc = getVenvScriptLocation( venvloc);
	return ventScriptLoc;	
	}
	
	public  String getVenvScriptLocation(String venvloc) {
		try {

		String loc = venvloc+PATH_TO_PY;
		loc = loc.replace("\\", "/");
		System.out.println(loc);
		return loc;
		} catch(Exception e) {
			throw new JPSRuntimeException(e);
		}
		
	}

	private static Callable<String> getStreamResult(InputStream inputStream) {
		Callable<String> callableTask = () -> {
			String line = "";
			try {
				InputStreamReader isr = new InputStreamReader(inputStream);
				BufferedReader br = new BufferedReader(isr);
				String newline;
				while ((newline = br.readLine()) != null) {
					line += newline + "\r\n";
				}

			} catch (Exception e) {
				logger.error("", e.getMessage(), e);
			}
			return line;
		};
		return callableTask;
	}

}
