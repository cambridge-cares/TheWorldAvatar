package uk.ac.cam.cares.jps.misc.performance;

import java.io.IOException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestHttp extends TestCase {
	
	public void printHelp() {
		System.out.println("\nTestHttp <host> <number HTTP requests> <query value size> <write to disk>");
		System.out.println("host - e.g. http:\\localhost:8080 or https:\\www.theworldavatar.com");
		System.out.println("number HTTP requests - integer > 0");
		System.out.println("query value size - integer >= 0, size of the String value in query component of HTTP request");
		System.out.println("write to disk - Booelan true or false; if true the requested servlet writes the query value to a file on disk");
		System.out.println("print to console - Booelan true or false; if true the requested servlet writes the query value to console output");
	}
	
	public void start(String[] args) throws IOException {
		
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		String host = args[0];
		int numberHttpRequests = Integer.valueOf(args[1]);
		int queryValueSize = Integer.valueOf(args[2]);
		boolean writeToDisk = Boolean.valueOf(args[3]);
		boolean printToConsole = Boolean.valueOf(args[4]);
		performHttpRequests(host, numberHttpRequests, queryValueSize, writeToDisk, printToConsole);
	}
	
	public void testStart() throws IOException {
		String[] args = new String[] {"http://localhost:8080", "100", "1000", "false", "false"};
		start(args);
	}
	
	public void performHttpRequests(String host, int numberHttpRequests, int querValueSize, boolean writeToDisk, boolean printToConsole) {
		
		JSONObject jo = new JSONObject();
		jo.put("writetodisk", writeToDisk);
		jo.put("printtoconsole", printToConsole);
		jo.put("testkey", getValue(querValueSize));
		String json = jo.toString();
				
		long start = System.currentTimeMillis();
		
		for (int i=1; i<=numberHttpRequests; i++) {
			System.out.println("Call " + i);
			String url = host + "/JPS_MISC/testperformance/test";
			String result = AgentCaller.executeGetWithURLAndJSON(url, json);
			//System.out.println(result);
		}
		
		long stop = System.currentTimeMillis();

		System.out.println("elapsed time in milli = " + (stop - start));
		
	}
	
	private String getValue(int queryValueSize) {
		StringBuffer b = new StringBuffer();
		for (int i=0; i<=queryValueSize; i++) {
			b.append("x");
		}
		return b.toString();
	}
}
