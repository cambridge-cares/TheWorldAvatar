package uk.ac.cam.cares.jps.misc.http;

import java.io.IOException;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class HttpGet {
	
	public void printHelp() {
		System.out.println("\nHttpGet <url>");
		System.out.println("url - any url, e.g. http://www.theworldavatar.com/JPS_SHIP");
	}
	
	public void start(String[] args) throws IOException {
		
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		String url = args[0];
		performHttpGet(url);
	}
	
	private void performHttpGet(String url) {
		System.out.println("requesting " + url);
		String result = AgentCaller.executeGetWithURL(url);
		System.out.println("response:");
		System.out.println(result);
	}
}
