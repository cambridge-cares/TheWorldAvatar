package uk.ac.cam.cares.jps.chatbotwrappers;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.util.ArrayList;
import java.util.Iterator;

public class MakeRequest {

	
	public static String send(ArrayList<String> inputs, ArrayList<String> keys, String url) {
		
		
		String data = "?";
		Iterator<String> it_input = inputs.iterator();
		Iterator<String> it_key = keys.iterator();
		
		while (it_key.hasNext() && it_input.hasNext()) { // iterate through both keys and data, join them .
		    String key = it_key.next();
		    String input = it_input.next(); 
		    
		    String part = key + "=" + input ;
		    data = data + part; 
		}
		
		var client = HttpClient.newHttpClient();

		// create a request
		var http_request = HttpRequest.newBuilder(
		       URI.create(url + data))
		   .header("accept", "application/json")
		   .build();

		// use the client to send the request
		try {
			String http_response = client.send(http_request, null).toString();
			return http_response;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
		
		 
	}
	
}
