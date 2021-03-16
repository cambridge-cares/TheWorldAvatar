package uk.ac.ceb.como.properties;

import java.io.BufferedReader;
import java.io.IOError;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

/**
 * HTTP Requests can be performed to get data published at the target of<br> 
 * different URLs.  
 * 
 * @author msff2
 *
 */
public class Request {
	/**
	 * Enables to perform an HTTP get request.
	 * 
	 * @author msff2@cam.ac.uk (Dr Feroz Farazi)
	 * @param query
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	public static String get(String query) throws MalformedURLException, IOException{
        
		URL httpURL = new URL(query);
        
        URLConnection httpURLConnection = httpURL.openConnection();
        
        BufferedReader in = new BufferedReader(
                                new InputStreamReader(
                                		httpURLConnection.getInputStream()));
        String inputLine;
        
        String fileContent = "";
        
        while ((inputLine = in.readLine()) != null){ 
            fileContent = fileContent.concat(inputLine);
        }
        
        in.close();
        System.out.println("fileContent:\n"+fileContent);
        
        return fileContent;
    }
}
