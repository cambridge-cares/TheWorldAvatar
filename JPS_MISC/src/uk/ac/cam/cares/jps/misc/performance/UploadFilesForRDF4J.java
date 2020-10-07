package uk.ac.cam.cares.jps.misc.performance;

import java.io.File;
import java.io.IOException;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.FileEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import junit.framework.TestCase;

public class UploadFilesForRDF4J extends TestCase {
	
	public void printHelp() {
		System.out.println("\nUploadFiles <url> <file or dir>");
		System.out.println("example url for rdf4j  = http://localhost:8080/rdf4j-server/repositories/<dataset name>/statements");
	}
	
	public void start(String[] args) throws IOException {
		
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		String datasetUrl = args[0];
		String fileOrDir = args[1];
		
		long start = System.currentTimeMillis();

		uploadFile(datasetUrl, fileOrDir);
		
		long stop = System.currentTimeMillis();

		System.out.println("elapsed time in milli = " + (stop - start));
		
		System.out.println("\n\nfinished for arguments:");
		
		for (String current : args) {
			System.out.println(current);
		}
	}
	
	public void uploadFile(String url, String fileOrDir) throws IOException {
		
		try {
			File file = new File(fileOrDir);
			if (file.isFile()) {
				uploadFile(url, file);
			} else {
				for (File current : file.listFiles()) {
					if (current.isFile()) {
						uploadFile(url, current);
					}
				}
			}
		} catch (IOException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}
	
	public String uploadFile(String url, File file) throws IOException {
		
		System.out.println("uploading file = " + file);
		
	    HttpPost post = new HttpPost(url);
	    post.setHeader("Content-Type", "application/rdf+xml;charset=UTF-8");
	    post.setHeader("Accept", "application/json");  
	    
	    FileEntity entity = new FileEntity(file);
	    post.setEntity(entity);
	    
	    CloseableHttpClient client = HttpClients.createDefault();
	    HttpResponse response = client.execute(post);
	    int httpStatus = response.getStatusLine().getStatusCode();
	    
	    System.out.println("HTTP Status = " + httpStatus);
	    
	    String responseMsg = null;
	    if (response != null && response.getEntity() != null) {
	    	responseMsg = EntityUtils.toString(response.getEntity(), "UTF-8");
	    	System.out.println(responseMsg);
	    }
	    
	    client.close();
	    return responseMsg;
	}
	
	public void testStart() throws IOException {
		
		String url = "http://localhost:8080/rdf4j-server/repositories/buildingsthehague/statements";
		String fileName = "C:/Users/Andreas/my/cityGML/buildingsthehague/01_buildings2.owl";
		
		String[] args = new String[] {url, fileName};
		start(args);
	}
}
