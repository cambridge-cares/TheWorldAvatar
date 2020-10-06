package kbirichanger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class KbIRIchanger {

	public static void irichanger() throws IOException {
	
		String myDirectoryPath ="C:/Users/KADIT01/TOMCAT/webapps/ROOT/kb/sgp/singapore/singaporeelectricalnetwork";
		//String myDirectoryPath ="D:/KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/sgp/pvsingaporenetwork";
		//String myDirectoryPath ="D:/plant to be splitted";
		
		File dir = new File(myDirectoryPath);
		  File[] directoryListing = dir.listFiles();
		  
		  //String csvFile = "D:/JParkSimulator-git-dev-database/kbirichanger/csv/bmsplanmaprelationchange.csv";
		   String csvFile = "D:/JParkSimulator-git-dev-database/kbirichanger/csv/map.csv";
		  
	        String line = "";
	        String cvsSplitBy = ",";
	        
	      
	        
	        
		  if (directoryListing != null) {
		    for (File child : directoryListing) {
		    	
		    	Path path = Paths.get(child.getPath());
		    	//Path path2 = Paths.get("D:/JParkSimulator-git-dev-database/kbirichanger/outcome/"+child.getName());
		    	//Path path2 = Paths.get("D:/KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/powerplants/"+child.getName());
		    	Path path2 = Paths.get("D:/tmp/"+child.getName());
		    	//System.out.println("what is path taken= "+child.getPath());
		    	Charset charset = StandardCharsets.UTF_8;
		    	String content = new String(Files.readAllBytes(path), charset);
		    	 
		    	try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

			            while ((line = br.readLine()) != null) {

			                // use comma as separator
			                String[] iri = line.split(cvsSplitBy);
			                
			                //content = content.replaceAll("\\(","");
			                //content = content.replaceAll("\\)","");
			                //content = content.replaceAll("&apos;", "");
			                //System.out.println("old name= "+iri[0]);
			                //System.out.println("new name= "+iri[1]);
			                
			               //content = content.replaceAll(iri[0], iri[1]);
			                
			                content = content.replaceAll("http://www.jparksimulator.com/kb/sgp/singaporepowernetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork");
			                content = content.replaceAll("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork");
			                content = content.replaceAll("##", "#");
			                Files.write(path2, content.getBytes(charset));	
					    	
			            }

			        } catch (IOException e) {
			            e.printStackTrace();
			        }
		    	
		    }
		    System.out.println("done");
		  } 
		  
		  else {
		    System.out.println("the directory is not correct");
		  }	  
		  
//		Path path = Paths.get("test.txt");
//	Charset charset = StandardCharsets.UTF_8;
//
//	String content = new String(Files.readAllBytes(path), charset);
//	content = content.replaceAll("www.theworldavatar.com", ".....");
//	Files.write(path, content.getBytes(charset));
	
	}
	
	
	public static void main (String[] args) throws IOException 
			{
		irichanger();
		//String myDirectoryPath ="D:/plantfromserver";
		//String myDirectoryPath ="D:/KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/temporary/DESold";
		
		//File dir = new File(myDirectoryPath);
		  //File[] directoryListing = dir.listFiles();
		  //System.out.println(dir.getName());
			}
}

