package kbduplicate;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Kbduplicate {

	
	public static void replaceString(String oldString, String newString, String oldFileName, String newFileName) throws FileNotFoundException, IOException {
		/** ONLY MAKING TEMPLATE BUT NOT THE REAL VALUE LOCATED YET
		 * Precondition: 
		 * oldString is a String
		 * newString is a String
		 * oldFileName is a String pointing to a valid .owl file
		 * newFileName is a String specifying the new filename of a new .owl file
		 * it's because in the current file all properties are referred with the specific main instance like EBus-001 or EGen-003, etc
		 * 
		 * Postcondition:
		 * newFileName.owl has been written to the package folder
		 * The newFileName.owl is identical to oldFileName.owl, except that all the occurrence of oldString replaced by newString
		 *  
		 *  */
		List<List<String>> recordsgen = new ArrayList<>();
		try (BufferedReader br = new BufferedReader(new FileReader("genmap.csv"))) {
		    String line;
		    while ((line = br.readLine()) != null) {
		        String[] values = line.split(",");
		        recordsgen.add(Arrays.asList(values));
		    }
		}
		
		List<List<String>> recordline = new ArrayList<>();
		try (BufferedReader br = new BufferedReader(new FileReader("linemap.csv"))) {
		    String line;
		    while ((line = br.readLine()) != null) {
		        String[] values = line.split(",");
		        recordline.add(Arrays.asList(values));
		    }
		}
		
		try {
			// input the file content to the StringBuffer "input"
			BufferedReader file = new BufferedReader(new FileReader(oldFileName));
			String line;
			StringBuffer inputBuffer = new StringBuffer();
	
			while ((line = file.readLine()) != null) {
				inputBuffer.append(line);
				inputBuffer.append('\n');
			}
			String inputStr = inputBuffer.toString();
	
			file.close();
	
			// System.out.println(inputStr); // check that it's inputting correctly
	
			//concept= gen,bus    and branch, bus,bus
	
			if(oldString.contains("Gen")) {
			String oldstring2="<topology:hasOutput rdf:resource=\"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-001.owl#EBus-001\"/>";
			//String ebusgenrelated=recordsgen.get(1).get(1);if want to map the value directly to the csv
			String ebusgenrelated="businput";
			String newstring2="<topology:hasOutput rdf:resource=\"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/"+ebusgenrelated+".owl#"+ebusgenrelated+"\"/>";
			inputStr = inputStr.replace(oldstring2, newstring2);
			}
			
			else if(oldString.contains("Line")) {
			
			String oldstring3="<topology:hasInput rdf:resource=\"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-045.owl#EBus-045\"/>";
			//String ebuslinerelated1=recordline.get(1).get(1);if want to map the value directly to the csv
			String ebuslinerelated1="businput";
			String newstring3="<topology:hasInput rdf:resource=\"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/"+ebuslinerelated1+".owl#"+ebuslinerelated1+"\"/>";
			String oldstring4="<topology:hasOutput rdf:resource=\"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-006.owl#EBus-006\"/>";
//			String ebuslinerelated2=recordline.get(1).get(2); //if want to map the value directly to the csv
			String ebuslinerelated2="busoutput";
			String newstring4="<topology:hasOutput rdf:resource=\"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/"+ebuslinerelated2+".owl#"+ebuslinerelated2+"\"/>";
			inputStr = inputStr.replace(oldstring3, newstring3);
			inputStr = inputStr.replace(oldstring4, newstring4);
			// check if the new input is right
			}
			
			inputStr = inputStr.replace(oldString, newString); // Replacing old instance number with new instance number
			
			// System.out.println("----------------------------------\n" + inputStr);
	
			// write the new String with the replaced line on a new file
			FileOutputStream fileOut = new FileOutputStream(newFileName);
			fileOut.write(inputStr.getBytes());
			fileOut.close();
	
		} catch (Exception e) {
			System.out.println("Problem reading file.");
		}
	}

	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		
		for(int i=2;i<=9;i++)
		{
		replaceString("EGen-001","EGen-00"+i,"EGen-001.owl","EGen-00"+i+".owl");
			
		}
		for(int i=1;i<=209;i++)
		{
			String b=String.format("%03d", i);
		replaceString("EBus-002","EBus-"+b,"EBus-002.owl","EBus-"+b+".owl");
		//it will duplicate EBus-001.owl with all the name reference to EBus-i ( with the i on 3 digit format) depends on the index how far)
			
		}
		for(int i=2;i<=219;i++)
		{
			String b=String.format("%03d", i);
		replaceString("ELine-001","ELine-"+b,"ELine-001.owl","ELine-"+b+".owl");
			
		}
		 
	}
		


}


