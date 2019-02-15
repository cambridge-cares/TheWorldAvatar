package kbduplicate;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;

public class Kbduplicate {

	
	public static void replaceString(String oldString, String newString, String oldFileName, String newFileName) {
		/** 
		 * Precondition: 
		 * oldString is a String
		 * newString is a String
		 * oldFileName is a String pointing to a valid .owl file
		 * newFileName is a String specifying the new filename of a new .owl file
		 * 
		 * Postcondition:
		 * newFileName.owl has been written to the package folder
		 * The newFileName.owl is identical to oldFileName.owl, except that all the occurrence of oldString replaced by newString
		 *  
		 *  */
		
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
	
			inputStr = inputStr.replace(oldString, newString); // Replacing old instance number with new instance number
	
			// check if the new input is right
	
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
		 
	}
		


}


