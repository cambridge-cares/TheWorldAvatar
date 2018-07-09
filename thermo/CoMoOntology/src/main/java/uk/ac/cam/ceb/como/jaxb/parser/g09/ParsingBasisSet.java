package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ParsingBasisSet {	

public static String getBasisSetString(File f) throws IOException {
		
		String basisSetString= "";	
		
		/**
		 * 
		 * @author nk510
		 *         <p>
		 *         try block works under JavaSE 1.7
		 *         </p>
		 * 
		 */

		BufferedReader br = new BufferedReader(new FileReader(f));

		for (String line; (line = br.readLine()) != null;) {

			if (line.contains("#p ")) {
				
				/**
				 * 
				 * @author nk510
				 *         <p>
				 *         Returns substring that starts with "/" string and ends with first appearing " " character. 
				 *         </p>
				 * 
				 */
				
				/**
				 * Split string that starts with '#p ' on two substrings before and after '/'.
				 */
				String splitOne[]=line.split("/");				
				line=  splitOne[1];				
				
				/**
				 * Split second substring into two strings before white space and after white space.
				 */
				
				String splitTwo[] = line.split(" ");				

				basisSetString = basisSetString + splitTwo[0];
				
				System.out.println(basisSetString);
				
				break;
			}
		}

		br.close();
	
	return basisSetString;
	
     }
 
}