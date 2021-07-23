package uk.ac.cam.cares.jps.agent.ukdigitaltwin.tools;

import java.util.ArrayList;
import java.util.List;

/**
 * Printer is developed for making the print ArrayList procedure convenient
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 * 
 */

public class Printer {
	public static void printArrayList(ArrayList<List<String>> arraylist) {
		for(int i = 0; i < arraylist.size(); i++) { 
			   List<String> array = arraylist.get(i);
			   System.out.println(array); 
			   }
	}
}
