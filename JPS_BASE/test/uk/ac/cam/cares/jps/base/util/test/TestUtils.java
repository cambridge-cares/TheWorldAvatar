package uk.ac.cam.cares.jps.base.util.test;



import java.util.ArrayList;

import uk.ac.cam.cares.jps.base.util.CommandHelper;


public class TestUtils {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
 
		String targetFolder = "/home/zhouxiaochi/Documents/JPS/JParkSimulator-git/JPS_BASE/src/uk/ac/cam/cares/jps/base/util/T";
		
		ArrayList<String> list = new ArrayList<String>();
		list.add("python3");
		list.add("foo.py");
		list.add("The password of the server is ...");
		
		
		CommandHelper.executeSingleCommand(targetFolder, "touch bar2.txt");
		CommandHelper.executeCommands(targetFolder, list);
		
	}

}
