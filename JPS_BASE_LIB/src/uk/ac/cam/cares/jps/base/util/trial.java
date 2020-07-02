package uk.ac.cam.cares.jps.base.util;


import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;

import org.cts.IllegalCoordinateException;
import org.cts.crs.CRSException;
import org.cts.op.CoordinateOperationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class trial {

	public static void main(String[] args) {
		double[] coord= {10,0,0,0,0};
		CRSTransformer cRSTransformer= new CRSTransformer();
		FileUtil fileUtil=new FileUtil();
		String actual = fileUtil.readFileLocally(System.getProperty("user.dir")+"/test_sample_dir/test.txt");
		double[] expected = null;
		try {
			expected = cRSTransformer.transformInternal("EPSG:3414", "EPSG:2326", coord);
		} catch (IllegalCoordinateException e) {
			// TODO Auto-generated catch block
			System.out.println("illegal");
		} catch (CoordinateOperationException e) {
			// TODO Auto-generated catch block
			System.out.println("op");
		} catch (CRSException e) {
			// TODO Auto-generated catch block
			System.out.println("crs");
		}
		ArrayList<String> commands= new ArrayList();
		commands.add("open");
		commands.add("-a");
		commands.add("Pages");
		commands.add("test.txt");
		String[] command = commands.toArray(new String[0]);
		for(int i=0;i<command.length;i++)    //length is the property of the array  
			System.out.print(command[i]);  
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(System.getProperty("user.dir")+"/test_sample_dir/"));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			 
		//System.out.println(Arrays.toString(command));
		//System.out.println(command.length);

		

}
}