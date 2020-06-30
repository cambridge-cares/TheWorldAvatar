package uk.ac.cam.cares.jps.base.util;


import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class trial {

	public static void main(String[] args) {
		CommandHelper commandHelper= new CommandHelper();
		String expected = commandHelper.executeSingleCommand(System.getProperty("user.dir")+"/test_sample_dir", "open -e test.txt");
		System.out.println(expected);
		

}
}