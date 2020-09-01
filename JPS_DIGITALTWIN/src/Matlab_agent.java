import java.io.File;  // Import the File class
import java.io.IOException;  // Import the IOException class to handle errors
import java.io.FileWriter; 
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.PrintStream;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.ArrayList;

import com.mathworks.engine.EngineException;
import com.mathworks.engine.MatlabEngine;
import com.mathworks.engine.MatlabExecutionException;
import com.mathworks.engine.MatlabSyntaxException;
import com.mathworks.*;
import matlabcontrol.*;

/**
 * Loads the CSV output from GProms into the Matlab agent and executes the matlab input file for the electical systems
 * 
 * @author Gourab
 * 
 */

public class Matlab_agent {
	public static void main(String[] args) throws IllegalArgumentException, IllegalStateException, InterruptedException, MatlabExecutionException, MatlabSyntaxException, ExecutionException, MatlabConnectionException, MatlabInvocationException {
		
		//Read the CSV input file from /res/input/ directory
		
		//Input filename: input.csv
		
		//Get CSV values starting from row 2 and store it in array
		
		//Loop the array till end and multiply ActivePower values with 0.5 in a new array key
		
		//Create a new CSV file and write it into the output directory /res/output
		
		//Output filename: output.csv
		
		//Get the current relative path
		Path currentRelativePath = Paths.get("");
		String s = currentRelativePath.toAbsolutePath().toString();
		
		
		//Append current relative path with the input file path
		String pathToInputFile = s+"/res/input/input.csv";
		
		BufferedReader csvReader = null;
		try {
			csvReader = new BufferedReader(new FileReader(pathToInputFile));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		String row;
		try {
			
			ArrayList<ArrayList<String>> output = new ArrayList<ArrayList<String>>();
			
			row = csvReader.readLine();			
			
			while ((row = csvReader.readLine()) != null) {
			    String[] input = row.split(",");
			    
			    
			    double input2 = Double.parseDouble(input[1]) * 0.5; 
			    
			    ArrayList<String> inner = new ArrayList<String>();        

			    inner.add(input[0]);     
			    inner.add(input[1]);
			    inner.add(Double.toString(input2));

			    output.add(inner);		    
			    
			}
			
			//close the reader
			csvReader.close();
			
			//Write the ArrayList into CSV into the path specified
			String pathToOutputFile = s+"/res/output/output.csv";			
			
			FileWriter csvWriter = new FileWriter(pathToOutputFile);
			csvWriter.append("Time");
			csvWriter.append(",");
			csvWriter.append("ActivePower");
			csvWriter.append(",");
			csvWriter.append("ReactivePower");
			csvWriter.append("\n");

			for (List<String> rowData : output) {
			    csvWriter.append(String.join(",", rowData));
			    csvWriter.append("\n");
			}
			
			//close the writer 
			csvWriter.flush();
			csvWriter.close();		
			
			
			//Call Matlab
			
			//path: 
			
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		MatlabProxyFactoryOptions.Builder builder = new MatlabProxyFactoryOptions.Builder();
		// setup the factory
//		    setCopyPasteCallback() connects to an existing MATLAB by copy-pasting a few lines into the command window
//		    setUsePreviouslyControlledSession() starts a new MATLAB or connects to a previously started MATLAB without any user intervention

		MatlabProxyFactory factory = new MatlabProxyFactory(builder.build());
		// get the proxy
		MatlabProxy proxy = factory.getProxy();
		// do stuff over the proxy
                  
        proxy.eval("run('C:/JParkSimulator-git-project/JPS_DIGITALTWIN/res/matlab/Run_Script.m')");
		
	}
}
