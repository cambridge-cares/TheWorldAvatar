package uk.ac.cam.cares.jps.agent.matlab;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

/**
 *Matlab Agent developed for setting-up and running electrical network
 *The files for Matlab execution should be placed in user.home//matlab folder
 * @author Gourab Karmakar (gourab.karmakar@cares.cam.ac.uk)
 */
@WebServlet("/JPSMatlabAgent")
public class JPSMatlabAgent extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public JPSMatlabAgent() {
        super();
        // TODO Auto-generated constructor stub
    }
	 
	/**
	 * Read the gPROMS output file from user.home/input directory
	 * Input filename: matlab.csv
	 * Get the values starting from row 2 and store it in array
	 * Loop the array till end and multiply ActivePower values with 0.5 in a new array key to get the reactive power 
	 * Create a new CSV file and write it into the output directory user.home/matlab
	 * Matlab input filename: matInput.dat
	 * Create a batch file to execute MATLAB from command line
	 */
    
	@Override
	   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			JSONObject jo = AgentCaller.readJsonParameter(request);
			String current = System.getProperty("user.home");
			String pathToInputFile = current + "\\matlab\\matlab.csv";
			String pathToSettingFile = current + "\\input\\Settings.input";			
			// Appending reactive power value on the Pump_power CSV file
			BufferedReader csvReader = null;
			try {
				csvReader = new BufferedReader(new FileReader(pathToInputFile));
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
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
				String matInputFile = current + "\\matlab\\matInput.dat";
				System.out.printf("\n Matlab input file is generated.\n Matlab input file is located at:" + matInputFile +"\n");
				FileWriter csvWriter = new FileWriter(matInputFile);
				for (List<String> rowData : output) {
				    csvWriter.append(String.join(",", rowData));
				    csvWriter.append("\n");
				}
				//close the writer 
				csvWriter.flush();
				csvWriter.close();
				
			} catch (IOException e) {
				// TODO Auto-generated catch block
			}
			
			//Create file path for batch file
			String batchFile = current + "\\matlab\\call_matlab.bat";
			System.out.printf("\n Matlab batch file generated. \n Batch file to execute matlab from cmd prompt is created at location:" + batchFile +"\n");
			
			//File path for Matlab script file
			String scriptFile = current + "\\matlab\\Run_Script.m";
			System.out.println("\n Executing the matlab script file. \n Matlab script file is located at:" + scriptFile + "\n");
			
			//Command string for Matlab
			String cmd = "matlab -nodisplay -nosplash -nodesktop -r \"run('"+ scriptFile + "');exit;\"";
	        
			//Creating batch file
		    try {
		    	File file = new File(batchFile);
	            FileWriter writer = new FileWriter(batchFile, true);
	            writer.write(cmd);
	            writer.close();
	            
	            //Execute batch file 
	            Runtime rs = Runtime.getRuntime();
	            try {
					rs.exec(batchFile).waitFor();
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
				}
	            System.out.printf("\nCompleted Execution\n");
				file.delete();
	            
	        } catch (IOException e) {
	            e.printStackTrace();
	        }
			//Delete the temporary file
		    File tempFile = new File(pathToInputFile);
			tempFile.delete();
			//Enter starting line here
			int startline=69;
			//Enter number of lines here.
			int numlines=4;
			JPSMatlabAgent now=new JPSMatlabAgent();
			now.delete(pathToSettingFile,startline,numlines);
			return jo;

		}
	
	void delete(String filename, int startline, int numlines)
	{
		try
		{
			BufferedReader br=new BufferedReader(new FileReader(filename));
 
			//String buffer to store contents of the file
			StringBuffer sb=new StringBuffer("");
 
			//Keep track of the line number
			int linenumber=1;
			String line;
 
			while((line=br.readLine())!=null)
			{
				//Store each valid line in the string buffer
				if(linenumber<startline||linenumber>=startline+numlines)
					sb.append(line+"\n");
				linenumber++;
			}
			if(startline+numlines>linenumber)
				System.out.println("End of file reached.");
			br.close();
 
			FileWriter fw=new FileWriter(new File(filename));
			//Write entire string buffer into the file
			fw.write(sb.toString());
			fw.close();
		}
		catch (Exception e)
		{
			System.out.println("Something went horribly wrong: "+e.getMessage());
		}
	}


}
