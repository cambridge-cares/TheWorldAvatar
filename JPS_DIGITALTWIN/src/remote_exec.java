import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class remote_exec {
	
	public static void main(String[] args) {
		
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
			String pathToOutputFile = s+"/res/matlab/output.dat";			
			
			FileWriter csvWriter = new FileWriter(pathToOutputFile);
			
			for (List<String> rowData : output) {
			    csvWriter.append(String.join(",", rowData));
			    csvWriter.append("\n");
			}
			
			//close the writer 
			csvWriter.flush();
			csvWriter.close();		
			
	
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// Execute matlab 
		
	    Runtime rs = Runtime.getRuntime();
	    try {
			try {
				System.out.printf("\n----------------------Starting the execution of the electrical system-----------------------\n ");
				rs.exec("//Users/gourab/JParkSimulator-git/JPS_DIGITALTWIN/res/matlab/call_matlab.sh").waitFor();
				
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			System.out.printf("executed ");
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	  
	}

}
