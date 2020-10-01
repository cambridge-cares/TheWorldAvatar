package uk.ac.cam.cares.jps.agent.gPROMS;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import au.com.bytecode.opencsv.CSVWriter;

public class output_demo {

	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		Path archive = Paths.get("C:\\Users\\caresadmin\\gPROMSAgent_5154434330700\\172.25.186.150_1798655975861500\\fornow.GPLOT");
			if (!Files.exists(archive)) throw new IOException("Cannot find expected archive at: " + archive);
			
		
		ArrayList<String> result = new ArrayList<>(); 
		byte[] bytes = Files.readAllBytes(archive);
		System.out.println(bytes);
			//byte[] result=new byte[10];
		try (BufferedReader br = new BufferedReader(new FileReader(archive.toString()))) {
	    while (br.ready()) {
	        result.add(br.readLine());
	        int i = result.size();
	        System.out.println(i);
	    }
	    br.close();
		}
	    //System.out.println(result.size());
	    //System.out.println(result.get(16038));
	    //System.out.println(result.get(0));
	    // Integer r is the line number of the required variable minus 1
	    int r=16038;
	    int vars=Integer.parseInt(result.get(0)); 
	    int n =result.size()/(vars+1);
	    //The required variable and the time index is stored into a float array
	    float table[][]=new float[n][2];
	    System.out.println(n);
	    for (int i=1; i<n;i++) {
	    	table[i][0]=Float.parseFloat(result.get((vars+1)*i));
	    	table[i][1]=Float.parseFloat(result.get(r+(vars+1)*i)); 
	    	System.out.println(table[i][0]);
			}
	    exportDataToExcel("C:\\Users\\caresadmin\\gPROMSAgent_5154434330700\\172.25.186.150_1798655975861500\\matlab.csv", table);
	    System.out.println("reading file...");
		}
		
	public static void exportDataToExcel(String fileName, float[][] data) throws FileNotFoundException, IOException{
    File file = new File(fileName);
    if (!file.isFile())
        file.createNewFile();

    CSVWriter csvWriter = new CSVWriter(new FileWriter(file));

    int rowCount = data.length;

    for (int i = 0; i < rowCount; i++)
    {
        int columnCount = data[i].length;
        String[] values = new String[columnCount];
        for (int j = 0; j < columnCount; j++)
        {
            values[j] = data[i][j] + "";
        }
        csvWriter.writeNext(values);
    }

    csvWriter.flush();
    csvWriter.close();
}
	}

