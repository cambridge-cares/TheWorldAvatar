package plantOwl;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class CountryChecker {

	public void check() throws IOException {
		
		List<String> countries = new ArrayList<String>();
		
		
		 String csvFile = "D:/JParkSimulator-git-dev-database/PlantOwl/datainput.csv";
	        String line = "";
	        String cvsSplitBy = ",";

	        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

	            while ((line = br.readLine()) != null) {

	                // use comma as separator
	                
	            	String[] data = line.split(cvsSplitBy);
		
		
		String nextCountryName = "Algeria";
		if (!data[0].contains(nextCountryName)) {
			countries.add(data[0]);
			String IRI = "http://dbpedia.org/resource/" + data[0];
			System.out.println(IRI);
		
		}
		
		
		
		
		
		
		
		
		//for (String current : countries) {
			
			
		
			
			
			
		//}
	            }
	        }
		
	}
	
	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		CountryChecker converter = new CountryChecker();
		converter.check();

	}
}
