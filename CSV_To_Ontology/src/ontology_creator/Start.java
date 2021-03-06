package ontology_creator;

import java.util.Scanner;

public class Start {
	
	/**for the coordinate, use the test.java for replacing the default coordinate to the one written in coordinate.csv
	 * 
	 * don't use the name of the unit ops' first abbreviation (e.g: R(reactor), T(tower),P(pump), etc....) as the file name of the required files 
	 * no dash(-) in the 2nd character of the filename of the required files
	 * */
	
	public static void main(String[] args) {
//		Scanner sc = new Scanner(System.in);
//		
//		System.out.print("Input plant name: ");
//		String plantName = sc.nextLine();
//		System.out.print("[For Unit Operations] Input the CSV file generated by the Excel Macro: ");
//		String aswFile = sc.nextLine();
//		System.out.print("[For Heat Exchangers] Input the CSV file specifying the side (shell/tube) the utility streams are in: ");
//		String heatXfile = sc.nextLine();
//		System.out.print("[For Pipes and Streams] Input the TXT file (tab delimited) specifying the necessary details of the chemical species: ");
//		String chemSpecFile = sc.nextLine();
//		System.out.print("[For Feed, Product, and Waste Streams] Input the CSV file specifying the feed, product, and waste streams: ");
//		String specialStreamsFile = sc.nextLine();
//		System.out.print("[For Utility Streams] Input the starting number (6 digits) for the utility streams: ");
//		int utilStartNum = Integer.parseInt(sc.nextLine());
//		
//		sc.close();

//		String plantName = "MTBE-Plant";
//		String aswFile = "CSV Files/mtbe/MTBE-Plant_CSV_2.csv";
//		String heatXfile = "CSV Files/mtbe/All_v2_HE_CSV.csv";
//		String chemSpecFile = "CSV Files/mtbe/All_v2_chemSpec.txt";
//		String specialStreamsFile = "CSV Files/mtbe/All_v2_SpecialStream_CSV.csv";
//		int utilStartNum = 350201;
		
		String plantName = "Cumene-Plant";
		String aswFile = "CSV Files/cumene/Cumene_test.csv";
		String heatXfile = "CSV Files/cumene/All_v2_HE_CSV_Cumene.csv";
		String chemSpecFile = "CSV Files/cumene/All_v2_chemSpec_CSV_Cumene.txt";
		String specialStreamsFile = "CSV Files/cumene/All_v2_SpecialStream_CSV_Cumene.csv";
		int utilStartNum = 340200;
		
//		String plantName = "MTBE-Plant";
//		String aswFile = "CSV Files/mtbe/result.csv";
//		String heatXfile = "CSV Files/mtbe/All_v2_HE_CSV.csv";
//		String chemSpecFile = "CSV Files/mtbe/All_v2_chemSpec.txt";
//		String specialStreamsFile = "CSV Files/mtbe/All_v2_SpecialStream_CSV.csv";
//		int utilStartNum = 350201;
		
		Plant chemicalPlant = new Plant(plantName, aswFile, heatXfile, chemSpecFile, specialStreamsFile, utilStartNum);
		chemicalPlant.createOntology();
		
		System.out.println("Process completed. Created OWL files can be found at the same folder as this program.");
		
	}

}
