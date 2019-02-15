package csvmerging;
import java.io.BufferedReader;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.channels.ReadableByteChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class CsvAnalyzer {
	static ArrayList <String> listofcountry = new ArrayList<>();
	static ArrayList <String> listofcapacity = new ArrayList<>();
	
	static ArrayList <PlantInstance> filtereddata1 = new ArrayList<>();
	static ArrayList <PlantInstance> filtereddata2 = new ArrayList<>();
	
	static HashMap<String, String> hmap = new HashMap<String, String>();
	public CsvAnalyzer() {
		super();
		hmap.put("Open Cycle Gas Turbine","OCGT");
		hmap.put("Gas Turbine","OCGT");
		hmap.put("Thermal and OCGT","OCGT");
		hmap.put("Power and Desalination Open Cycle Gas Turbine","OCGT");
		hmap.put("OCGT and CCGT","OCGT");
		hmap.put("Gas Engines","OCGT");
		hmap.put("Combustion Turbine","OCGT");
		hmap.put("Thermal and OCGT and CCGT","OCGT");
		hmap.put("Combined Cycle Gas Turbine","CCGT");
		hmap.put("Combined Cycle Gas Engine (CCGE)","CCGT");
		hmap.put("Power and Heat Combined Cycle Gas Turbine","CCGT");
		hmap.put("Mix of Steam and Combustion Turbines","CCGT");
		hmap.put("Thermal and CCGT","CCGT");
		hmap.put("Heat and Power Steam Turbine","CCGT");
		hmap.put("Power and Desalination Combined Cycle Gas Turbine","CCGT");
		hmap.put("Heat and Power GE","CCGT");
		hmap.put("","unidentified");
		hmap.put("Cogeneration Power and Desalination Steam Turbine","cogeneration");
		hmap.put("Cogeneration Power and Heat Steam Turbine","cogeneration");
		hmap.put("Power and Heat Open Cycle Gas Turbine","cogeneration");
		hmap.put("Cogeneration Power and Heat Supercritical Steam Turbine","cogeneration");
		hmap.put("Oil Engine","engine");
		hmap.put("Sub and Ultrasuper Critical Thermal","subcritical");
		hmap.put("Sub-critical Steam Turbine","subcritical");
		hmap.put("Sub-critical Thermal","subcritical");
		hmap.put("Sub and Super Critical Thermal","subcritical");
		hmap.put("Super and Ultra-Super Critical Thermal","supercritical");
		hmap.put("Super-critical Thermal","supercritical");
		hmap.put("Super-critical Steam Turbine","supercritical");
		hmap.put("Ultra-Super-Critical Thermal","ultrasupercritical");
		
		
	}
	
	public int countUniqueElement(ArrayList<String> targetlist, String keyword){
		
		int a=0;
		int sizecountry=targetlist.size();
		for(int n=0;n<sizecountry;n++)
		{
			if (targetlist.get(n).contains(keyword))
			{
				a++;
			}
		}
		
		return a;
	}
	
	public ArrayList<PlantInstance> readCSVtocontainer(String csvfiledirectory,int dataindex1needed,int dataindex2needed,int dataindex3needed,int dataindex4needed,int nameindex){ //give the new array consisting (country,capacity, technology, generalfuel,name)
		String line = "";
		String cvsSplitBy = ",";

		ArrayList<PlantInstance>combinelist=new ArrayList<PlantInstance> ();
		try (BufferedReader br = new BufferedReader(new FileReader(csvfiledirectory))) {
			int b=0;
			while ((line = br.readLine()) != null) {
				if(b==0){
					b++;
					continue;
				}
				
				PlantInstance a= new PlantInstance(String.valueOf(b));

				// use comma as separator
				String[] country = line.split(cvsSplitBy);

				//System.out.println("data [country name= " + country[dataindex1needed] + " , capacity=" + country[dataindex2needed] + "]");
			a.setLineID(b);
			a.setName(country[nameindex]);
			a.setTechnology(country[dataindex3needed]);
			a.setGeneralFuel(country[dataindex4needed]);
				//if(combinelist.size()==1||!combinelist.contains(country[dataindex1needed]))
				//{
				//combinelist.add(country[dataindex1needed]);
			//	System.out.println(country[dataindex1needed]);
				a.setCountry(country[dataindex1needed]);
				//}
				
				//combinelist.add(country[dataindex2needed]); //add the capacity first
			//	System.out.println(country[dataindex2needed]);
				a.setCapacity(Double.valueOf(country[dataindex2needed]));
				combinelist.add(a);
				b++;
			}
			
		} catch (IOException e) {
			e.printStackTrace();

		}
		return combinelist;
	}
	
	public ArrayList<String> doMainJob(String csvfiledirectory, int dataindex1needed,int dataindex2needed,ArrayList<String>containerusedtarget) { //return the array of capacity based on country
		
		ArrayList <String> capacityarray = new ArrayList<>();
		String line = "";
		String cvsSplitBy = ",";

		try (BufferedReader br = new BufferedReader(new FileReader(csvfiledirectory))) {

			while ((line = br.readLine()) != null) {

				// use comma as separator
				String[] country = line.split(cvsSplitBy);

				//System.out.println("data [country name= " + country[dataindex1needed] + " , capacity=" + country[dataindex2needed] + "]");
				listofcountry.add(country[dataindex1needed]);
				listofcapacity.add(country[dataindex2needed]);
			}

			addUniqueValuetoNewContainer(containerusedtarget);
			
			
			for(int a=0;a<listofcapacity.size();a++){
				if (containerusedtarget.contains(listofcountry.get(a))){
					capacityarray.add(listofcapacity.get(a));
//					System.out.println("country= "+listofcountry.get(a));
//					System.out.println("capacity= "+listofcapacity.get(a));
				}
				
			}
			
			
			
			
			
			
			
		//	System.out.println("=========================================================================");
		} catch (IOException e) {
			e.printStackTrace();

		}
		listofcountry.clear();
		listofcapacity.clear();

		return capacityarray;
	}

	/**
	 * @param containerused
	 */
	public void addUniqueValuetoNewContainer(ArrayList<String>containerusedtarget) {
		
		
		Set<String> uniqueCountry = new HashSet<String>(listofcountry);


		//System.out.println(uniqueCountry.toString());
//	System.out.println("=========================================================================");

		Object[] uniqueC = (Object[]) uniqueCountry.toArray();
		for (int a = 0; a < uniqueCountry.size(); a++) {

			//System.out.println((String) uniqueC[a] + ",amount= " + countUniqueElement(listofcountry,(String) uniqueC[a]));
			containerusedtarget.add((String) uniqueC[a]);
			containerusedtarget.add(String.valueOf(countUniqueElement(listofcountry,(String) uniqueC[a])));
			//containerused.add("an array consist of capacity of every country");
			
		}
	}
	
	public ArrayList<String> Filterstep1(ArrayList<String>containerused1,ArrayList<String>containerused2){ //what exist in GEO but not exist in chuan
		int size1=containerused1.size();
		int size2=containerused2.size();
		ArrayList<String>combined=new ArrayList<String> ();
		for(int n=0;n<size1;n+=2)
		{
			if(containerused2.contains(containerused1.get(n).replace(" ", "_"))){
				String amountin1=containerused1.get(n+1);
				String amountin2=containerused2.get(containerused2.indexOf(containerused1.get(n).replace(" ", "_"))+1);
				System.out.println("country duplicated= "+containerused1.get(n).replace(" ", "_"));
				System.out.println("amount of plant in container 1 (original GEO)= "+amountin1);
				System.out.println("amount of plant in container 2 (chuan)= "+amountin2);
				if(amountin1.equals(amountin2)){
					combined.add(containerused1.get(n).replace(" ", "_"));
					combined.add(containerused1.get(n+1));
					
				}
				System.out.println("=========================================================================");
			}
			else
			{
				System.out.println("country not exist in chuan's data= "+containerused1.get(n));
				System.out.println("=========================================================================");
			}
	}
		containerused1.clear();
		containerused2.clear();
		//combined.clear();
		
		return combined;
		
	}
	
	public void Filterstep2(ArrayList<String>containerused1,ArrayList<String>containerused2,ArrayList<String>combined){ //what exist in Chuan but not exist in GEO
		int size1=containerused1.size();
		int size2=containerused2.size();
		for(int n=0;n<size2;n+=2)
		{
			if(containerused1.contains(containerused2.get(n).replace("_", " "))){
				String amountin2=containerused2.get(n+1);
				String amountin1=containerused1.get(containerused1.indexOf(containerused2.get(n).replace("_", " "))+1);
				System.out.println("country duplicated= "+containerused2.get(n).replace("_", " "));
				System.out.println("amount of plant in container 1 (original GEO)= "+amountin1);
				System.out.println("amount of plant in container 2 (chuan)= "+amountin2);
				if(amountin1.equals(amountin2)){
					combined.add(containerused2.get(n).replace("_", " "));
				}
				System.out.println("=========================================================================");
			}
			else
			{
				System.out.println("country not exist in geo data= "+containerused2.get(n));
				System.out.println("=========================================================================");
			}
	}
		containerused1.clear();
		containerused2.clear();
		//combined.clear();
		
	}
	
	public void matchArray(ArrayList<String>containerused1,ArrayList<String>containerused2) throws IOException{
		//clear the header first
		containerused1.remove(0);
		containerused1.remove(0);

		containerused2.remove(0);
		containerused2.remove(0);
		
		int size1= containerused1.size();
		int size2= containerused2.size();
		
//		String csvFilecombined = "D://merging workspace/updated folder space 17119/combined.csv";
//		 FileWriter writer = new FileWriter(csvFilecombined);
//		 writer.append("country");
//		 writer.append(",");
//		 writer.append("numberofplant");
//		 writer.append(",");
//		 writer.append("capacity");
//		 writer.append("\n");
		for (int w=0;w<size1;w++)
		{
			if (!containerused1.get(w).contains("a")&!containerused1.get(w).contains("i")&!containerused1.get(w).contains("u")&!containerused1.get(w).contains("e")&!containerused1.get(w).contains("o")){
				System.out.println("answer from 1: capacity= "+containerused1.get(w)+" and index= "+w);
//				 writer.append(containerused1.get(w));
//				 writer.append("\n");
			}
			else
			{
				System.out.println("answer from 1: country= "+containerused1.get(w));
//				 writer.append(containerused1.get(w));
//				 writer.append(",");

			}
		}
		
		for (int w=0;w<size2;w++)
		{
			if (!containerused2.get(w).contains("a")&!containerused2.get(w).contains("i")&!containerused2.get(w).contains("u")&!containerused2.get(w).contains("e")&!containerused2.get(w).contains("o")){
				//System.out.println("answer from 2: capacity= "+containerused2.get(w)+" and index= "+w);
			}
			else
			{
				System.out.println("answer from 2: country= "+containerused2.get(w));
			}
		}
		

		
		
		
		
//		 writer.flush();
//		 writer.close();
		
	}
	

        
        
	 public static void main(String[] args) throws IOException {
		 String csvFile = "D://merging workspace/updated folder space 17119/powerplantsGEOdatabase.csv";
		 CsvAnalyzer instance=new CsvAnalyzer();
		 
		 //System.out.println("array= "+instance.readCSVtocontainer(csvFile,0,1));
		 //instance.doMainJob(csvFile,0,1,filtereddata1);//0=country, 1= capacity
		 System.out.println("==========================based on GEO Data:==========================================");
		 filtereddata1=instance.readCSVtocontainer(csvFile, 0, 1,3,2,5);//0=country, 1= capacity,3=technology, 4=fuel,5 name
		 System.out.println("plant example= "+filtereddata1.get(2));
		 System.out.println("country= "+filtereddata1.get(2).getCountry());
		 System.out.println("capacity= "+filtereddata1.get(2).getCapacity());
		 System.out.println("id= "+filtereddata1.get(2).getLineID());
		 
		 System.out.println("plant example= "+filtereddata1.get(3));
		 System.out.println("country= "+filtereddata1.get(3).getCountry());
		 System.out.println("capacity= "+filtereddata1.get(3).getCapacity());
		 System.out.println("id= "+filtereddata1.get(3).getLineID());
		 System.out.println("==========================based on Chuan Data:==========================================");
			String csvFile2 = "D://merging workspace/updated folder space 17119/powerplant_database_chuan.csv";
			 filtereddata2=instance.readCSVtocontainer(csvFile2, 0, 1,3,2,0);//0=country, 1= capacity,3=technology, 4=fuel,5 = name
			 
			 //instance.matchArray(filtereddata1, filtereddata2);
			 //instance.Filterstep1(filtereddata1,filtereddata2); //result only cuba after ignoring the spacing and underscore
			 //instance.Filterstep2(filtereddata1,filtereddata2,null); //result

				String csvFilecombined = "D://merging workspace/updated folder space 17119/combined.csv";
				 FileWriter writer = new FileWriter(csvFilecombined);
			 int datasizegeo=filtereddata1.size();
			 int datasizechuan=filtereddata2.size();
			 writer.append("id based on chuan data");
			 writer.append(",");
			 writer.append("name");
			 writer.append(",");
			 writer.append("country");
			 writer.append(",");
			 writer.append("capacity");
			 writer.append(",");
			 writer.append("technology");
			 writer.append(",");
			 writer.append("general fuel");
			 writer.append("\n");
			 for (int v=0;v<datasizechuan;v++){
				 for (int q=0;q<datasizegeo;q++){
				if(filtereddata2.get(v).getCountry().equals(filtereddata1.get(q).getCountry())&&filtereddata2.get(v).getCapacity()==(filtereddata1.get(q).getCapacity())&&hmap.get(filtereddata1.get(q).getTechnology()).equals(filtereddata2.get(v).getTechnology())&&filtereddata2.get(v).getGeneralFuel().equals(filtereddata1.get(q).getGeneralFuel())){
				System.out.println ("line-"+filtereddata2.get(v).getLineID()+" in Chuan's data = line-"+filtereddata1.get(q).getLineID()+" in GEO data; name= "+filtereddata1.get(q).getName());
				writer.append(String.valueOf(filtereddata2.get(v).getLineID()));
				 writer.append(",");
				 writer.append(filtereddata1.get(q).getName());
				 writer.append(",");
				 writer.append(filtereddata2.get(v).getCountry());
				 writer.append(",");
				 writer.append(String.valueOf(filtereddata2.get(v).getCapacity()));
				 writer.append(",");
				 writer.append(filtereddata2.get(v).getTechnology());
				 writer.append(",");
				 writer.append(filtereddata2.get(v).getGeneralFuel());
				 writer.append("\n");
				}
				
					 
				 }
			 }
			 writer.flush();
			 writer.close();
				 
			 
			 
			 
			 //createCSV(instance);
	 }

	/**
	 * @param instance
	 * @throws IOException
	 */
	public static void createCSV(CsvAnalyzer instance) throws IOException {
		ArrayList<String>combined=new ArrayList<String> ();
		// combined= instance.Filterstep1(filtereddata1,filtereddata2); //result only cuba after ignoring the spacing and underscore
		 int comsize=combined.size();
			String csvFilecombined = "D://merging workspace/updated folder space 17119/combined.csv";
			 FileWriter writer = new FileWriter(csvFilecombined);
			 writer.append("country");
			 writer.append(",");
			 writer.append("numberofplant");
			 writer.append(",");
			 writer.append("capacity");
			 writer.append("\n");
		 for (int f=0;f<comsize;f+=2)
		 {
			 writer.append(combined.get(f));
			 writer.append(",");
			 writer.append(combined.get(f+1));
			 writer.append(",");
			 writer.append("0");
			 writer.append("\n");
		 }
		 writer.flush();
		 writer.close();
	}

	    }

