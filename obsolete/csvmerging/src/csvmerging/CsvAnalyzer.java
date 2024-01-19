package csvmerging;
import java.io.BufferedReader;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import java.util.ArrayList;

import java.util.HashMap;
import java.util.HashSet;

import java.util.Set;

public class CsvAnalyzer {
	static ArrayList <String> listofcountry = new ArrayList<>();
	static ArrayList <String> listofcapacity = new ArrayList<>();
	
	static ArrayList <PlantInstance> filtereddata1 = new ArrayList<>();
	static ArrayList <PlantInstance> filtereddata2 = new ArrayList<>();
	static ArrayList <PlantInstance> tempdatabase = new ArrayList<>();
	
	
	static HashMap<String, String> hmap = new HashMap<String, String>();
	
	public CsvAnalyzer() {
		super();
		//mapping from the more complete one to the less complete one (GEO to Chuan's)
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
		//-------------- Mapping for the specific fuel-------------------------- (unused currently)
		hmap.put("Natural Gas", "natural_gas");
		hmap.put("Coal", "coal");
		hmap.put("Bituminous", "bituminous");
		hmap.put("Lignite", "lignite");
		hmap.put("Anthracite", "anthracite");
		hmap.put("Subbituminous", "subbituminous");
		
		
	}
	
	
	public ArrayList<PlantInstance> readCSVtocontainer(int sourcefile, String csvfiledirectory,int dataindex1needed,int dataindex2needed,int dataindex3needed,int dataindex4needed,int nameindex, int dataindex5needed, int dataindex6needed,int adddata, int adddata2){ //give the new array consisting (country,capacity, technology, generalfuel,name, x||year, y||anngen, spec fuel)
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

				System.out.println("arraysize="+ country.length);
			
			a.setLineID(b);
			a.setName(country[nameindex]);
			a.setTechnology(country[dataindex3needed]);
			a.setGeneralFuel(country[dataindex4needed]);
			a.setSpecificFuel(country[adddata]);
			a.setCountry(country[dataindex1needed]);
			a.setCapacity(Double.valueOf(country[dataindex2needed]));
			
				if (sourcefile == 0) { // 0=flag for the GEO database

					a.setx(Double.valueOf(country[dataindex5needed]));
					a.sety(Double.valueOf(country[dataindex6needed]));
					if (country.length >= 14) {
						a.setOwner(country[adddata2]);
					} else {
						a.setOwner("unidentified");
					}
				} else if (sourcefile == 1) { // flag fro Chuan's database
					a.setYear(Integer.valueOf(country[dataindex5needed]));
					a.setanngen(Double.valueOf(country[dataindex6needed]));
					a.setOwner("unidentified");
				}


				combinelist.add(a);
				b++;
			}
			
		} catch (IOException e) {
			e.printStackTrace();

		}
		return combinelist;
	}


        
	 public static void main(String[] args) throws IOException {
		 String csvFile = "D://merging Plant Data Workspace/updated folder space 17119/powerplantsGEOdatabase.csv";
		 String csvFile2 = "D://merging Plant Data Workspace/updated folder space 17119/powerplant_database_chuan.csv";
		 String csvFilecombined = "D://merging Plant Data Workspace/updated folder space 17119/combined3.csv";
		 String csvFilefinal = "D://merging Plant Data Workspace/updated folder space 17119/combined4.csv";
		 CsvAnalyzer instance=new CsvAnalyzer();
		 

		 System.out.println("==========================based on GEO Data:==========================================");
		 filtereddata1=instance.readCSVtocontainer(0,csvFile, 0, 1,3,2,5,11,10,6,13);//0=country, 1= capacity,3=technology, 2=generalfuel,5 name, 10=y , 11=x, 6=specific fuel

		 System.out.println("==========================based on Chuan Data:==========================================");
		
	    filtereddata2=instance.readCSVtocontainer(1,csvFile2, 0, 1,3,2,0,4,6,7,0);//0=country, 1= capacity,3=technology, 2=generalfuel,5 = name, 4=year, 6=anngen,  7= spec fuel
			 
		
		createCSVTotal(filtereddata1,filtereddata2,csvFilecombined);
				 
		System.out.println("total data plant= "+tempdatabase.size());
		
		ArrayList<PlantInstance> clearplantdata=eliminateDuplication(tempdatabase);
		
		int numberofplant=clearplantdata.size();

		System.out.println("total data plant after elimination= "+numberofplant);
				
				
				createCSVfromcombineddata(clearplantdata,csvFilefinal);
				
				tempdatabase.clear();
				filtereddata1.clear();
				filtereddata2.clear();

	 }

		private static void createCSVTotal(ArrayList <PlantInstance> filtereddata1,ArrayList <PlantInstance> filtereddata2,String csvFilecombined) throws IOException {
			
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
			 writer.append(",");
			 writer.append("x");
			 writer.append(",");
			 writer.append("y");
			 writer.append(",");
			 writer.append("year");
			 writer.append(",");
			 writer.append("annual generation");
			 writer.append(",");
			 writer.append("specific fuel");
			 writer.append(",");
			 writer.append("owner");
			 writer.append("\n");
	
	 /**try to compare between the 2 sources based on same country, capacity, technology, and general fuel
	  * With the chuan's excel data as basis
	  * GEO database just to complete what is missing from chuan's excel data*/

		for (int v = 0; v < datasizechuan; v++) {
			for (int q = 0; q < datasizegeo; q++) {
				if (filtereddata2.get(v).getCountry().replace("_", " ").equals(filtereddata1.get(q).getCountry())) {
					if (filtereddata2.get(v).getGeneralFuel().equals(filtereddata1.get(q).getGeneralFuel())) {
						// &&
						// hmap.get(filtereddata1.get(q).getSpecificFuel()).equals(filtereddata2.get(v).getSpecificFuel())
						if (hmap.get(filtereddata1.get(q).getTechnology())
								.equals(filtereddata2.get(v).getTechnology())) {
							if (Math.round(filtereddata2.get(v).getCapacity()) == (Math
									.round(filtereddata1.get(q).getCapacity()))) {

								PlantInstance a = new PlantInstance(filtereddata1.get(q).getName());
								a.setName(filtereddata1.get(q).getName());
								a.setCountry(filtereddata2.get(v).getCountry());
								a.setLineID(filtereddata2.get(v).getLineID());
								a.setanngen(filtereddata2.get(v).getanngen());
								a.setCapacity(filtereddata1.get(q).getCapacity());
								a.setGeneralFuel(filtereddata2.get(v).getGeneralFuel());
								a.setSpecificFuel(filtereddata2.get(v).getSpecificFuel());
								a.setTechnology(filtereddata2.get(v).getTechnology());
								a.setx(filtereddata1.get(q).getx());
								a.sety(filtereddata1.get(q).gety());
								a.setYear(filtereddata2.get(v).getYear());
								a.setOwner(filtereddata1.get(q).getOwner());

								tempdatabase.add(a);
								System.out.println("line-" + filtereddata2.get(v).getLineID()
										+ " in Chuan's data = line-" + filtereddata1.get(q).getLineID()
										+ " in GEO data; name= " + filtereddata1.get(q).getName());
								writer.append(String.valueOf(filtereddata2.get(v).getLineID()));
								writer.append(",");
								writer.append(filtereddata1.get(q).getName());
								writer.append(",");
								writer.append(filtereddata2.get(v).getCountry());
								writer.append(",");
								writer.append(String.valueOf(filtereddata1.get(q).getCapacity()));
								writer.append(",");
								writer.append(filtereddata2.get(v).getTechnology());
								writer.append(",");
								writer.append(filtereddata2.get(v).getGeneralFuel());
								writer.append(",");
								writer.append(String.valueOf(filtereddata1.get(q).getx()));
								writer.append(",");
								writer.append(String.valueOf(filtereddata1.get(q).gety()));
								writer.append(",");
								writer.append(String.valueOf(filtereddata2.get(v).getYear()));
								writer.append(",");
								writer.append(String.valueOf(filtereddata2.get(v).getanngen()));
								writer.append(",");
								writer.append(String.valueOf(filtereddata2.get(v).getSpecificFuel()));
								writer.append(",");
								writer.append(filtereddata1.get(q).getOwner());
								writer.append("\n");

							}
						}
					}
				}

			}

		}
	 
	 
	 //second method to add the remaining plant data in chuan's file
	 int tempdatabasesize =tempdatabase.size();
	 System.out.println("size of matched plant= "+tempdatabasesize);
	 ArrayList<Integer> arrayid= new ArrayList<Integer>();
	 ArrayList<String> nameid= new ArrayList<String>();
	 for(int u=0;u<tempdatabasesize;u++) {
			 arrayid.add(tempdatabase.get(u).getLineID());
			 nameid.add(tempdatabase.get(u).getName());
	 }
	 
		for (int c = 1; c < datasizechuan; c++) {
			if (!arrayid.contains(filtereddata2.get(c).getLineID())) {

				PlantInstance a = new PlantInstance("plant-" + filtereddata2.get(c).getLineID());
				a.setName("plant-" + filtereddata2.get(c).getLineID());
				a.setCountry(filtereddata2.get(c).getCountry());
				a.setLineID(filtereddata2.get(c).getLineID());
				a.setanngen(filtereddata2.get(c).getanngen());
				a.setCapacity(filtereddata2.get(c).getCapacity());
				a.setGeneralFuel(filtereddata2.get(c).getGeneralFuel());
				a.setSpecificFuel(filtereddata2.get(c).getSpecificFuel());
				a.setTechnology(filtereddata2.get(c).getTechnology());
				a.setx(0.0);
				a.sety(0.0);
				a.setYear(filtereddata2.get(c).getYear());
				a.setOwner("unidentified");
				tempdatabase.add(a);

				writer.append(String.valueOf(filtereddata2.get(c).getLineID()));
				writer.append(",");
				writer.append("no name");
				writer.append(",");
				writer.append(filtereddata2.get(c).getCountry());
				writer.append(",");
				writer.append(String.valueOf(filtereddata2.get(c).getCapacity()));
				writer.append(",");
				writer.append(filtereddata2.get(c).getTechnology());
				writer.append(",");
				writer.append(filtereddata2.get(c).getGeneralFuel());
				writer.append(",");
				writer.append(String.valueOf("no x"));
				writer.append(",");
				writer.append(String.valueOf("no y"));
				writer.append(",");
				writer.append(String.valueOf(filtereddata2.get(c).getYear()));
				writer.append(",");
				writer.append(String.valueOf(filtereddata2.get(c).getanngen()));
				writer.append(",");
				writer.append(String.valueOf(filtereddata2.get(c).getSpecificFuel()));
				writer.append(",");
				writer.append("unidentified");
				writer.append("\n");
			}

		}
	
		for (int v = 0; v < datasizegeo; v++) {

			if (!nameid.contains(filtereddata1.get(v).getName())) {

				PlantInstance a = new PlantInstance(filtereddata1.get(v).getName());
				a.setName(filtereddata1.get(v).getName());
				a.setCountry(filtereddata1.get(v).getCountry());
				a.setLineID(v + 7000);
				a.setanngen(0.0);
				a.setCapacity(filtereddata1.get(v).getCapacity());
				a.setGeneralFuel(filtereddata1.get(v).getGeneralFuel());
				a.setTechnology(hmap.get(filtereddata1.get(v).getTechnology()));
				a.setx(filtereddata1.get(v).getx());
				a.sety(filtereddata1.get(v).gety());
				a.setYear(0);
				a.setOwner(filtereddata1.get(v).getOwner());
				a.setSpecificFuel(filtereddata1.get(v).getSpecificFuel());
				tempdatabase.add(a);

				writer.append(String.valueOf(v + 7000));
				writer.append(",");
				writer.append(filtereddata1.get(v).getName());
				writer.append(",");
				writer.append(filtereddata1.get(v).getCountry());
				writer.append(",");
				writer.append(String.valueOf(filtereddata1.get(v).getCapacity()));
				writer.append(",");
				writer.append(hmap.get(filtereddata1.get(v).getTechnology()));
				writer.append(",");
				writer.append(filtereddata1.get(v).getGeneralFuel());
				writer.append(",");
				writer.append(String.valueOf(filtereddata1.get(v).getx()));
				writer.append(",");
				writer.append(String.valueOf(filtereddata1.get(v).gety()));
				writer.append(",");
				writer.append("no year");
				writer.append(",");
				writer.append("no anngen");
				writer.append(",");
				writer.append(String.valueOf(filtereddata1.get(v).getSpecificFuel()));
				writer.append(",");
				writer.append(filtereddata1.get(v).getOwner());
				writer.append("\n");
			}

		}
	 
	 
	 
	 
	 
	 writer.flush();
	 writer.close();
		}


	public static ArrayList<PlantInstance> eliminateDuplication(ArrayList<PlantInstance> tempdatabase) {
		ArrayList<PlantInstance> eliminationresult = new ArrayList<PlantInstance>();
		ArrayList<String> nameid = new ArrayList<String>();

		for (PlantInstance element : tempdatabase) {
			if (!eliminationresult.contains(element) && !nameid.contains(element.getName())) {
				eliminationresult.add(element);
				nameid.add(element.getName());
			}
		}

		return eliminationresult;
	}
		
	private static void createCSVfromcombineddata(ArrayList<PlantInstance> filtereddata1, String csvFilecombined)
			throws IOException {

		FileWriter writer = new FileWriter(csvFilecombined);

		writer.append("number");
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
		writer.append(",");
		writer.append("x");
		writer.append(",");
		writer.append("y");
		writer.append(",");
		writer.append("year");
		writer.append(",");
		writer.append("annual generation");
		writer.append(",");
		writer.append("specific fuel");
		writer.append(",");
		writer.append("owner");
		writer.append("\n");

		for (int h = 0; h < filtereddata1.size(); h++) {

			writer.append(String.valueOf(h + 1));
			writer.append(",");
			writer.append(filtereddata1.get(h).getName());
			writer.append(",");
			writer.append(filtereddata1.get(h).getCountry());
			writer.append(",");
			writer.append(String.valueOf(filtereddata1.get(h).getCapacity()));
			writer.append(",");
			writer.append(filtereddata1.get(h).getTechnology());
			writer.append(",");
			writer.append(filtereddata1.get(h).getGeneralFuel());
			writer.append(",");
			writer.append(String.valueOf(filtereddata1.get(h).getx()));
			writer.append(",");
			writer.append(String.valueOf(filtereddata1.get(h).gety()));
			writer.append(",");
			writer.append(String.valueOf(filtereddata1.get(h).getYear()));
			writer.append(",");
			writer.append(String.valueOf(filtereddata1.get(h).getanngen()));
			writer.append(",");
			writer.append(String.valueOf(filtereddata1.get(h).getSpecificFuel()));
			writer.append(",");
			writer.append(filtereddata1.get(h).getOwner());
			writer.append("\n");
		}

		writer.flush();
		writer.close();

	}
		
	    }

