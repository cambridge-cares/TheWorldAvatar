package csvmerging;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class CsvMerging {

	
	public String getCountryCapacityGenFuel(ArrayList<String> comparisoncontainer,int index){
		
		return comparisoncontainer.get(index).split("A--and")[0];
	}
	
	public String getSpecificFuel(ArrayList<String> comparisoncontainer,int index){
		
		return comparisoncontainer.get(index).split("A--and")[1];
	}
	public String getTechnology(ArrayList<String> comparisoncontainer,int index){
		
		return comparisoncontainer.get(index).split("A--and")[2];
	}
	public String getPlantName(ArrayList<String> comparisoncontainer,int index){
		
		return comparisoncontainer.get(index).split("A--and")[3];
	}
	public String getLatitude (ArrayList<String> comparisoncontainer,int index){
		
		return comparisoncontainer.get(index).split("A--and")[4];
	}
	public String getLongitude (ArrayList<String> comparisoncontainer,int index){
		
		return comparisoncontainer.get(index).split("A--and")[5];
	}
	public String getOwner (ArrayList<String> comparisoncontainer,int index){
	
	return comparisoncontainer.get(index).split("A--and")[6];
	}

	public String getYear (ArrayList<String> comparisoncontainer,int index){
	
	return comparisoncontainer.get(index).split("A--and")[7];
	}

	public String getAge (ArrayList<String> comparisoncontainer,int index){
	
	return comparisoncontainer.get(index).split("A--and")[8];
	}

	public String getAnnGen (ArrayList<String> comparisoncontainer,int index){
	
	return comparisoncontainer.get(index).split("A--and")[9];
	}
	
	public void merging() throws IOException
	{
        //String csvFile = "D:/JParkSimulator-git-dev-database/csvmerging/fileadditional/powerplant_database_with_name_new (1).csv";
		// need to sorted based on keyidentifier
		String csvFile = "D:/merging workspace/combination3trial.csv";
		String csvFileforspecificfuel = "D:/merging workspace/sourcemapping.csv";
		String csvFilefortechmapping = "D:/merging workspace/technologymapping.csv";
        String line = "";
        String line2 = "";
        String line3 = "";
        String cvsSplitBy = ",";
        
        ArrayList<String> correctbox = new ArrayList<String>();
        
    	ArrayList<String> comparisoncontainer = new ArrayList<String>();
    	ArrayList<String> listoffuel = new ArrayList<String>();
    	ArrayList<String> listoftech = new ArrayList<String>();
       
    	BufferedReader br2 = new BufferedReader(new FileReader(csvFileforspecificfuel));
    	
        while ((line2 = br2.readLine()) != null) {
        	
       	 String[] dataofspecificfuel = line2.split(cvsSplitBy);
       	          	 
       	 listoffuel.add(dataofspecificfuel[0]);
       	 listoffuel.add(dataofspecificfuel[1]);
      	 
           	}
        
    	BufferedReader br3 = new BufferedReader(new FileReader(csvFilefortechmapping));
    	
        while ((line3 = br3.readLine()) != null) {
        	
       	 String[] dataoftech = line3.split(cvsSplitBy);
       	          	 
       	 listoftech.add(dataoftech[0]);
       	 listoftech.add(dataoftech[1]);
      	 
           	}
        

        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

System.out.println("From the first");
            while ((line = br.readLine()) != null) {
            	
            	 String[] data = line.split(cvsSplitBy);
            	          	 
            	 comparisoncontainer.add(data[0]);
                	}
                		
           
            
        
            int totalincontainer= comparisoncontainer.size();
            System.out.println("size of the comparisoncontainer= "+totalincontainer);
            
            for (int a=0;a<totalincontainer;a++)
            {
            
            String kar1= getCountryCapacityGenFuel(comparisoncontainer,a);
           
            String kar2= getSpecificFuel(comparisoncontainer,a);
           if (listoffuel.contains(kar2))
            {
            	int index=listoffuel.indexOf(kar2);
            	if(index%2==0&&!listoffuel.get(index+1).equals("empty"))
            	{
            	kar2=listoffuel.get(index+1);
            	}
            }
            String kar3= getTechnology(comparisoncontainer,a);
/*            if (listoftech.contains(kar3))
            {
            	int index=listoftech.indexOf(kar3);
            	if(index%2==0&&!listoftech.get(index+1).equals("empty"))
            	{
            	kar3=listoftech.get(index+1);
            	}
            }*/
            String kar4= getPlantName(comparisoncontainer,a);
            String kar5= getLatitude(comparisoncontainer,a);
            String kar6= getLongitude(comparisoncontainer,a);
            String kar7= getOwner(comparisoncontainer,a);
            String kar8= getYear(comparisoncontainer,a);
            String kar9= getAge(comparisoncontainer,a);
            String kar10= getAnnGen(comparisoncontainer,a);
            
            	for(int b=a;b<totalincontainer;b++)
            	{
                    
                    String kar11= getCountryCapacityGenFuel(comparisoncontainer,b);
                    String kar12= getSpecificFuel(comparisoncontainer,b);
                    if (listoffuel.contains(kar12))
                    {
                    	int index=listoffuel.indexOf(kar12);
                    	if(index%2==0&&!listoffuel.get(index+1).equals("empty"))
                    	{
                    	kar12=listoffuel.get(index+1);
                    	}
                    }
                    String kar13= getTechnology(comparisoncontainer,b);
/*                    if (listoftech.contains(kar13))
                    {
                    	int index=listoftech.indexOf(kar13);
                    	if(index%2==0&&!listoftech.get(index+1).equals("empty"))
                    	{
                    	kar13=listoftech.get(index+1);
                    	}
                    }*/
                    String kar14= getPlantName(comparisoncontainer,b);
                    String kar15= getLatitude(comparisoncontainer,b);
                    String kar16= getLongitude(comparisoncontainer,b);
                    String kar17= getOwner(comparisoncontainer,b);
                    String kar18= getYear(comparisoncontainer,b);
                    String kar19= getAge(comparisoncontainer,b);
                    String kar20= getAnnGen(comparisoncontainer,b);
                    
                    if(kar1.equals(kar11)){
                    	
                    	if(kar2.equals(kar12)){
                    	
                    		if(kar3.equals(kar13)){
                    			
                    			if((kar4.equals("empty")&&!kar14.equals("empty"))||(kar14.equals("empty")&&!kar4.equals("empty"))){
                    				
                    				if(kar4.equals("empty"))
                    				{
                    					//System.out.println("line number= "+a);
                    					//System.out.println("first case executed");
                    					//System.out.println(kar4);
                        				//System.out.println(kar14);
                    				String char4=kar14;
                    				String char5=kar15;
                    				String char6=kar16;
                    				String char7=kar17;
                    				String char8=kar8;
                    				String char9=kar9;
                    				String char10=kar10;
                    				
                    				String newarray= kar1+"A--and"+kar2+"A--and"+kar3+"A--and"+char4+"A--and"+char5+"A--and"+char6+"A--and"+char7+"A--and"+char8+"A--and"+char9+"A--and"+char10;
                    				comparisoncontainer.set(a, newarray+"modified for the first case");
                    				break;
                    				
                    				//correctbox.add(comparisoncontainer.get(b));	
                    				
                    				}
                    				else if (kar14.equals("empty"))
                    				//	System.out.println("line number= "+a);
                    					//System.out.println("second case executed");
                    				//System.out.println(kar4);
                    				//System.out.println(kar14);
                    				{
                    				String char14=kar4;
                    				String char15=kar5;
                    				String char16=kar6;
                    				String char17=kar7;
                    				String char18=kar18;
                    				String char19=kar19;
                    				String char20=kar20;
                    				String newarray= kar11+"A--and"+kar12+"A--and"+kar13+"A--and"+char14+"A--and"+char15+"A--and"+char16+"A--and"+char17+"A--and"+char18+"A--and"+char19+"A--and"+char20;
                    				comparisoncontainer.set(b, newarray+"modified for the Second case");
                    				break;

                    				//System.out.println("value added= "+newarray);
                    				//System.out.println("index= "+b);
                    				//correctbox.add(comparisoncontainer.get(b));	
                    				
                    				
                    				}
                    			}
                    				

                    		}
                    	}
                    	
                    	
                    }                 	
                                     
            	}
            	
            	
            	            		
            		correctbox.add(comparisoncontainer.get(a));
            		if (a>0&&!correctbox.get(a).contains("for the Second case")&&correctbox.get(a-1).contains("for the first case"))
            		{
            			if(correctbox.get(a-1).split("A--and")[3].equals(correctbox.get(a).split("A--and")[3]))
            			{
            				
            			correctbox.set(a,"needtobedeleted");

                		}
            		}
            		if (correctbox.get(a).contains("for the Second case"))
            		{	
            			if(correctbox.get(a-1).split("A--and")[3].equals(correctbox.get(a).split("A--and")[3]))
            			{
            			correctbox.set(a-1,"needtobedeleted");
            			}
                  		
            		}

//            	PrintStream console = System.out;
//
//        		File file = new File("out.txt");
//        		FileOutputStream fos = new FileOutputStream(file);
//        		PrintStream ps = new PrintStream(fos);
//        		System.setOut(ps);
//            	
//            	System.out.println(correctbox.get(a));
            	
            }
 
           System.out.println("size of the correctbox1= "+correctbox.size());
             int size=correctbox.size();
             
/*            for(int j = 0; j < size; j++)
            {
                String obj = correctbox.get(j);

                if(obj.contains("needtobedeleted")){
                   //found, delete.
                    correctbox.remove(j);
                    size=size-1;
                    
                }

            }*/
            
            while(correctbox.remove("needtobedeleted")) {}
            

            System.out.println("size of the correctbox2= "+correctbox.size());
            
             
            for (int k=0;k<correctbox.size();k++){
            	System.out.println(correctbox.get(k));
            }	
       } 
        
        catch (IOException e) {
            e.printStackTrace();
        }

	}
	


	
	 public static void main(String[] args) throws IOException {

CsvMerging a= new CsvMerging();
     a.merging();
	    }

}
