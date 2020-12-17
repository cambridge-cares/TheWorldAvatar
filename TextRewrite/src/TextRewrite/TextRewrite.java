package TextRewrite;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class TextRewrite {


	public static void main(String[] args) {
       /* System.out.print("Phase: ");
        Scanner sp = new Scanner(System.in);
        String p;
        p = sp.nextLine();

        System.out.print("Database: ");
        Scanner sd = new Scanner(System.in);
        String d;
        d = sd.nextLine();

        System.out.print("IP address: ");
        Scanner sip = new Scanner(System.in);
        int ip = sip.nextInt();*/
		
		String[] gen1= {
	       		"Fan-10-1_TripAlarm",
	       		 "Fan-10-1_RunStat",
	       		"Fan-10-1_Mode",
	       		"Fan-10-1_VSDF",
	       		"Fan-10-1_VSDC",
	       		"D-10-1_1_BDF",
	       		"D-10-1_1_BDC",
	       		"10-1_D69F",
	       		"10-1_D69P",
	       		"10-1_D69T"
	       		};
		
		String[] gen2= {
				"E-303load",
	       		 "E-305load",
	       		"P-301load",
	       		"P-302load",
	       		"R-301load",
	       		"R-302load"
	       		,"T-303condenserload",
	       		"T-303reboilerload",
	       		"V-301load",
	       		"V-302load",
	       		"V-303load",
	       		"V-304load"
	       		 };
		
		
for(int i=1;i<=3;i++)
     {
	/**set for FCU*/
	int j=5*i+22; //same as st 27
	int k= 3*i+17; //same as p 20
	int l=5*i+19;//same as st 24
	int m=3*i+19;//same as p 22
	
	int st25=5*i+20;
	int st26=5*i+21;
	int p21=3*i+18;
	
	//String x=String.format("%03d", i); //function to make 1 become 001 and 54 become 054
         try
             {
             //File file = new File("C://Users//kevin//Dropbox (Cambridge CARES)//IRP3 CAPRICORN shared folder//LZHOU//backup//OntologyDevelopForEIP//ChemicalPlant//BiodieselPlantsOWLFilesYiRen//BiodieselPlant3OWL-withPipe - Li//individual owl file/BiodieselPlant-3Node.owl");
        	 File file = new File("D://airsep individual//AirSeparationPlantNode.owl");
             BufferedReader reader = new BufferedReader(new FileReader(file));
             String line = "", oldtext = "";
             while((line = reader.readLine()) != null)
                 {
                 oldtext += line + "\r\n";
             }
             reader.close();
             
             /*String[] room= {
            		 "VA-C7-1_2",
            		 "VA-C7-1_1",
            		 "F-C7-1",
            		 "T-joint-001",
            		 "HE-C7-1_1",
            		 "HE-C7-1_3",
            		 "HE-C7-1_2",
            		 "Fan-C7-1"};
             
             String[] roomname= {
            		 "MAU-VA-C7-1_2",
            		 "MAU-VA-C7-1_1",
            		 "MAU-F-C7-1",
            		 "MAU-Tjoint-C7-1",
            		 "MAU-HE-C7-1_1",
            		 "MAU-HE-C7-1_3",
            		 "MAU-HE-C7-1_2",
            		 "MAU-Fan-C7-1"};*/
             
 

        
             String replacedtext  = oldtext.replaceAll("AirSeparation","Lanxess");
             
             replacedtext = replacedtext.replaceAll("Air_Liquide","Lanxess"); //in to out
             replacedtext = replacedtext.replaceAll("T-501001","V-401001");
             replacedtext = replacedtext.replaceAll("501","401");
             replacedtext = replacedtext.replaceAll("X-501001","P-401001");
             
             
             /* replacedtext = replacedtext.replaceAll("T-303","T-601003");
             replacedtext = replacedtext.replaceAll("V-303","V-601003");
             replacedtext = replacedtext.replaceAll("C-301","MIX-601008");
             replacedtext = replacedtext.replaceAll("S-301","MIX-601009");
             replacedtext = replacedtext.replaceAll("V-301","V-601001");
             replacedtext = replacedtext.replaceAll("M-301","MIX-601001");
             replacedtext = replacedtext.replaceAll("P-301","P-601001");
             replacedtext = replacedtext.replaceAll("R-302","R-601002");
             replacedtext = replacedtext.replaceAll("E-302","E-601002");
             replacedtext = replacedtext.replaceAll("VA-301","VA-601001");
             replacedtext = replacedtext.replaceAll("E-304","E-601004");
             replacedtext = replacedtext.replaceAll("E-306","E-601006");
             replacedtext = replacedtext.replaceAll("M-303","MIX-601003");*/
             
             
             
             /**set for index in FCU*/
             /*replacedtext = replacedtext.replaceAll("D27","D"+ j);
             replacedtext = replacedtext.replaceAll("P20","P"+k);
             replacedtext = replacedtext.replaceAll("D24","D"+l);
             replacedtext = replacedtext.replaceAll("P22","P"+m);
             //replacedtext = replacedtext.replaceAll("C7-1","C7-"+i);
             replacedtext = replacedtext.replaceAll("024","0"+l);
             replacedtext = replacedtext.replaceAll("025","0"+st25);
             replacedtext = replacedtext.replaceAll("026","0"+st26);
             replacedtext = replacedtext.replaceAll("027","0"+j);
             replacedtext = replacedtext.replaceAll("CHW-020","CHW-0"+k);
             replacedtext = replacedtext.replaceAll("CHW-021","CHW-0"+p21);
             replacedtext = replacedtext.replaceAll("CHW-022","CHW-0"+m);
             replacedtext = replacedtext.replaceAll("Pipe-020","Pipe-0"+k);
             replacedtext = replacedtext.replaceAll("Pipe-021","Pipe-0"+p21);
             replacedtext = replacedtext.replaceAll("Pipe-022","Pipe-0"+m);*/
             
            /* replacedtext = replacedtext.replaceAll("P1","P9");
             replacedtext = replacedtext.replaceAll("P8","P16");
             replacedtext = replacedtext.replaceAll("Duct-001","Duct-007");
             replacedtext = replacedtext.replaceAll("Duct-002","Duct-008");
             replacedtext = replacedtext.replaceAll("Duct-003","Duct-009");
             replacedtext = replacedtext.replaceAll("Duct-004","Duct-010");
             replacedtext = replacedtext.replaceAll("Duct-005","Duct-011");
             replacedtext = replacedtext.replaceAll("Duct-006","Duct-012");*/
             
             /*replacedtext = replacedtext.replaceAll("Pipe-001","Pipe-009");
             replacedtext = replacedtext.replaceAll("Pipe-002","Pipe-010");
             replacedtext = replacedtext.replaceAll("Pipe-003","Pipe-011");
             replacedtext = replacedtext.replaceAll("Pipe-004","Pipe-012");
             replacedtext = replacedtext.replaceAll("Pipe-005","Pipe-013");
             replacedtext = replacedtext.replaceAll("Pipe-006","Pipe-014");
             replacedtext = replacedtext.replaceAll("Pipe-007","Pipe-015");
             replacedtext = replacedtext.replaceAll("Pipe-008","Pipe-016");*/
                          
             /*replacedtext = replacedtext.replaceAll("St-001","St-007");
             replacedtext = replacedtext.replaceAll("St-002","St-008");
             replacedtext = replacedtext.replaceAll("St-003","St-009");*/
             //replacedtext = replacedtext.replaceAll("St-004","St-010");
             //replacedtext = replacedtext.replaceAll("St-005","St-011");
             //replacedtext = replacedtext.replaceAll("St-006","St-012");
             
             /*replacedtext = replacedtext.replaceAll("D3","D9");
             replacedtext = replacedtext.replaceAll("D4","D10");
             replacedtext = replacedtext.replaceAll("D6","D12");*/
             
             
             /**set for reactor editing change owl file*/
             /* String replacedtext  = oldtext.replaceAll("R-301", "R-401");
             replacedtext = replacedtext.replaceAll("Reaction_Bio1","Reaction_SBRProduction1");
             replacedtext = replacedtext.replaceAll("ReactionNetwork_Bio", "ReactionNetwork_SBRProduction");
             replacedtext = replacedtext.replaceAll("tripalmitin", "Cyclohexene");//reactant1
              replacedtext = replacedtext.replaceAll("methanol", "Styrene");//reactant2
              //Diisooctyl-phthalate as reactant 3
              //1,3-Butadiene as reactant 4
             replacedtext = replacedtext.replaceAll("methylPalmitate", "Styrene-butadiene_rubber");//product1
             replacedtext = replacedtext.replaceAll("glycerol", "Styrene");//product2
             //Cyclohexene as product 3
             replacedtext = replacedtext.replaceAll("_3-3", "_4-3"); //input1
              replacedtext = replacedtext.replaceAll("_3-4", "_4-4");//input2
             replacedtext = replacedtext.replaceAll("_3-5", "_4-5");//output1
             replacedtext = replacedtext.replaceAll("_3-6", "_4-6");//output2
*/
  
             FileWriter writer = new FileWriter("D://lanxess individual//LanxessPlantNode.owl");
             writer.write(replacedtext);

             System.out.println("success!");

             writer.close();


         }
         catch (IOException ioe)
             {
             ioe.printStackTrace();
         }
     }
	}
}
