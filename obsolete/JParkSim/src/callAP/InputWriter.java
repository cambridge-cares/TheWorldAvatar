package callAP;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

public class InputWriter {
	public static String test1 = new String("C:/Users/Zhou/workspace/RunAspen/test.CSV"); 
	public static String APINCSV = new String("C:/Users/Zhou/workspace/RunAspen/APIN.CSV"); 
	static ArrayList<String[]> editStack = new ArrayList<String[]>();
	
	public static void initiate(){
		
		String[] layers = new String[2];
		String[] FIDs = new String[2];
		
		layers[0]="ChemProcess";
		FIDs[0]= "2";
		//manually define the editStack ZL-151202
		for (int i=0; i<layers.length; i++) {
			editStack.add(new String[] {layers[i], FIDs[i]});
			System.out.println("editStack="+editStack);
	    }
		FileWriter test = null;  //testing FileWriter ZL-151202
		try {
			test = new FileWriter(test1);
			test.append("layers=" + layers[0]);
			test.append(", FIDs=" + FIDs[0]);
			test.flush();
			test.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public static void main(String args[]){
		ArrayList<String[]> skeleton = new ArrayList<String[]>(); 
		
		initiate(); //initiate editStack ZL-151202
		skeleton.add(new String[] {"OIL,MEOH","FOIL,FMEOH"});  //manually add elements to skeleton ZL-151202
		writeAPCSV(skeleton); //write the APIN.CSV i.e. the input data for Aspen Plus ZL-151202
//		runPyScript(editStack); //call python script to run Aspen Plus model "BiodiesePlant" ZL-151202
		
	}
	
 	public static void writeAPCSV(ArrayList<String[]> skeleton){  
	        FileWriter flieWriter = null;
	        
	        try{
	        flieWriter = new FileWriter(APINCSV);
	        for(int i=0; i<skeleton.size(); i++){
	         flieWriter.append(skeleton.get(i)[0]); //write headers-Aspen Plus input names
	         flieWriter.append("\n");  //New line
	         String[] ArcGISfields = skeleton.get(i)[1].split(",");
//	         Map<String,Object> attributes = attributeslist.get(i); //pulls all date fields available from ArcGIS
	         
		 for(int j=0; j<ArcGISfields.length; j++) {
		    if(ArcGISfields[j].equals("FOIL")) {
			String ArcGISOILF ="30.04";
			flieWriter.append(ArcGISOILF);
			flieWriter.append(",");
			}
			else if(ArcGISfields[j].equals("FMEOH")){
			String ArcGISMEOHF ="90.899";
			flieWriter.append(ArcGISMEOHF);
			}			  	  	         
	        } 	         
 
	        }
	       } catch (Exception e) {
				e.printStackTrace();
			} finally {
				try {
					flieWriter.flush();
					flieWriter.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
	}

}
