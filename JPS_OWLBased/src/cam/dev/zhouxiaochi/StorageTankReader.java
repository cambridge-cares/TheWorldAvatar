package cam.dev.zhouxiaochi;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cam.dev.zhouxiaochi.OWLReader.OWLfileNode;


	public class StorageTankReader {
		

		private static StorageTankReader instance = null;
		
	public  ArrayList<String> attributes = new ArrayList<String>();
	public ArrayList<String> deviceNames = new ArrayList<String>();

    public ArrayList<String>  values = new ArrayList<String>();

 
    public StorageTank[]   tankDrawInfo;
    
    public class StorageTank{
    	private double xCoor;
    	private double yCoor;
    	private double radius;
		public double getxCoor() {
			return xCoor;
		}
		public void setxCoor(double xCoor) {
			this.xCoor = xCoor;
		}
		public double getyCoor() {
			return yCoor;
		}
		public void setyCoor(double yCoor) {
			this.yCoor = yCoor;
		}
		public double getRadius() {
			return radius;
		}
		public void setRadius(double radius) {
			this.radius = radius;
		}
    	
    }
    
    
    
    public static StorageTankReader getInstance() throws IOException, Exception{
    	   if(instance == null) {
    	         instance = new StorageTankReader();
    	      }
    	      return instance;
    }


	
	
	 protected StorageTankReader() throws IOException, Exception{
		
		//////get all owlReader devices
		ArrayList<String> allNodeNames = OWLReader.read_owl_file("storagetankcomplete.owl", null);
		///////filter through to get a list

		
		for(String nodeName : allNodeNames){////IS IT A DEVICE NODE?
			if(nodeName.matches("^TankID_\\d+$")){//=>YES
				//////=>add  the node in DEVICE NAME LIST
             deviceNames.add(nodeName);
			}			
		}
		System.out.println(deviceNames.size());
		boolean first = true;
		
		tankDrawInfo = new StorageTank[deviceNames.size()];
		/////expand each device to get data
		for(String deviceName : deviceNames){
			OWLReader.read_owl_file("storagetankcomplete.owl", deviceName);
			//////copy to value array
			
			

			if (first){//if first device, extract the attribute list
				
				for(String dataName : OWLReader.name_list){
					////is it a distinct attribute?
						//BaseArea_TankID_0
						Pattern p = Pattern.compile("(\\w+)(_TankID_\\d+)(_?\\w*)");
						Matcher m = p.matcher(dataName);
						StringBuffer result = new StringBuffer();
						boolean found = false;
						//YES=>
						//delete the id and add it in
						while (m.find()) {
							m.appendReplacement(result, m.group(1) + ""+m.group(3));
						found = true;
						}
						m.appendTail(result);
						
						if(found){
							//System.out.println(result.toString());
							attributes.add(result.toString());
						}				
						}
				
				first = false;
			}
			
			///sort value list to the sequence of attribute list
			String[] temp = new String[attributes.size()];

			for(int idxValue =0 ; idxValue < OWLReader.value_list.size();idxValue++){
				String datAttriName = OWLReader.name_list.get(idxValue);
				for(int idxAttri = 0; idxAttri < attributes.size(); idxAttri++){
						Pattern p = Pattern.compile("(\\w+)(_TankID_\\d+)(_?\\w*)");
						Matcher m = p.matcher(datAttriName);
						StringBuffer result = new StringBuffer();
						boolean found = false;
						//YES=>
						//delete the id and add it in
						while (m.find()) {
							m.appendReplacement(result, m.group(1) + ""+m.group(3));
						found = true;
						}
						m.appendTail(result);
						
						if(found && result.toString().equals(attributes.get(idxAttri))){
							//System.out.println(result.toString());
							temp[idxAttri] =  OWLReader.value_list.get(idxValue);
						}			
						
									
				}
				
				
			}			
			
			for(int i =0 ; i < temp.length;i++){
				values.add(temp[i]);
				
			}			
		}
		

		
		
		
		



		}
	}
	
	
	
	

