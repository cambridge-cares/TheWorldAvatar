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


/***
 * Uses to owlreader to read all storage tank information from owl. Note that currently storage tank has its own owl file.
 * Singelton class, only has one instance that stores all information.
 * @author Shaocong
 *
 */
	public class StorageTankReader {
		

		private static StorageTankReader instance = null;//singelton instance
	public  ArrayList<String> attributes = new ArrayList<String>();//attribute name list
	public ArrayList<String> deviceNames = new ArrayList<String>();//device(storage tank) name list
    public ArrayList<String>  values = new ArrayList<String>();//value of all data

    public StorageTank[]   tankDrawInfo;//Array of tank infos
    
    /***
     * Storage tank info data class.
     * Private attris: x, y, radius. Setter& Getter defined for each.
     * @author Shaocong
     *
     */
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
    
    
    /***
     * static method to return the singelton instance of this class to client
     * @return StorageTankReader: the only instance of this class
     * @throws IOException
     * @throws Exception
     */
    public static StorageTankReader getInstance() throws IOException, Exception{
    	   if(instance == null) {
    	         instance = new StorageTankReader();
    	      }
    	      return instance;
    }


	
	/***
	 * Constructor. Initiate and populate value list, attribute name list, device name list from owl file.
	 * Utilizing OWLReader.
	 * @throws IOException
	 * @throws Exception
	 */
	 protected StorageTankReader() throws IOException, Exception{
		
		//////get all owlReader first level nodes
		ArrayList<String> allNodeNames = OWLReader.read_owl_file(App.storageInfo.owlSource, null);
		///////filter through to get a list of actual devices
		for(String nodeName : allNodeNames){////DOES THE NAME MATCH THIS PATTERN?
			if(nodeName.matches("^TankID_\\d+$")){//=>YES
				//////=>add name of the node in DEVICE NAME LIST
             deviceNames.add(nodeName);
			}			
		}
	//	System.out.println(deviceNames.size());
		boolean first = true;//boolean flag: if first time in looping all devices
		
		tankDrawInfo = new StorageTank[deviceNames.size()];//initiate tank info array
		for(String deviceName : deviceNames){		/////loop through all devices
			OWLReader.read_owl_file(App.storageInfo.owlSource, deviceName);//call owl reader to extract all data node of this device
			
			if (first){//if first device, construct the attribute name list				
				for(String dataName : OWLReader.name_list){/////for all data's name of this device 
					////is it a distinct name?
						//BaseArea_TankID_0
						Pattern p = Pattern.compile("(\\w+)(_TankID_\\d+)(_?\\w*)");//define regex pattern
						Matcher m = p.matcher(dataName);
						StringBuffer result = new StringBuffer();
						boolean found = false;//flag: if indeed find a match of regex
						while (m.find()) {// find a match?				
							m.appendReplacement(result, m.group(1) + ""+m.group(3));//delete the id part of data name 
						found = true;//set flag found to be true
						}
						m.appendTail(result);
						if(found){//indeed has match?
							attributes.add(result.toString());//add name excludes id part to attri name list
						}				
						}
				
				first = false;//set first flag to be false
			}
			
		/****rearrange value list to be the exact sequence of attribute name list**************/
			String[] temp = new String[attributes.size()];//temp array to store the data value of one device

			///loop through data name and attribute name, put data into temp array at corresponding place of attribute name list
			for(int idxValue =0 ; idxValue < OWLReader.name_list.size();idxValue++){//loop through all data name of one device
				String datAttriName = OWLReader.name_list.get(idxValue);
				for(int idxAttri = 0; idxAttri < attributes.size(); idxAttri++){//loop through attribute name list
						Pattern p = Pattern.compile("(\\w+)(_TankID_\\d+)(_?\\w*)");//define regex pattern
						Matcher m = p.matcher(datAttriName);
						StringBuffer result = new StringBuffer();
						boolean found = false;
					
						while (m.find()) {//if find pattern match
							m.appendReplacement(result, m.group(1) + ""+m.group(3));//add to result the name without id part
						found = true;
						}
						m.appendTail(result);
						
						if(found && result.toString().equals(attributes.get(idxAttri))){//if find pattern match &result equals one in attribute name list
							//System.out.println(result.toString());
							temp[idxAttri] =  OWLReader.value_list.get(idxValue);//put data value into corresponding place in temp array
						}			
						
									
				}
				
				
			}			

			//add rearranged temp value list to final value list
			for(int i =0 ; i < temp.length;i++){//
				values.add(temp[i]);
				
			}
	////\
	///||
	///||
	/// ==== loop back to top to process next device
		}
		

		
		
		
		



		}
	}
	
	
	
	

