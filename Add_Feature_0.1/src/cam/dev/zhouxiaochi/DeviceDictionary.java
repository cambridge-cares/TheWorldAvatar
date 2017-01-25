package cam.dev.zhouxiaochi;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
/***
 * Not used currently, kept for furture development. identitfy type with name(However currently name abbrev is not unique for type!)
 * @author Shaocong
 *
 */
public class DeviceDictionary {

	public static DeviceDictionary singelton = null;
	
	private Map<String,String> nameDictionary = null; 
	
	private static final String MAP_FILE_LOCATION = "deviceMap.txt";
	private static final String ERR_MSG_MAL_FORMAT_MAP_FILE = "Wrong format in map txt!!USE , to seperate data";
	private static final String ERR_MSG_MAL_FORMAT_DEVICE_NAME = "Wrong format of device name!!, correct: V-301, error name:";

	private DeviceDictionary() throws IOException{
		
		nameDictionary = new HashMap<String, String>();
		try (BufferedReader br = new BufferedReader(new FileReader(MAP_FILE_LOCATION))) {

			String sCurrentLine;

			while ((sCurrentLine = br.readLine()) != null) {
				if (sCurrentLine.split(",").length != 2){
					System.out.println(ERR_MSG_MAL_FORMAT_MAP_FILE);
					return; 
				}
			
				String[] splitStrs =sCurrentLine.split(",");
				nameDictionary.put(splitStrs[0], splitStrs[1]);
			}			
		} 
		
		
		
	}
	
	
	public static DeviceDictionary getInstance() throws IOException{
		if (singelton == null){
			singelton = new DeviceDictionary();
		}
		return singelton;
	}
	
	public String getTypeName(String deviceName){
		//delete ID from device name
		String[] splitStrs =deviceName.split("-");
		if (splitStrs.length != 2){
			System.out.println(ERR_MSG_MAL_FORMAT_DEVICE_NAME);
			return null;
		}
		return nameDictionary.get(splitStrs[0]);
	}
	
}
