package cam.dev.zhouxiaochi;

import java.io.IOException;

import com.esri.core.io.UserCredentials;
import com.esri.map.ArcGISFeatureLayer;

import cam.dev.zhouxiaochi.FeatureServiceUpdater.LayerType;

public class updateLayer {

public static String owlSource = "owl/BiodieselPlant3.owl";	
	
/* This class serves as a temporary tool for updating the ArcGIS database due to the structure change of the 
 * owl file. Meanwhile, such class can also be considered as a prototype of an automatic updating solution
 */



	public static void main(String[] args) throws IOException, Exception {

		String[] name = {"R-302"};		       		// the name of the device, please note that "-" is used here instead of "_"
		String[] type = 	{"chemReactor"}; 		// this piece of information should be found in map.txt in map folder
		int[] id_array = {148};      			    // the array of ids of the layers to be deleted 
										   			//by extending this array, multiple layers can be deleted at the same time 
			
		//=====================Delete the target layer=========================//
		FeatureServiceUpdater updater = new FeatureServiceUpdater(App.BASE_URL);
		updater.deleteLayers(id_array);
		//=====================================================================//
		
		
		
		
		//========== loop for multiple time 
		for(int counter = 0 ; counter < id_array.length ; counter ++)
		{ 
		//=============recreate the layer basing on the new data structure=====//
		App.createLayer(name[counter], owlSource, null);
		//=====================================================================//
		
		 
		
		//================read the data from target owl file===================//
		App.readAllEntityList();
		App.readDeviceListFromOwl();
		OWLReader.read_owl_file(owlSource, name[counter]);
		//=====================================================================//
		
		
		
		//=====================Initialize the target layer=====================//
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); 
		ArcGISFeatureLayer layer = new ArcGISFeatureLayer(App.BASE_URL + "/" + id_array[counter],user);	
		layer.initializeAsync();
		while(!layer.isAllowGeometryUpdates())
		{
			Thread.sleep(500);
		}
		//=====================================================================//
		
		
		
		//======================write new data into the layer==================//
		App.writeToLayer(name[counter],layer,OWLReader.x, OWLReader.y,type[counter]);
		//=====================================================================//
		}
		
		
		
		
	}

}
