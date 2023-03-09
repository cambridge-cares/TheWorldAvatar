import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polyline;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.map.ArcGISFeatureLayer;

public class App {
 
	public static String[] targets;
	public static String[] types;
	
	public static ArrayList<Point> deviceCoordinates = new ArrayList<Point>();				  // stores the coordinates of the devices 
	public static ArrayList<Point[]> pipeLineCoordinates = new ArrayList<Point[]>();	  // stores the coordinates of the start and end point of all pipelines 
	public static Set<ArrayList<Point>> allpipelist = new HashSet<ArrayList<Point>>(); // in a set, all the elements are unique
	public static Map<Integer,Pipe> device_pipe_map = new HashMap<Integer,Pipe>();                             // maps the devices and pipelines connected to it , the index of the pipes  are the key 
	public static ArcGISFeatureLayer  referencelayer;
	public static Set<Integer> index_list = new HashSet<Integer>();
	
	public static ArrayList<Pipe> pipeList = new ArrayList<Pipe>();															// a set that stores all the pipe as an independent class 
	
	
	public static class Pipe // In a Pipe class, the String Set devices stores the names of the one or two devices that is connected by the pipe , Point ArrayList coordinates stores the geometry of the pipe 
	{
		public  Set<String> devices = new HashSet<>();
		public  ArrayList<Point> coordinates = new ArrayList<Point>();
	} 
	
	public static void main(String[] args) throws IOException, Exception {
	
		
		
		// TODO Auto-generated method stub
		 
		// 1 . read the owl files, kml files, get the coordinates of the equipments 
		// 2. read the kml file of Material Line 	
		// 3. compare the coordinates and identify the id of the pipeline, store the device name in the pipe instance
	    // 4. all the final result are stored in an ArrayList of Pipe named pipeList
		
		readlist();
		
		
		
	    UserCredentials user = new UserCredentials();
	    user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
	    referencelayer = new ArcGISFeatureLayer
				("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/linelayer003/FeatureServer/0", user); // currently, referencelayer is the linelayer 
		
		referencelayer.initializeAsync();
		while(!referencelayer.isAllowGeometryUpdates()) // stop the loop when the referencelayer is initialized / fully loaded. sleep for 100 milliseconds in order to save computing capability. 
		{
			Thread.sleep(100);
		}
		
		
		// loop through all the targets
		// This loop gets all the coordinates from the devices. 
		
		// Get the starting and ending point of Material Lines
		
		KMLReader reader = new KMLReader();
		reader.readkml(referencelayer);
		pipeLineCoordinates = KMLReader.pipeLineCoordinates;
		
		//  
		for(int i = 0 ; i < targets.length ; i ++)
		{
			String device_name = targets[i];                  // represents the name of the device of this iteration, it will be used in the Pipe class's set of devices 
			OWLReader.read_owl_file(null, targets[i]);
			double x = OWLReader.x ;
			double y = OWLReader.y ;
			 
		    FeatureWriter.createFeature(types[i], x, y, null, targets[i]);
		    deviceCoordinates =  FeatureWriter.CoordinateList;
		  
		    int counter_pipe = 0;
		    for( Point[] point_array : pipeLineCoordinates)  // iterate through all the pipeLines, calculate the distance between all the points of a device and each of their two ends 
		    {
		    	//--------------------------starting point of a new pipe
		    	int device_connection_counter = 0;
		    	Pipe this_pipe = new Pipe();
		    	
		    	Point startpoint =  point_array[0];                      // one end of the pipeline
		    	Point endpoint =  point_array[1];   						// another end of the pipeline
		    	// for(Point device_point : deviceCoordinates)
		    	for(int index = 0 ; index < deviceCoordinates.size(); index++)
		    	{
		    		Point device_point_1 = null;
		    		Point device_point_2 = null;
		    	//	boolean start_on_device = CalculateDistance(startpoint,device_point, 0.1); // whether the startpoint is close enough to the device 
		    	//	boolean end_on_device = CalculateDistance(endpoint,device_point, 0.1);    // whether the endpoint is close enough to the device 
		    																																		// also calculate whether the start / end point is on the line between two points
 		    		if(index + 1 < deviceCoordinates.size())
 		    		{
 		    			device_point_1  = deviceCoordinates.get(index);
 		    			device_point_2 = deviceCoordinates.get(index + 1);
 		    		}
 		    		else
 		    		{
 		    			device_point_1  = deviceCoordinates.get(0);
 		    			device_point_2 = deviceCoordinates.get(index);
 		    		}
		    	
		    		boolean start_on_device = IsOnTheLine(device_point_1,device_point_2,startpoint, 0.02);
		    		boolean  end_on_device = IsOnTheLine(device_point_1,device_point_2,endpoint, 0.02);
		    		
		    		if(start_on_device || end_on_device)
		    		{
		    			
		    			
		    			System.out.println("-----A pipe is on the device---" + targets[i] + "------" + counter_pipe);
		    			allpipelist.add(KMLReader.coordinates.get(counter_pipe)); // when a pipe's start or end point is connected to a device in targets[], store it in set allpipelist
		    			index_list.add(counter_pipe);
		    			this_pipe.coordinates = KMLReader.coordinates.get(counter_pipe);
		    			this_pipe.devices.add(device_name);
		    			device_connection_counter++;
		    			 
		    		} 
		    	}
		    	
 		    	if(device_connection_counter >= 1)
		    	{
		    		pipeList.add(this_pipe);
		    	}
		    	
		    	counter_pipe++;
		    }
		     
		}
 
                 //		drawPipe();			 //uncomment this only when you want to visually examine the result 
	/*
		for(int index : index_list)
		{
			System.out.println("index ---> " + index);
			pipeList.add(device_pipe_map.get(index)); 
		}
		
		*/
		
		
		
		PrintTheResult();
		
		
		 
	}
	
	
	
	 // draw the pipeline stored in pipe list on ArcGIS map
	 // iterate through the set allpipelist 
	 // this is to test whether the calculation is accurate or not
	
	
	
	
	
	  public static void drawPipe()
	  { 
		  
		 
	 	  SimpleLineSymbol outline2 = new SimpleLineSymbol(new Color(133, 234, 0), 300);
	 	  System.out.println("allpipelist size --> " + allpipelist.size());
		  Graphic[] adds = new Graphic[allpipelist.size()];// declare an array of graphic to store the features to be written into ArcGIS
		  for(int i = 0 ; i < allpipelist.size();i++)
		  {
			  
			  Map<String, Object> attributes = new HashMap<>();
			  
			  attributes.put("Name", String.valueOf(i));
			  
			  
			  
	          Polyline pipeLine = new Polyline();
	          ArrayList<Point> pipe = (ArrayList<Point>) allpipelist.toArray()[i]; 
	          pipeLine.startPath(pipe.get(0).getX(), pipe.get(0).getY()); // set the start point of a pipeline
	          
	          for(int j = 1 ; j < pipe.size(); j++)
	          { 
	        	  pipeLine.lineTo(pipe.get(j).getX(),  pipe.get(j).getY());
	          }
	 
	          Graphic polygonGraphic = new Graphic(pipeLine, outline2,attributes);
	          adds[i] = polygonGraphic; // add the newly generated pipeLine into the adds[] array, which would be submitted into ArcGIS
		  }
		  
		  referencelayer.applyEdits(adds, null, null, null);
		   
	  }
	
	
	  public static void readlist() 
	  {
		  ArrayList<String> temp = new ArrayList<String>();
		  try (BufferedReader br = new BufferedReader(new FileReader("map.txt"))) {

				String sCurrentLine;

				while ((sCurrentLine = br.readLine()) != null) {
					temp.add(sCurrentLine);
				}

			} catch (IOException e) {
				e.printStackTrace();
			}
		  
		  targets = new String[temp.size()];
		  types = new String[temp.size()];
		  int counter = 0; 
		  for(String item : temp)
		  {
			 targets[counter] = item.split("#")[0];
		     types[counter]    = item.split("#")[1];
		     
	//	     System.out.println(item.split("#")[0]);
	//	     System.out.println(item.split("#")[1]);
			  counter++;
		  }
		  
		  
	  }

		/***
		 * calculate whether the pipeline touches the boundary of the device
		 * @param device_point_1 , one of the point on the device
		 * @param device_point_2 , one of the point on the device
		 * @param pipe_point , one end of the pipe
		 * @param threshold , the threshold to identify whether the pipe is connected 
		 *  @return result , the boolean that tells whether the pipe is connected
		 */
  
	  public static boolean IsOnTheLine(Point device_point_1, Point device_point_2 , Point pipe_point, double threshold)
	  {
		  boolean result = false;
		   // calculate the distance between each pair of points, if the sum of the two distance is very close to the other  distance, it is on the line.
 
		  
		  double distance_1 = Math.sqrt(Math.pow((device_point_1.getX() - pipe_point.getX()), 2) + Math.pow((device_point_1.getY() - pipe_point.getY()), 2) ); // the distance between device_point_1 & pipe_point
		  double distance_2 = Math.sqrt(Math.pow((device_point_2.getX() - pipe_point.getX()), 2) + Math.pow((device_point_2.getY() - pipe_point.getY()), 2) ); // the distance between device_point_2 & pipe_point
		  double distance_3 = Math.sqrt(Math.pow((device_point_1.getX() - device_point_2.getX()), 2) + Math.pow((device_point_1.getY() - device_point_2.getY()), 2) ); // the distance between device_point_1 & device_point_2
		  // get the distance between each pair of coordinate points... 
		  double difference = Math.abs((distance_1 + distance_2) - distance_3);
		  // if the end of the pipe is exactly on the boundary, the difference will be 0 
		
		  
		  if(difference < threshold)
		  {
			  result = true; // the pipe is connected 
		/**
			  System.out.println("-----> distance_1 --" + distance_1);
			  System.out.println("-----> distance_2 --" + distance_2);
			  System.out.println("-----> distance_3 --" + distance_3);
			  System.out.println("-----> difference --" + difference);
			  **/
		  }
		  else
		  {
			  result = false;
		  }
		  
		  
		  return result; 
	  }
	  
	  
	  
	  
	  // print out the set of pipes  
	  public static void PrintTheResult()
	  {
		  int counter = 0; 
		  for(Pipe pipe : pipeList)
		  {
			  counter++;
			  System.out.println("-----------PIPE NO." + counter +"---- Devices Connected");
			  String device1 = (String) pipe.devices.toArray()[0];
			  String device2 = (String) pipe.devices.toArray()[pipe.devices.toArray().length - 1];
			  System.out.println("device 1 -- " + device1 );
			  System.out.println("device 2 -- " + device2 );

			  System.out.println("------------------------------------------------------------------------------");
		  }
		  
	
	  }
	 
	  
} 
 



















