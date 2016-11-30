package cam.dev.zhouxiaochi;

import java.awt.EventQueue;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.xml.parsers.ParserConfigurationException;

import org.json.JSONException;
import org.xml.sax.SAXException;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.DataOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.Geometry;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Point;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.FeatureSet;
import com.esri.core.map.Graphic;
import com.esri.core.renderer.SimpleRenderer;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.tasks.ags.query.Query;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.ArcGISFeatureLayer.SELECTION_METHOD;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.JMap;
import com.esri.map.LayerList;
import com.esri.map.MapOptions;
import com.esri.map.MapOptions.MapType;
import com.esri.map.MapOverlay;
import com.esri.map.QueryMode;

 

public class App {
	
	public static int layerID;
	public static ArcGISFeatureLayer testLayer;
	public static  JCheckBox chinButton;
	public static String target;
	public static String[] nameList;
	  public static void create_object(double x, double y) throws SAXException, IOException, ParserConfigurationException
	  {
		
	 
		    testLayer.setOperationMode(QueryMode.SELECTION_ONLY);
		    
		    FeatureWriter featurewriter = new FeatureWriter();
		    if(testLayer.getDefaultSpatialReference() == null)
		    {
		    	JOptionPane.showMessageDialog(null, "Sorry, the layer is not yet fully loaded");
		    }
		    else
		    {
		    Graphic[] g = featurewriter.createFeature("heater", x, y,testLayer.getDefaultSpatialReference());
		    testLayer.applyEdits(g, null, null, null);
		    
		    }
		  }
 
	  
	  private class MouseMoveOverlay extends MapOverlay  {
		    private static final long serialVersionUID = 1L;

		    @SuppressWarnings("deprecation")
			@Override
		    public void onMouseClicked(MouseEvent arg0) {
		     
		   Query query = new Query();
		   Point clickPointOnBaseLayer = map.toMapPoint(arg0.getX(), arg0.getY());
	
		   Point clickPointOnFeatureLayer = (Point) GeometryEngine.project(
		            clickPointOnBaseLayer,
		            map.getSpatialReference(),
		            testLayer.getDefaultSpatialReference());
		    	
		   Geometry queryExtent = new Envelope(clickPointOnFeatureLayer, 0.5, 0.5);
	        query.setGeometry(queryExtent);
	        
	        SELECTION_METHOD featureSelectionMethod = SELECTION_METHOD.SUBTRACT;
	        
	        
		    	if(chinButton.isSelected())
		    	{
		    	try {
		        if (!map.isReady()) {
		          return;
		        }
		        
		        java.awt.Point screenPoint = arg0.getPoint();
		        com.esri.core.geometry.Point mapPoint = map.toMapPoint(screenPoint.x, screenPoint.y);

		       
		        String mapCoords = "Map Coordinates: X = " + mapPoint.getX()
		            + ", Y = " + mapPoint.getY();
		       System.out.println(mapCoords);
		       
		       double x = mapPoint.getX();
		       double y = mapPoint.getY();
		       
		       try {
				create_object(x,y);
			} catch (SAXException | IOException | ParserConfigurationException e) {
				 
				e.printStackTrace();
			}
		       System.out.println(testLayer.getSelectedFeatures());

		      } finally {
		        super.onMouseClicked(arg0);
		      }
		    	
		    	}
		    	else
		    	{
		    		 try {
		    	          testLayer.selectFeatures(
		    	              query,
		    	              featureSelectionMethod,
		    	              new CallbackListener<FeatureSet>() {

		    	                @Override
		    	                public void onError(Throwable e) {
		    	                  e.printStackTrace();
		    	                }

		    	                @Override
		    	                public void onCallback(FeatureSet objs) {
		    	                  
		    	                }

		    	              }
		    	              );
		    	        } catch (Exception e) {
		    	          e.printStackTrace();
		    	        }
		    	 
		    	}
		    	 
		    }
		  }
	
	
  final static SimpleFillSymbol testcolor = new SimpleFillSymbol(Color.black, new SimpleLineSymbol(Color.cyan, 1), SimpleFillSymbol.Style.SOLID);

  private JFrame window;
  private JMap map;

  public App() throws Exception {
	    
	  chinButton = new JCheckBox("Add");
	
	  chinButton.setMnemonic(KeyEvent.VK_C); 
	  chinButton.setSelected(true);
	  chinButton.setSize(50,50);
	  chinButton.setLocation(0, 0);
	  chinButton.setText("Add");
	  
	  
	  JButton Load_feature = new JButton("Load Feature From OWL");
	  Load_feature.setLocation(65, 0);
	  Load_feature.setSize(250,60);
 
	  
	  
	  
	  
	  
	  
	  
	  
    window = new JFrame();
    window.setSize(800, 600);
    window.setLocationRelativeTo(null); // center on screen
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    window.getContentPane().setLayout(new BorderLayout(0, 0));
    window.add(chinButton);
    window.add(Load_feature);
    // dispose map just before application window is closed.
    
    
    
    window.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent windowEvent) {
        super.windowClosing(windowEvent);
        map.dispose();
      }
    });
    
    
   
    Load_feature.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    			try {
					create_object(OWLReader.x,OWLReader.y);
				} catch (SAXException | IOException | ParserConfigurationException e) {
					// TODO Auto-generated catch block
					
					e.printStackTrace();
				}
    	}
    });
    
   
    
 
    
    

    MapOptions mapOptions = new MapOptions(MapType.TOPO);
    map = new JMap(mapOptions);

    ArcGISTiledMapServiceLayer tiledLayer = new ArcGISTiledMapServiceLayer("http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");
    LayerList layerList = new LayerList();
    layerList.add(tiledLayer);
    UserCredentials user = new UserCredentials();
    user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
    testLayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/test003/FeatureServer/" + layerID, user);
	 
    OWLReader.read_owl_file(null, target);

    
    
    
	//String[] nameList = { "Field1", "Field2" }; 
    
    
  
 
    
	int length = OWLReader.name_list.size();
	
	String[] typeList = new String[length];
	 nameList = new String[length];
	
	
	System.out.println("--------------------------------------");
	
	for(int i = 0; i < length; i++)
	{
		typeList[i] = "esriFieldTypeString";
	}
	
	for(int i = 0; i < length; i++)
	{
		nameList[i] = i + OWLReader.name_list.get(i);
		System.out.println(">>>>>" + nameList[i]);
	}
	

	
	  
	   
	//construct name map of lists
	Map<String, String[]> attrLists = new HashMap<String, String[]>();

	attrLists.put("name", nameList);
	attrLists.put("type", typeList);
	attrLists.put("alias", nameList);
	int lengthOfEachList = nameList.length;

	System.out.println("----------" + nameList);
	System.out.println("Find me" + nameList.length);
 	FeatureServiceUpdater.generateLayer(lengthOfEachList, attrLists);
	  
	  //   testLayer.initializeAsync(); 
    SimpleRenderer renderer = new SimpleRenderer(testcolor);
    testLayer.setRenderer(renderer);
 
    layerList.add(testLayer);
    layerList.add(tiledLayer);
 
    map.getLayers().add(testLayer);
    Point mapCenter = new Point(11543665,141400);
    map.setExtent(new Envelope(mapCenter,7200,5400));
    map.addMapOverlay(new MouseMoveOverlay());
    window.getContentPane().add(map);
    
  }

  /**
   * Starting point of this application.
   * @param args
 * @throws Exception 
 * @throws IOException 
   */
  public static void main(String[] args) throws IOException, Exception {
	  	System.out.println("Please Input layerID");
	  	layerID =  51;
	   
	  
	  	System.out.println("ID is " + layerID);
	  	target = "Reactor_10D01";
	  	
		OWLReader.read_owl_file(null, target);
		 System.out.println("Give  you some thing " + OWLReader.name_list);
		 
		 System.out.println(OWLReader.x);
		 System.out.println(OWLReader.y);
		
		 
		 
    EventQueue.invokeLater(new Runnable() {

      @Override
      public void run() {
        try {
          App application = new App();
          application.window.setVisible(true);
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    });
  }
}
