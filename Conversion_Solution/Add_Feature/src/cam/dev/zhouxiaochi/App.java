package cam.dev.zhouxiaochi;

import java.awt.EventQueue;

import javax.swing.JCheckBox;
import javax.swing.JFrame;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;

import com.esri.runtime.ArcGISRuntime;
import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.CoordinateConversion.UTMConversionMode;
import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.Geometry;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
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
	
	public static ArcGISFeatureLayer testLayer;
	public static  JCheckBox chinButton;
	
	  public static void create_object(double x, double y)
	  {
		  Polygon polygon = new Polygon();
		  polygon.startPath(x,y);
		  polygon.lineTo(x +  1,y );
		  polygon.lineTo(x +  1,y + 1);
		  polygon.lineTo(x  ,y + 1);
		  polygon.closePathWithLine();
		  
		  SimpleLineSymbol outline = new SimpleLineSymbol(new Color(0, 150, 0), 300);
		  SimpleFillSymbol symbol2 = new SimpleFillSymbol(new Color(0, 240, 0, 180), outline);
		  
		   Map<String,Object> attributes = new HashMap<String,Object>();
		    attributes.put("Name", "Test Object");
			
		    Graphic polygonGraphic = new Graphic(polygon, symbol2,attributes);
		    Graphic[] adds = {polygonGraphic};
		    testLayer.applyEdits(adds, null, null, null);
		    testLayer.setOperationMode(QueryMode.SELECTION_ONLY);
		    
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
		       
		       create_object(x,y);
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
		    	                  System.out.println("Total selected features : " +
		    	                      testLayer.getSelectedFeatures().length);
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

  public App() throws InterruptedException {
	    chinButton = new JCheckBox("Add");
	    chinButton.setMnemonic(KeyEvent.VK_C); 
	    chinButton.setSelected(true);
	    chinButton.setSize(50,50);
	    chinButton.setLocation(200, 200);
	    chinButton.setText("Add");
	    
	    
	  
    window = new JFrame();
    window.setSize(800, 600);
    window.setLocationRelativeTo(null); // center on screen
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    window.getContentPane().setLayout(new BorderLayout(0, 0));
    window.add(chinButton);
    // dispose map just before application window is closed.
    window.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent windowEvent) {
        super.windowClosing(windowEvent);
        map.dispose();
      }
    });

    MapOptions mapOptions = new MapOptions(MapType.TOPO);
    map = new JMap(mapOptions);

    ArcGISTiledMapServiceLayer tiledLayer = new ArcGISTiledMapServiceLayer("http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");
    LayerList layerList = new LayerList();
    layerList.add(tiledLayer);
    UserCredentials user = new UserCredentials();
    user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
    testLayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/test004/FeatureServer/0", user);

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
   */
  public static void main(String[] args) {
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
