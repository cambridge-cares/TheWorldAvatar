package cam.dev.zhouxiaochi;
/* ArcGIS Online Account that stores hosted feature layers:
 * Username: jparksimulator
 * Password: c4tjpark
 */
/* CURRENTLY KNOWN ISSUES THAT CAN BE IMPROVED:
 * 3. No method to extract layers from LayerList map.getLayers(), have to manually add layers to array ArcGISFeatureLayer[] 


 * 4. Cannot draw and edit features yet
 */

// For more information and API reference on the ArcGIS SDK for Java, go to https://developers.arcgis.com/java/
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;       




import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.xml.sax.SAXException;

import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.MultiPoint;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polyline;
import com.esri.core.geometry.Transformation2D;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Feature;
import com.esri.core.map.Graphic;
import com.esri.core.portal.Portal;
import com.esri.core.portal.WebMap;
import com.esri.core.renderer.SimpleRenderer;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol.Style;
import com.esri.core.symbol.Symbol;
import com.esri.map.ArcGISDynamicMapServiceLayer;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.GraphicsLayer;
import com.esri.map.GroupLayer;
import com.esri.map.JMap;
import com.esri.map.Layer;
import com.esri.map.LayerEvent;
import com.esri.map.LayerList;
import com.esri.map.LayerListEventListenerAdapter;
import com.esri.map.MapEvent;
import com.esri.map.MapEventListenerAdapter;
import com.esri.map.QueryMode;
import com.esri.map.popup.PopupDialog;
import com.esri.map.popup.PopupView;
import com.esri.map.popup.PopupViewEvent;
import com.esri.map.popup.PopupViewListener;
import com.esri.runtime.ArcGISRuntime;
import com.esri.toolkit.legend.JLegend;
import com.esri.toolkit.overlays.HitTestEvent;
import com.esri.toolkit.overlays.HitTestListener;
import com.esri.toolkit.overlays.HitTestOverlay;
import com.esri.toolkit.overlays.InfoPopupOverlay;
import com.esri.core.symbol.PictureMarkerSymbol;

 

public class JParkSim {
	
 
		 
		public static int[] count;
	// style of different layers
	
	 
		final static SimpleFillSymbol testColor =new SimpleFillSymbol(Color.green, new SimpleLineSymbol(Color.blue, 1), SimpleFillSymbol.Style.SOLID);
		final static SimpleLineSymbol lineColor = new SimpleLineSymbol(Color.blue, 1);
		 

		
		static int layerID;
	
	private JFrame window;
	private JMap map;
	//try to put new variable
	
	private GraphicsLayer graphicsLayer;
	
	private MultiPoint planes;
	
				
	//if want to add new map
	  private HashMap<String, String> idMap;
	  private JComboBox mapIds;
	  private Portal arcgisPortal = new Portal("https://www.arcgis.com", null);
	  
//	private JLayerTree jLayerTree;  //ZL-151207 add layertree
	public static JLayeredPane contentPane;
		
	// initialize layers
	public static String editedValue;
	public static String editedName;
	
	
	public static ArcGISFeatureLayer testLayer;
 
	
	
 	public static String httpStringCSV = new String("D:/httpReq.CSV"); // (mjk, 151115) investigating structure of DataOutputStream object
 	public static String httpStringCSV1 = new String("D:/httpReq1.CSV"); // (ZL-151203) investigating structure of DataOutputStream object
 	public static String httpStringCSV2 = new String("D:/httpReq2.CSV"); // (ZL-151203) investigating structure of DataOutputStream object
 	
 	

 	
	
	// method to render all layers in an array using a certain style (multiple layer renderer)
	private void createRenderer(LayerList layers, ArcGISFeatureLayer[] arrayoflayers, Symbol col) {
		for (ArcGISFeatureLayer layer : arrayoflayers) {
			layer.setRenderer(new SimpleRenderer(col));
			layers.add(layer);
		}
	}
	
	
	
	
	//add link for webmap graph
	Portal portal = new Portal("http://www.arcgis.com",null);
	  // item ID of a public map on arcgis.com with charts
	  final String MAP_ID = "f809dccb780a4af0a506e56aaa84d084";
	  
	  
	
  public JParkSim() throws IOException, Exception {
	  
 
 
 
    // empty JMap constructor and add a tiled basemap layer
    map = new JMap();
    final LayerList layers = map.getLayers(); // object storing all the map layers (NOT AN ARRAY - use completelayerlist instead)
    ArcGISTiledMapServiceLayer tiledLayer = new ArcGISTiledMapServiceLayer("http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");
    layers.add(tiledLayer); // add basemap layer


// layer for the emission
ArcGISDynamicMapServiceLayer emissionLayer = new ArcGISDynamicMapServiceLayer(
            "http://localhost:6080/arcgis/rest/services/emission/MapServer");
                layers.add(emissionLayer);
    
                
    // map centered on Jurong Island
    Point mapCenter = new Point(11543665,141400);
    map.setExtent(new Envelope(mapCenter,7200,5400));
    map.addMapEventListener(new MapEventListenerAdapter() {
    	@Override
    	public void mapReady(MapEvent event) {
    		System.out.println("Map has finished loading");
    	}
    });
  
    // adds layers uploaded onto ArcGIS for Developers
    UserCredentials user = new UserCredentials();
    user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
   
 
   // testLayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST017/FeatureServer/9", user);
    ArcGISFeatureLayer linelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/linelayer003/FeatureServer/0", user);
    ArcGISFeatureLayer buildinglayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/BuildingLayer/FeatureServer/0", user);

 
 
   //================================================================
    App.readlist();
   String[] targets =  App.targets;
 //  ArcGISFeatureLayer[] completeLayerList =  {testLayer};
   ArcGISFeatureLayer[] completeLayerList =  new ArcGISFeatureLayer[targets.length + 2];
   
    for(int i = 0; i < targets.length; i++)
    {
 
    ArcGISFeatureLayer  newLayer = new ArcGISFeatureLayer(
   "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST017/FeatureServer/" + targets[i], user);
    
    completeLayerList[i] = newLayer;
    createRenderer(layers, new ArcGISFeatureLayer [] {newLayer}, testColor);
    
    }
    
    completeLayerList[completeLayerList.length - 2] = linelayer;
    createRenderer(layers, new ArcGISFeatureLayer [] {linelayer}, lineColor);
    
    completeLayerList[completeLayerList.length - 1] = buildinglayer;
    createRenderer(layers, new ArcGISFeatureLayer [] {buildinglayer}, testColor);
    
 
    ArcGISDynamicMapServiceLayer highwayLayer = new ArcGISDynamicMapServiceLayer(
            "http://localhost:6080/arcgis/rest/services/opex/MapServer");
                layers.add(highwayLayer);
          

                ArcGISDynamicMapServiceLayer sensitivityLayer = new ArcGISDynamicMapServiceLayer(
                        "http://localhost:6080/arcgis/rest/services/sensitivity/MapServer");
                            layers.add(sensitivityLayer);
                
                 
                            
    // initialize window
    window = new JFrame("J-Park Simulator");
    window.setSize(1200, 900);
    window.setLocationRelativeTo(null); // centered on screen
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    window.getContentPane().setLayout(new BorderLayout(0, 0));
   
    
     
    
    
	// create panel to select layer to edit
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.setSize(220, 175);
    panel.setLocation(260, 10); // located near top left next to legend
    
    // command to switch the map 22/3/2016
    
    String[] mapStrings = {
        
    "cost of reactors"};
    String[] idStrings = {
         
    "f809dccb780a4af0a506e56aaa84d084"};
    idMap = new HashMap<>();
    for (int i = 0; i < idStrings.length; i++) {
      idMap.put(mapStrings[i], idStrings[i]);
    }

    mapIds = new JComboBox(mapStrings);
    mapIds.setSelectedIndex(0);
    mapIds.setAlignmentX(Component.LEFT_ALIGNMENT);

    //JButton button = createButton();

    
    
    // create text
    JTextArea description = new JTextArea("Click on a feature to start editing");
    description.setFont(new Font("Verdana", Font.PLAIN, 11));
    description.setForeground(Color.WHITE);
    description.setBackground(new Color(0, 0, 0, 0));
    description.setEditable(false);
    description.setLineWrap(true);
    description.setWrapStyleWord(true);
    description.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    // create label for dropdown selector
    JLabel lblLayer = new JLabel("Select a feature to edit:");
    lblLayer.setForeground(Color.WHITE);
    lblLayer.setAlignmentX(Component.LEFT_ALIGNMENT);
    // create dropdown selector for layer via key-value pairs
    final Map<String, ArcGISFeatureLayer> editlayer = new LinkedHashMap<>();
    // dropdown options with key = String layer name and value = layer object
    
 
   for(int i = 0 ; i < completeLayerList.length - 2  ; i++)
   {
	   editlayer.put(targets[i],completeLayerList[i]);
   }
    
    editlayer.put("linelayer", completeLayerList[completeLayerList.length - 2]);
    
    editlayer.put("buildinglayer", completeLayerList[completeLayerList.length - 1]);
    
    
    final JComboBox cbxLayer = new JComboBox(editlayer.keySet().toArray(new String[0]));	// initialize dropdown box
    cbxLayer.setMaximumSize(new Dimension(220, 25));
    cbxLayer.setAlignmentX(Component.LEFT_ALIGNMENT);
    
 // create text
    JLabel lblLayer2 = new JLabel("feature list to query:");
    lblLayer2.setForeground(Color.WHITE);
    lblLayer2.setAlignmentX(Component.LEFT_ALIGNMENT);    
    
 // create text
    JTextArea description3 = new JTextArea("press refresh to delete pin point marking");
    description3.setFont(new Font("Verdana", Font.PLAIN, 11));
    description3.setForeground(Color.WHITE);
    description3.setBackground(new Color(0, 0, 0, 0));
    description3.setEditable(false);
    description3.setLineWrap(true);
    description3.setWrapStyleWord(true);
    description3.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    
    ArrayList<String[]> editStack = new ArrayList<String[]>(); 							// create a stack of edited features for PowerWorld to execute on
 
    HitTestOverlay[] listenerList = new HitTestOverlay[completeLayerList.length];				// container for listeners arranged by index of layers in completeLayerList
    
    for (ArcGISFeatureLayer layer : completeLayerList) {										// add event listener to all layers in completeLayerList
        final HitTestOverlay hitTestOverlay = new HitTestOverlay(layer);						// listener is of type HitTestOverlay
        hitTestOverlay.addHitTestListener(new HitTestListener() {								// listens for MOUSE CLICK on feature
        	@Override
        	public void featureHit(HitTestEvent event) {
    	        HitTestOverlay overlay = event.getOverlay();
    	        Graphic hitGraphic = (Graphic) overlay.getHitFeatures().get(overlay.getHitFeatures().size()-1); // get bottom-most graphic hit by mouse-click
    	        try {
    	        	PopupView contentPanel = PopupView.createEditView("Edit Attributes", layer); // create a popup in edit view
    	        	contentPanel.setGraphic(layer, hitGraphic);
    		          for (ArcGISFeatureLayer somelayer : completeLayerList) {					// highlight selected graphic and unselect previously selected graphic by searching all layers
    		        	  if (somelayer.getSelectedFeatures() != null) {						// layers outside map extent will produce null error, ignore these
	    			          for (Graphic graphic : somelayer.getSelectedFeatures()) {			// search for selected features
	    			        	  somelayer.unselect((int) graphic.getId());					// unselect them by graphic Id
	    			          }
    		        	  }
    		          }
    		          layer.select((int) hitGraphic.getId());													// highlight selected graphic
    		          final PopupDialog popup = map.createPopup(new JComponent[]{contentPanel}, hitGraphic);	// create map popup to display the popup view
    		          popup.setTitle("Edit Attributes: " + layer.getName());
    		          popup.setVisible(true);
    		  
    		          contentPanel.addPopupViewListener(new PopupViewListener() {
    		              @Override
    		              public void onCommitEdit(PopupViewEvent popupViewEvent, Feature feature) {			// save button
//    		            	  String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("OBJECTID"))}; // newFeature is a new String[] element to be added to editStack (e.g. {Load_Points, 103})
//		               	      String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("FID")), String.valueOf(hitGraphic.getAttributes().get("OBJECTID"))};  //ZL-151209 try to get FID and OBJECTID 
//    		            	  String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("OBJECTID")), String.valueOf(hitGraphic.getAttributes().get("boilingpt"))}; 
    		            	
    		            	  
     		            	  for(String name : hitGraphic.getAttributeNames())
    		            	  {
    		            		  if(feature.getAttributes().get(name)!=null)
    		            		  {
    		            		String current_value =  feature.getAttributes().get(name).toString();
    		            		String previous_value = hitGraphic.getAttributes().get(name).toString();
    		            		 
    		            		
    		            		
    		            		if(!current_value.contentEquals(previous_value))
    		            		{
    		            		 System.out.println(current_value  + "--" + previous_value);
    		            		 System.out.println(name);
    		            		 editedValue = current_value;
    		            		 editedName = name;

    		            		 
    		            		}
    		            		 
    		            	  }
    		            	
    		            	  }
    		            	  
     		            	 String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("OBJECTID")), editedName + "plusValue" + editedValue}; 
    		            	  
    		            	  System.out.println("newFeature[0]=" + newFeature[0] + ", newFeature[1]=" + newFeature[1]); //ZL-151209
//double y= Double.parseDouble(newFeature[2]);
//double z=2*y;
//System.out.println("vbnew function=" +z);    		            	  
    		            	  //try to expand new button
    		            	  
    		            	  boolean addtoStack = true;
    		            	  System.out.println("editStack size=" + editStack.size());
    		             
    		            	  for (int i=0; i<editStack.size(); i++) {							// (mjk, 151120) check through (i) elements in editStack where (i) is the number of modified feature objects in the layers.
    		            		  String itemlayer = editStack.get(i)[0];
    		            		  String graphicOBJECTID = editStack.get(i)[1];
//    		            		  String appCallFlag = editStack.get(i)[2];   //ZL-151208
    		            		  if (layer.getName().equals(itemlayer) && (String.valueOf(hitGraphic.getAttributes().get("OBJECTID")).equals(graphicOBJECTID))) { 
//    		            		  if (layer.getName().equals(itemlayer) && (String.valueOf(hitGraphic.getAttributes().get("FID")).equals(graphicFID)||String.valueOf(hitGraphic.getAttributes().get("OBJECTID")).equals(graphicOBJECTID))) {	  
    		            			  addtoStack = false; 												// if identical feature is found, don't add to editStack
    		            		  }
    		            	  }  		            		  
    		            	  if (addtoStack) {															// add only if not duplicate
    		            		  editStack.add(newFeature);
    		            	  }
    		            	  
    		            	  for(String name : hitGraphic.getAttributeNames())
    		            	  {
    		            		  if(feature.getAttributes().get(name)!=null)
    		            		  {
    		            		String current_value =  feature.getAttributes().get(name).toString();
    		            		String previous_value = hitGraphic.getAttributes().get(name).toString();
    		            		 
    		            		
    		            		
    		            		if(!current_value.contentEquals(previous_value))
    		            		{
    		            		 System.out.println(current_value  + "--" + previous_value);
    		            		 System.out.println(name);  		         
    		            		}
    		            		 
    		            	  }
    		            	
    		            	  }
    		            	 
							
						 
    		          
    		            	  
    		              }
    		               
    		              
    		              @Override
    		              public void onCancelEdit(PopupViewEvent popupViewEvent, Feature feature) {	// cancel button
    		            	  layer.unselect((int) hitGraphic.getId());									// unselect feature on cancel
    		            	  popup.close();
    		              }
    		            });
    		          } catch (Exception e) {
    		            e.printStackTrace();
    		          }
        	}
        });
        hitTestOverlay.setActive(false);															// disable all listeners initially to prevent conflict
        listenerList[Arrays.asList(completeLayerList).indexOf(layer)] = hitTestOverlay;				// add listener(overlay) to an array arranged in indexed order
        map.addMapOverlay(hitTestOverlay);															// add all layer listeners to map
    }
    
   															// default layer listener enabled is the first one (landlots layer)
    for(int i = 0 ; i < listenerList.length;i++)
    {
    	 listenerList[i].setActive(true);	
    }
 
   
JButton change = new JButton("change to fin.data");
change.addActionListener(new ActionListener() {
	@Override
	public void actionPerformed(ActionEvent arg0) {
		
		try {
			String newId = idMap.get(mapIds.getSelectedItem());
        WebMap webMap = WebMap.newInstance(newId, arcgisPortal);
        map.loadWebMap(webMap);
        
        
		} catch (Exception e)  {
			e.printStackTrace();
		}
		
		
	}
});

change.setEnabled(true);
change.setVisible(true);
change.setSize(190,30);
change.setLocation(890, 45);
    

 
    
 
    JButton PWPrButton = new JButton("Run Pr PowerWorld");
    PWPrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
			//	url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
    			url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
    			
    			urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for PowerWorld!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
//					StringBuilder FIDs = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					StringBuilder editInfo = new StringBuilder();
					System.out.println("---------->" + editStack.size());
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
						OBJECTIDs.append(",");
						appCallFlag.append("PWPr");
						appCallFlag.append(",");
						editInfo.append(item[2]);
						
						System.out.println("EditInfo --> " +  item.length);
						editInfo.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					
					
					outputString.append(URLEncoder.encode("editInfo", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(editInfo.toString(), "UTF-8"));
					outputString.append("&");
					
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					
					
					
					
					
					
					
					
					
					System.out.println("outputString=" + outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					
					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
					httpString = new FileWriter(httpStringCSV2);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();
					
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "PrPowerWorld has finished evaluating!");
						editStack.clear(); // delete all items in editStack
						
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try evaluating PrPowerWorld again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    PWPrButton.setEnabled(true);
    PWPrButton.setVisible(true);
    PWPrButton.setSize(190,30);
    PWPrButton.setLocation(690, 10);

 
 
	  
	  
	  
	  
	  
	  
	  
    
    
    
    JButton refreshButton = new JButton("Refresh Map");
    refreshButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    		graphicsLayer.removeAll();
    		layers.remove(graphicsLayer);
    		
    	}
    });
    refreshButton.setEnabled(true);
    refreshButton.setVisible(true);
    refreshButton.setSize(130,30);
    refreshButton.setLocation(1090, 10);
    
    JButton LoadFromOWL = new JButton("Load Features from OWL file");
    LoadFromOWL.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
			try {
				App.main(null);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
	    		
    	}
    });
    LoadFromOWL.setEnabled(true);
    LoadFromOWL.setVisible(true);
    LoadFromOWL.setSize(130,30);
    LoadFromOWL.setLocation(890, 10);
    
    
    graphicsLayer = new GraphicsLayer();
    graphicsLayer.setName("simple graphics");
    
    
  //button for query (15-04-2016))
    final JTextField querylayer = new JTextField();
    querylayer.setAlignmentX(Component.LEFT_ALIGNMENT);
    querylayer.setMaximumSize(new Dimension(220, 25));
    querylayer.setEditable(true);
    
    final JButton queryButton = new JButton("Query Features");
    queryButton.setAlignmentX(Component.CENTER_ALIGNMENT);
    queryButton.setEnabled(true);
    queryButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
    	    ArrayList<String[]> editStack2 = new ArrayList<String[]>();	
			HttpURLConnection urlCon;
			OutputStreamWriter out;
			InputStreamReader in;
			URL url;
			
			String QueryString = null;
			if(e.getActionCommand().equals ("Query Features"));{
				String graphicFID = " ";
			    String graphicOBJECTID =  " ";
			    String appCallFlag = " ";
				QueryString = querylayer.getText();
				String[] newFeature = new String[] {graphicFID, graphicOBJECTID, appCallFlag, QueryString}; 
				boolean addtoStack = true;			  		            		  
			  	  if (addtoStack) {		
			  		editStack2.add(newFeature);
			  	  }
			}
									
			try{
				url = new URL("http://172.25.182.41/PWServlet/");
//				url = new URL("http://172.25.182.41/OPARTServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);				
				out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
				
				StringBuilder layers = new StringBuilder();
				StringBuilder OBJECTIDs = new StringBuilder();
				StringBuilder appCallFlag = new StringBuilder();
				StringBuilder QueryT = new StringBuilder();
				for (String[] item : editStack2) { 
					layers.append(item[0]);
					layers.append(",");
		 			OBJECTIDs.append(item[1]); 
				    OBJECTIDs.append(",");
					appCallFlag.append(item[2]);
					appCallFlag.append(",");
					 
					QueryT.append(querylayer.getText());
				}
				
				StringBuilder outputString = new StringBuilder();
				outputString.append(URLEncoder.encode("layers", "UTF-8"));
				outputString.append("=");
				outputString.append(URLEncoder.encode(" ", "UTF-8"));
				outputString.append("&");
				outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
				outputString.append("=");
				outputString.append(URLEncoder.encode(" ", "UTF-8"));
				outputString.append("&");
				outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
				outputString.append("=");
				outputString.append(URLEncoder.encode("Query", "UTF-8"));
//				outputString.append(URLEncoder.encode("OPALRT", "UTF-8"));
				outputString.append("&");
				outputString.append (URLEncoder.encode("QueryT", "UTF-8"));
				outputString.append ("=");				
				outputString.append (URLEncoder.encode(QueryT.toString(), "UTF-8"));
				
				DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
				wr.writeBytes(outputString.toString());
				wr.flush();
				wr.close();
				
				if(urlCon.getResponseCode()==200){

					in = new InputStreamReader(urlCon.getInputStream());
					final BufferedReader br = new BufferedReader(in);
					String[] strTemp = null;
					strTemp = br.readLine().split("\"");																			
					br.close();
					
					planes = new MultiPoint();
			        PictureMarkerSymbol planeSymbol = new PictureMarkerSymbol("http://static.arcgis.com/images/Symbols/Basic/GreenShinyPin.png");
			        planeSymbol.setSize(50, 50);			         			         
			         
			        double[] x= new double[(strTemp.length-1)/2];
			        for(int i=0; i<(strTemp.length-1)/2; i++){
			        	 x[i] = Double.parseDouble(strTemp[2*i+1]);
			         }
			         
			         for (int k=0 ; k<x.length/2 ; k++){ 
			        	 planes.add(x[2*k],x[2*k+1]); 			          
			         }
			         
			         Graphic gPlanes = new Graphic(planes, planeSymbol);
			         graphicsLayer.addGraphic(gPlanes);
			          			            			            
					 JOptionPane.showMessageDialog(null, "Query has been successfully performed!" );
					//editStack.clear(); 
					
				}
				out.close();
			}catch (IOException equery){
				equery.printStackTrace();
			}
			layers.add(graphicsLayer);
			
		}
      
    });
    queryButton.setSize(130, 30);
    queryButton.setLocation(1090, 45);
    
    JButton PrAPPWButton = new JButton("Run Pr AP + PW");
    PrAPPWButton.setEnabled(true);
    PrAPPWButton.setVisible(true);
    PrAPPWButton.setSize(190,30);
    PrAPPWButton.setLocation(690, 80);
    
    PrAPPWButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					StringBuilder editInfo = new StringBuilder();
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
			 			OBJECTIDs.append(item[1]); // ZHOU CHANGED ITEM[2] TO ITEM[1]
					    OBJECTIDs.append(",");
						appCallFlag.append("PrAPPW");
						appCallFlag.append(",");
						editInfo.append(item[2]);
						editInfo.append(",");
						
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					
					outputString.append(URLEncoder.encode("editInfo", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(editInfo.toString(), "UTF-8"));
					outputString.append("&");
					
					
					
					
					
					
					
					
					
					
					
					
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "Parameterized AP & PW has finished running!");
						editStack.clear(); // delete all items in editStack
						CSVReader.readCSV("PrAPPW");
				
						
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Parameterised AP again");
					} 
					
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    

    
    // combine text, label and dropdown list into one panel for selecting layer to edit
    panel.setBackground(new Color(0, 0, 0, 180));
    panel.add(description);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(lblLayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(cbxLayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(lblLayer2);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(querylayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(description3);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
    
    // create legend
    JLegend legend = new JLegend(map);
    legend.setPreferredSize(new Dimension(250, 700));
    legend.setBorder(new LineBorder(new Color(205, 205, 255), 3));
    
    // initialize contentPane and add contents
    
    // ZHOU add new buttons
    contentPane = new JLayeredPane();
  
    contentPane.setLayout(new BorderLayout(0,0));
    contentPane.setVisible(true);
    contentPane.add(PWPrButton); 
    contentPane.add(PrAPPWButton); 
    contentPane.add(refreshButton);
     contentPane.add(panel);
     
     contentPane.add(LoadFromOWL);
    contentPane.add(queryButton);
    contentPane.add(map, BorderLayout.CENTER);
    contentPane.add(legend, BorderLayout.WEST);
    

    
    
  
  //adding the graph here
    //map = createMap();
   //contentPane.add(map);
    
    //only until here
	
    
    window.add(contentPane);
    
    // dispose map just before application window is closed.
    window.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent windowEvent) {
    	  if (!editStack.isEmpty()) { // check if ArcGIS edits have not been saved in PowerWorld .pwb file
        	  int reply = JOptionPane.showConfirmDialog(null, "ArcGIS edits may have not been saved in PowerWorld or AspenPlus. "
        	  		+ "Are you sure you want to close?", "Window Closing", JOptionPane.YES_NO_OPTION);
        	  if (reply == JOptionPane.YES_OPTION) {
    	        super.windowClosing(windowEvent);
    	        map.dispose();
    	        window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // confirm close window
        	  } else { // if reply is NO
        		  window.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // don't close window
        	  }
    	  }
      }
    });
    
  } 

  
  
  private JMap createMap() {

	    final JMap jMap = new JMap();
	        final InfoPopupOverlay popupOverlay = new InfoPopupOverlay();
	    jMap.addMapOverlay(popupOverlay);
	        // grab the ArcGISFeatureLayer when added to the map and associate it with the infopopup overlay
	    jMap.getLayers().addLayerListEventListener(new LayerListEventListenerAdapter() {
	            @Override
	      public void multipleLayersAdded(LayerEvent event) {
	        for (Layer layer : event.getChangedLayers().values()) {
	          if (layer instanceof ArcGISFeatureLayer) {
	            popupOverlay.addLayer(layer);
	          }
	          else if(layer instanceof GroupLayer) {
	            for(Layer groupedLayer: ((GroupLayer) layer).getLayers()) {
	              if(groupedLayer instanceof ArcGISFeatureLayer) {
	                popupOverlay.addLayer(groupedLayer);
	              }
	            }
	          }
	        }
	      }
	            @Override
	      public void layerAdded(LayerEvent event) {
	        Layer layer = event.getChangedLayer();
	        if (layer instanceof ArcGISFeatureLayer) {
	          popupOverlay.addLayer(layer);
	        } else if(layer instanceof GroupLayer) {
	            for(Layer groupedLayer: ((GroupLayer) layer).getLayers()) {
	              if(groupedLayer instanceof ArcGISFeatureLayer) {
	                popupOverlay.addLayer(groupedLayer);
	              }
	            }
	          }
	               }
	    });

	    // create and load the web map
	    WebMap webMap = null;
	    try {
	      webMap = WebMap.newInstance(MAP_ID, portal);
	      jMap.loadWebMap(webMap);
	      
	    } catch (Exception e) {
	      e.printStackTrace();
	    }

	    return jMap;
	  }
  
  /**
   * Starting point of this application.
   * @param args
 * @throws IOException 
   */
  public static void main(String[] args) throws IOException { // main function invoked
 
	  
	  EventQueue.invokeLater(new Runnable() {

      @Override
      public void run() {
        try {
          JParkSim application = new JParkSim(); // instance of this application
          application.window.setVisible(true);
          ArcGISRuntime.setClientID("aSg9q12qgnN4OQq2"); // license app
        } catch (Exception e) {
          e.printStackTrace();
        }
       
      }
    });
 
  }
  
  
 

 
  
  
  
   
  
}

 