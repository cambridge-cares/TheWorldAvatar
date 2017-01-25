package cam.dev.zhouxiaochi;
import com.esri.runtime.ArcGISRuntime;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.CoordinateConversion.UTMConversionMode;
import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.Geometry;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.geometry.Polyline;
import com.esri.core.geometry.SpatialReference;
import com.esri.core.geometry.Transformation2D;
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
import com.esri.map.QueryMode
;
/***
 * /**A* for line Arrangement :draw connections on map.
 * Deprecated. Kept only for reference.
 * @author Shaocong
 *
 */
public class Draw_connections {

	public static ArrayList<Rectangle> obstacles = new ArrayList<Rectangle>();
	public static ArrayList<Connection> connections = new ArrayList<Connection>();
	public static ArrayList<Connection> temp = new ArrayList<Connection>();
	
	public static ArrayList<Graphic> graphics = new ArrayList<Graphic>();
	
 
		 
	
	public static Graphic[]  createConnections()
	{
		
	 	  SimpleLineSymbol outline = new SimpleLineSymbol(new Color(33, 150, 0), 300);
		  SimpleFillSymbol symbol2 = new SimpleFillSymbol(new Color(0, 240, 0, 180), outline);

		  
		  
		 if((connections.size() > 0) && (obstacles.size() > 0))
		 {
 
		
 
		aStarLineGenerator astar = new aStarLineGenerator(40,80,200	,200,connections, obstacles); 
		for(int idx = 0; idx < connections.size();idx++){
			
			
			
			
		ArrayList<Point> Line = astar.getLineAt(idx);
		
		if(astar.getLineAt(idx)!=null)
		{
		Polyline line = new Polyline();
		for(int i = 0 ; i < Line.size() ; i++){
			 double x = Line.get(i).getX() + FeatureWriter.p_left_button.getX();
			 double y = Line.get(i).getY() + FeatureWriter.p_left_button.getY();
			if(i == 0)
			{
				line.startPath(x,y);
			}
			else
			{
				line.lineTo(x,y);
				  
			}
			 
		}
 
		
        double angleRad = Math.toRadians(-45);
        Transformation2D rotateTx = new Transformation2D();
        rotateTx.rotate(Math.cos(angleRad), Math.sin(angleRad), FeatureWriter.p_left_button);
        line.applyTransformation(rotateTx);
   	 
		
		
		   Graphic  connection = new Graphic(line,symbol2, null);
		   graphics.add(connection);
		}
		}
		
		Graphic[] adds = new Graphic[graphics.size()];
	
		
		
		for(int i = 0; i < graphics.size(); i++)
		{
			adds[i] = graphics.get(i);
			
		}
		
		return adds;
		 }
		 else
		 {
			 System.out.println("NO!  one of the shit is null");
		 }
		return null;
		
	}
	
	
		
}
	 
 

