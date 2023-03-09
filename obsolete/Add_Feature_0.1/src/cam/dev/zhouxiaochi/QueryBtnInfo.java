package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.JTextField;

import com.esri.core.geometry.MultiPoint;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.PictureMarkerSymbol;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.GraphicsLayer;

/****
 * 
 * @author Shaocong
 *Mother class: httpRequestBtnInfo
 *Defines actionListener of a query btn.
 */
public class QueryBtnInfo extends httpRequestBtnInfo{


    private GraphicsLayer graphicsLayer;
	private JTextField querylayer;
    
	
	/***
	 * 
	 * @param completeLayerList  See mother class.
	 * @param emptyEditMsg       See mother class.
	 * @param doneMsg            See mother class.
	 * @param errMsg              See mother class.
	 * @param appCallFlag          See mother class.
	 * @param editStack            See mother class.
	 * @param graphicsLayer        graphicsLayer in JParkSim to apply change to.
	 * @param querylayer           queryLayer in JParkSim to get info from
	 */
	public QueryBtnInfo(ArcGISFeatureLayer[] completeLayerList, String emptyEditMsg, String doneMsg, String errMsg,
			String appCallFlag, ArrayList<String[]> editStack, GraphicsLayer graphicsLayer, JTextField querylayer) {
		super(completeLayerList, emptyEditMsg, doneMsg, errMsg, appCallFlag, editStack);
		this.graphicsLayer = graphicsLayer;
		this.querylayer = querylayer;
		// TODO Auto-generated constructor stub
	}
	


	@Override
	/***
	 * Override from mother class.
	 * Call @ beginning of action listener to pack query info into editStack.
	 */
	void preCall(ActionEvent e) {
		String QueryString = null;
		if(e.getActionCommand().equals ("Query Features"));{
			String graphicFID = " ";
		    String graphicOBJECTID =  " ";
		    String appCallFlag = " ";
			QueryString = querylayer.getText();
			String[] newFeature = new String[] {graphicFID, graphicOBJECTID, null, QueryString}; 
			boolean addtoStack = true;			  		            		  
		  	  if (addtoStack) {		
		  		editStack.add(newFeature);
		  	  }
		}		
	}

	@Override
	/**
	 * Overide from mother class.
	 * Draw symbols for queried features.
	 */
	void callback(HttpURLConnection urlCon) throws IOException {
          
		JOptionPane.showMessageDialog(null, "Query has been successfully performed!" );
		InputStreamReader in = new InputStreamReader(urlCon.getInputStream());
		final BufferedReader br = new BufferedReader(in);
		String[] strTemp = null;
		strTemp = br.readLine().split("\"");																			
		br.close();
		MultiPoint planes = new MultiPoint();
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

 	
	
	
	
}
