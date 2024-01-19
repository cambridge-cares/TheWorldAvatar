package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.GraphicsLayer;
import com.esri.map.LayerList;

/***
 * 
 * @author Shaocong
 * Implements BtnInfo Interface.
 */
public class RefreshBtnInfo implements BtnInfo {
    ArcGISFeatureLayer[] completeLayerList;
	public LayerList layers;
     public  GraphicsLayer graphicsLayer;
	


     /***
      * Pass necessary info from JParkSim to create listener.
      * @param layers             layerList of JParkSim
      * @param graphicsLayer      graphicsLayer to apply change to
      * @param completeLayerList  
      */
	public RefreshBtnInfo( LayerList layers, GraphicsLayer graphicsLayer,
			ArcGISFeatureLayer[] completeLayerList) {
		this.layers = layers;
		this.graphicsLayer = graphicsLayer;
		this.completeLayerList = completeLayerList;
	}



	/***
	 * Overrided from BtnInfo Interface.
	 * Creates Listener for refresh btn.
	 */
	public  ActionListener createListener(){
		return new ActionListener() {
	    	@Override
	    	public void actionPerformed(ActionEvent arg0) {
	    		JParkSim.completeLayerList[17].requery();
				JParkSim.completeLayerList[17].refresh();
				JParkSim.completeLayerList[12].requery();
				JParkSim.completeLayerList[12].refresh();
				JParkSim.completeLayerList[13].requery();
				JParkSim.completeLayerList[13].refresh();
	    		/*for (ArcGISFeatureLayer layer : completeLayerList) {
	    			layer.requery();
	    			layer.refresh();
	    		}*/
	    		graphicsLayer.removeAll();
	    		layers.remove(graphicsLayer);
	    		
	    	}
	    };
		
	};

}
