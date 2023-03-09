package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import com.esri.map.GraphicsLayer;
import com.esri.map.LayerList;
/**
 * @author Shaocong
 *Interface for all BtnInfo.
 *Contains a abstract function createListener to create the corresponding actionlistener for the button.
 *Child :[httpRequestBtnInfo | refreshBtnInfo | LoadOwlBtnInfo]
 */
public interface BtnInfo{
	
	//public String emptyEditMsg;
	//public String doneMsg;
	//public String errMsg;
	//public String outFileLocation;
	//public String appCallFlag;
	//public LayerList layers;
//	public  GraphicsLayer graphicsLayer;
	



	/****
	 * Creates an ActionListener for the button.
	 * @return ActionListener.
	 */
	public abstract ActionListener createListener();
	
	
}