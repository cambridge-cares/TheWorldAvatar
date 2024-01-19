package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.ArrayList;

import javax.swing.JOptionPane;

import com.esri.map.ArcGISFeatureLayer;
/***
 * 
 * @author Shaocong
 * PrBtnInfo: child of httpRequestBtnInfo
 * contains info to construct a function button and define its actionlistener
 *
 */
public class PrBtnInfo extends httpRequestBtnInfo {



	/***
	 * See motherclass for parameter list.
	 * @param completeLayerList   
	 * @param emptyEditMsg
	 * @param doneMsg
	 * @param errMsg
	 * @param appCallFlag
	 * @param editStack
	 */
	public PrBtnInfo(ArcGISFeatureLayer[] completeLayerList, String emptyEditMsg, String doneMsg, String errMsg,
			String appCallFlag, ArrayList<String[]> editStack) {
		super(completeLayerList, emptyEditMsg, doneMsg, errMsg, appCallFlag, editStack);
		// TODO Auto-generated constructor stub
	}

	/**
	 * Overrides preCall from mother class: httpRequestBtnInfo
	 * Call at the beginning of actionlistener.
	 * @param ActionEvent    event triggers the action listener
	 */
	@Override
	void preCall(ActionEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	/***
	  * Overrides callback from mother class: httpRequestBtnInfo
	  * Call when http request in the action listener succeeds
	 * @param HttpURLConnection    httpURLConnection 
 
	 */
	void callback(HttpURLConnection urlCon) throws IOException {
		JOptionPane.showMessageDialog(null, doneMsg);
		editStack.clear();		
	}

}
