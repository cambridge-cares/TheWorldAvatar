package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import cam.dev.zhouxiaochi.*;

import javax.swing.JOptionPane;

import org.json.JSONException;

import com.esri.core.io.UserCredentials;
import com.esri.map.ArcGISFeatureLayer;

/***********
 * ABSTRACT CLASS stores info necessary to create HTTPRequest BUTTON  
 * child class [QueryBtnInfo | PrBtnInfo]
 * */
public abstract class httpRequestBtnInfo implements BtnInfo{
	//	url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
	public static final String BaseStr =  "http://172.25.182.41/"; //URL OF SERVLET
	public static final String PwUrlStr =  "PWServlet_OWL/"; //URL OF SERVLET
	public static final String APWOWHRStr =  "APWOWHRServlet/"; //URL OF SERVLET
	public static final String APWWHRStr =  "APWWHRServlet/"; //URL OF SERVLET
	public static final String query =  "QUERYServlet/"; //URL OF SERVLET

	 
	    
    ArcGISFeatureLayer[] completeLayerList;
    ArcGISFeatureLayer[] activeLayerList;
    
    
    
    public String emptyEditMsg;
	public String doneMsg;
	public String errMsg;
	public String appCallFlagStr;
	public String urlStr = BaseStr;
	ArrayList<String[]> editStack;

	/***
	 * 
	 * Constructor
	 * @param completeLayerList     completeLayerList as in JParkSim class
	 * @param emptyEditMsg          message to show when no edits done    
	 * @param doneMsg               message to show when action done
	 * @param errMsg                message to show when error occurs
	 * @param appCallFlag           appCallFlag [Query|PrAPPW|PWPr]
	 * @param editStack
	 */
	public httpRequestBtnInfo(ArcGISFeatureLayer[] completeLayerList, String emptyEditMsg, String doneMsg,
			String errMsg, String appCallFlagStr, ArrayList<String[]> editStack) {
		super();
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
		
		/*ArcGISFeatureLayer R301 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/114?token=2VUSGcKBo69OQ74UCC3DqA9ZhUI7IIKCeMXv8PAEacxBmu4LIg49J127MlNipq2iNe5WMJM_reVU9KRWPAwd5AOS2yUaqvwTiH0ek1yiDnh9XwLHwDuDwMr2f7QBLKcBi35Z75wkokMUR14TKhPf0SDlA04PXAXjMTQlCB70PtO3aohnCchmst51fAxM5LRNGX2OjUYh3lz21a5hh3wAYrEZRzGidXCjNVKgGsFNQ4M.", user);
		ArcGISFeatureLayer R302 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/115?token=2VUSGcKBo69OQ74UCC3DqA9ZhUI7IIKCeMXv8PAEacxBmu4LIg49J127MlNipq2iNe5WMJM_reVU9KRWPAwd5AOS2yUaqvwTiH0ek1yiDnh9XwLHwDuDwMr2f7QBLKcBi35Z75wkokMUR14TKhPf0SDlA04PXAXjMTQlCB70PtO3aohnCchmst51fAxM5LRNGX2OjUYh3lz21a5hh3wAYrEZRzGidXCjNVKgGsFNQ4M.", user);
		ArcGISFeatureLayer T302 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/119?token=2VUSGcKBo69OQ74UCC3DqA9ZhUI7IIKCeMXv8PAEacxBmu4LIg49J127MlNipq2iNe5WMJM_reVU9KRWPAwd5AOS2yUaqvwTiH0ek1yiDnh9XwLHwDuDwMr2f7QBLKcBi35Z75wkokMUR14TKhPf0SDlA04PXAXjMTQlCB70PtO3aohnCchmst51fAxM5LRNGX2OjUYh3lz21a5hh3wAYrEZRzGidXCjNVKgGsFNQ4M.", user);
		ArcGISFeatureLayer T202 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/T-202", user);
		ArcGISFeatureLayer M108 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/M-108", user);
		ArcGISFeatureLayer M208 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/M-208", user);
		ArcGISFeatureLayer T102 = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST019/FeatureServer/T-102", user);
		
		activeLayerList =  new ArcGISFeatureLayer[7];
		activeLayerList[0]=R301;
		activeLayerList[1]=R302;
		activeLayerList[2]=T302;
		activeLayerList[3]=T202;
		activeLayerList[4]=M108;
		activeLayerList[5]=M208;
		activeLayerList[6]=T102;*/
		
		this.completeLayerList = completeLayerList;
		this.emptyEditMsg = emptyEditMsg;
		this.doneMsg = doneMsg;
		this.errMsg = errMsg;
		this.appCallFlagStr = appCallFlagStr;
		this.editStack = editStack;
		///determine servlet address by callflag
	if (appCallFlagStr.contentEquals("Query")){//if run Query
			urlStr+= query;
		}
		else{//all other function requires
			urlStr+=PwUrlStr;//route to PW servelet
		}
	}

	/***
	 * Call at the beginning of actionlistener to be created for the button
	 * @param actionEvent in Listener
	 */
	abstract void preCall(ActionEvent e);
	
	/**
	 * Call when HTTPRequest completes
	 * @param urlCon
	 * @throws IOException
	 */
	abstract void  callback(HttpURLConnection urlCon) throws IOException;
	
	/***
	 * Implement create listener function from interface BtnInfo
	 * Create a httprequest listener callback
	 */
	@Override
	public ActionListener createListener() {
		return new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				
				preCall(arg0);//call before http request
				HttpURLConnection urlCon;
				OutputStreamWriter out;
				URL url;
				try {
				//System.out.println("appCallFlag"+ appCallFlagStr);
					url = new URL(urlStr); // URL of servlet
					urlCon = (HttpURLConnection) url.openConnection();
					urlCon.setRequestMethod("POST");
					urlCon.setDoOutput(true);
					
					if (editStack.isEmpty()) {//empty edit records?
						//=>YES!=>Show empty edit msg
						JOptionPane.showMessageDialog(null, emptyEditMsg);
					} else {
						out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
						StringBuilder layers = new StringBuilder();
						StringBuilder OBJECTIDs = new StringBuilder();
						StringBuilder appCallFlag = new StringBuilder();
						StringBuilder editInfo = new StringBuilder();
						StringBuilder QueryT = new StringBuilder();

						System.out.println("---------->" + editStack.size());
						for (String[] item : editStack) { // extract info from editStack
							layers.append(item[0]);
							layers.append(",");
							OBJECTIDs.append(item[1]);
							OBJECTIDs.append(",");
							appCallFlag.append(appCallFlagStr);
							appCallFlag.append(",");
							editInfo.append(item[2]);
							editInfo.append(",");
						   if(item.length  > 3){
							   QueryT.append(item[3]);
						   }
						   
						 
													}
						StringBuilder outputString = new StringBuilder();
						// Encode query string in UTF-8
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
						
						System.out.println("-------->>>>>>" + editInfo.toString());
						
						outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
						outputString.append("=");
						outputString.append(URLEncoder.encode(QueryT.toString(), "UTF-8"));

						
						
						System.out.println("Look here ---- outputString=" + outputString);
						
						//append query content formed above to url
						DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
						wr.writeBytes(outputString.toString()); 
						wr.flush();
						wr.close();
						
						
						
						if (urlCon.getResponseCode()==200) {//Request success?
							//=>YES!==>Call callback function
							callback(urlCon);
							
							
							try {
								CSVReader.readCSV(appCallFlagStr);
							} catch (JSONException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
							
							
							
						} else {//NO! => Show err message
							JOptionPane.showMessageDialog(null, errMsg + urlCon.getResponseCode());
						}
						out.close();//close HTTP conenction
					}
				} catch (IOException e) {
					e.printStackTrace();
				}
				
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
			}
		};
		
	}

}
