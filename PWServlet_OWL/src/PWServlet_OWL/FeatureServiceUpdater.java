package PWServlet_OWL;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/***
 * Update Arcgis service through rest API.
 * Check Arcgis Rest API DOC for more info: http://resources.arcgis.com/en/help/arcgis-rest-api/#/The_ArcGIS_REST_API/02r300000054000000/
 * Provide tiers of constructed functions to use rest API, higher tiers utilities lower tier functions.
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * First tier: httpRequest [sent raw HTTP request, base of every function]
 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Second tier : DoSth2Feature, DoSth2Fields, DoSth2Service.
 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Third tier: deleteFeaturesFromTable, deleteAllFeaturesInTable, addFields2Table, deleteFieldsFromTable, deleteLayers, deleteAllLayers, generateLayer, isLayerExist, areLayerExist
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * utility functions: initParameters, generateToken
 * 
 * @author Shaocong
 *
 */
public class FeatureServiceUpdater {

	private final static String[] fieldAttrs = { "name", "type", "alias", "sqlType", "nullable", "editable", "domain",
			"defaultValue", "length" };// All attributes of a field definition

	private static String token;// security token to be acquired

	/******** final strings ************/
	private final static String addDefinitionInputName = "addToDefinition";
	private final static String deleteDefinitionInputName = "deleteFromDefinition";
	private final static String updateDefinitionInputName = "updateDefinition";

	private String baseURL;//base url of service to send http request to
	public static String layerID; //TO be used in APP class, layerID of the most recently generated layer

	/***********constructor**********************/
	public FeatureServiceUpdater(String baseURL) {
		super();
		this.baseURL = baseURL;
	}

	/***********main function for testing only**********************/

	  public static void main(String[] args) {
	 
	  
	  //TODO: test each function 
		  String baseUrl =
	  "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST020/FeatureServer";
	  FeatureServiceUpdater mUpdater = new FeatureServiceUpdater("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST015/FeatureServer");
	  
	  
	  
	  
		  String templateServiceURL =
	  "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/Load_points/FeatureServer";
	  
	  //Map<String String[]> attrValueListsMap = new Map<String String[]> ();
	  
	  //String[] nameList = {"blower"};
	  
	 // Map attris = new HashMap<String, String>();
	  //attris.put("Long","1");
	  //attris.put("Lat","1");

		  int[] idList = new int[25];
		  for(int i = 0 ; i < idList.length; i++){
			  
			  idList[i] = i;
		  }
	  //Map layerlist = new HashMap<Integer, Map<String, String>>();
	  //layerlist.put(0, attris);
	  try {
		mUpdater.deleteAllLayers();
	} catch (JSONException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} 
	 // String [] names = {"blower", "cdd", "blower2"}; boolean[]
	  //results = mUpdater.areLayersExist(names);
	  
	  //System.out.println(results.toString()); } catch (JSONException e) { //
	  //TODO Auto-generated 
	   
	  
	 }
	  
	

	/***
	 * init prameters to be sent through request to server
	 * (most usual case) f(format) - json
	 * 
	 * @return initial parameters
	 */
	public List<NameValuePair> initParameters() {
		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));

		return parameters;
	}

	/**************
	 * send a http request to retreive token from web token service, needed for accessing admin action
	 * 
	 * @return String: token
	 * @throws JSONException
	 */
	private String generateToken() throws JSONException {
		String mURL = "https://www.arcgis.com/sharing/generateToken";//URL for token generating service
		// "https://www.arcgis.com/sharing/rest/generateToken
		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));//response data format - json
		parameters.add(new BasicNameValuePair("username", "kleinelanghorstmj"));//username

		parameters.add(new BasicNameValuePair("password", "h3OBhT0gR4u2k22XZjQltp"));//password
		parameters.add(new BasicNameValuePair("client", "referer"));//client type, check API DOC for more info
		parameters.add(new BasicNameValuePair("referer", "http://www.arcgis.com"));

		return httpRequest(mURL, "application/x-www-form-urlencoded; charset=utf-8", parameters).getString("token");//get response as jsonobject, extract token string
	}

	/****
	 * Delete, Add, Query features or request for the layer info
	 * 
	 * @param option
	 *            [delete|add|query|info|serviceinfo|edits], action to be taken to the features in
	 *            the layer
	 * @param layerInd
	 *            index of layer to be taken action of
	 * @param parameters
	 *            parameters to be carried in the request, see the complete list
	 *            of possible parameters on Arcgis REST API DOC
	 * @return
	 * @throws JSONException
	 */
	public JSONObject doSth2Feature(String option, int layerInd, List<NameValuePair> parameters) throws JSONException {
		//lazy initiation: token only generated once 
		if (token == null) {//=>have a token already?
			//=>NO!=>Then generate one!
			token = generateToken();
			System.out.println("token acquired: " + token);
		}

		// String baseURL =
		// "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/test003/FeatureServer";
		/***URL segments String****/
		String deleteFeatureURL = "/deleteFeatures";
		String queryFeatureURL = "/query";
		String addFeatureURL = "/addFeatures";
		String layerURL = "/" + layerInd;
		String updateURL = "/updateFeatures";
        String applyEditURL = "/applyEdits";
		String mURL = baseURL + layerURL;
      /**************************/
		
		//switch on user option to construct url to send request to
		switch (option.toLowerCase()) {
		case "delete":
			mURL += deleteFeatureURL;
			break;
		case "query":
			mURL += queryFeatureURL;
			break;
		case "add":
			mURL += addFeatureURL;
			break;
		case "info"://info of layer
			break;
		case "serviceinfo"://info of full service
			mURL = baseURL;
			break;
		case "update"://update layer
			mURL  += updateURL;

		case "edits":	//update|Add|Delete from service level, check API DOC for more info
			mURL = baseURL + applyEditURL; 
			break;
		default:
			return null;
		}
		parameters.add(new BasicNameValuePair("token", token));
		return httpRequest(mURL, "application/x-www-form-urlencoded; charset=utf-8", parameters);

	}

	//TODO: TEST This before adding it to CSVReader
	
	
	/****
	 * 
	 * @param entityNameList   a list of names of all the entity to be updated
	 * @param nameValueListofAllLayers   Map<Integer, Map<String, String>>, a map of idx(corresponding to idx in name array) to its parameter list
	 * @return boolean, true if action succeed
	 * @throws JSONException
	 */
	public boolean updateFeaturesInTable(String[] entityNameList, Map<Integer, Map<String, String>> nameValueListofAllLayers, int objectID) throws JSONException{
		List<NameValuePair> parameters = initParameters();
		int[] idList = new int[entityNameList.length];
		if(entityNameList.length != nameValueListofAllLayers.size()){///Do length of namelist match numbers of parameter list?
			//=>NO!=>Print err and exit
			System.out.println("ERR: namelist of entity does not have same length as parameter list");
			return false;
					}
		
		/***************query service with name list to get id list of the layers to be updated****************************/
	     ///query server for service info
          JSONObject returned = doSth2Feature("serviceInfo", 2, parameters);
         
          //get responce?
         if(returned!= null ){//=>YES!=>Process it!
             //System.out.println(returned.toString());

        	 JSONArray layers = returned.getJSONArray("layers");//extract JSONArray layers
        	 
        	 for(int i = 0 ; i < layers.length(); i ++){//for each JSONObject layer
        		 JSONObject layer = (JSONObject) layers.get(i);
        		 for(int j = 0 ; j < entityNameList.length; j ++){//filter through my list of entities to update
        			 //TODO: Note that this works bcs now we have each layer only one entity & each entity name as layer name
        			 //TODO: Might need to sifle through features in list in future
        		 if (layer.getString("name").equals(entityNameList[j])){//Does any layer name matches name in my update list?
        			 idList[j] = layer.getInt("id");//=>YES! => Record down the id, put in id array same place as before in name array 
        		// System.out.println("Layer id"+ j +" value:"+idList[j]);    
        		 }
        		   }
        		 }
		
		
        	 /****************construct json to sent to service from acquired layer id list**************/
		JSONArray updateLayers = new JSONArray();
		
		Iterator  it = nameValueListofAllLayers.entrySet().iterator();//get iterator of map of parameter list
       while (it.hasNext()) {//iterate through the map
           Map.Entry pair = (Map.Entry)it.next();
           //System.out.println(pair.getKey() + " = " + pair.getValue());
           Map<String, String> parameters2Update = (Map<String, String>) pair.getValue();//get paramter list for each layer(entity)
           int idxEntity = (int) pair.getKey();//get idx in the name list for each layer(entity)
  		// System.out.println("Entity id in namelist saved in map"+ idxEntity);    

           JSONObject updateLayer = new JSONObject();
           updateLayer.put("id", idList[idxEntity]);
           JSONArray  updateArr = new JSONArray();
           JSONObject updateFeature = new JSONObject();
           JSONObject attributes = new JSONObject();
           attributes.put("OBJECTID", objectID);
           for (Map.Entry<String, String> entry : parameters2Update.entrySet()) {
        	    attributes.put(entry.getKey(), entry.getValue()); //put each name value pair to attributes json object
         		 //System.out.println(entry.getKey()+":  "+ entry.getValue());    
        	}
           
           updateFeature.put("attributes", attributes);
           updateArr.put(updateFeature);
           updateLayer.put("updates", updateArr);
           updateLayers.put(updateLayer);
           it.remove(); // avoids a ConcurrentModificationException
       }
		parameters.add(new BasicNameValuePair("edits", updateLayers.toString()));//put query json into paramters
		//System.out.println("sent json"+updateLayers.toString());
		
		  JSONObject result = doSth2Feature("edits", 1, parameters);//get result

			return true;		
	}
         //=>No reponse when querying ids?==?print out err and exit
         System.out.println("Err, no response JSON empty");
		return false;
}

	/***
	 * Delete ALL Features in a table
	 * 
	 * @param layerInd
	 *            index of a layer
	 * @return success flag: bool
	 * @throws JSONException
	 */
	public boolean deleteAllFeaturesInTable(int layerInd) throws JSONException {

		List<NameValuePair> parameters = initParameters();
		parameters.add(new BasicNameValuePair("where", "OBJECTID > -1"));

		JSONArray result = doSth2Feature("delete", layerInd, parameters).getJSONArray("deleteResults");
		if (result == null || result.length() == 0) {
			return false;
		}
		return true;
	}

	/****
	 * sample feature { "geometry" : {"x" : -118.15, "y" : 33.80}, "attributes"
	 * : { "OWNER" : "Joe Smith", "VALUE" : 94820.37, "APPROVED" : true,
	 * "LASTUPDATE" : 1227663551096 } }
	 * 
	 * @param features
	 *            to add structed in JSONObject
	 * @return boolean if it is success
	 * @throws JSONException
	 */
	// TODO:TEST!!!!!
	public boolean addFeatures2Table(int layerId, JSONArray featureList) throws JSONException {

		List<NameValuePair> parameters = initParameters();
		parameters.add(new BasicNameValuePair("features", featureList.toString()));

		JSONArray result = doSth2Feature("add", layerId, parameters).getJSONArray("addResults");
		if (result == null || result.length() == 0) {
			return false;
		}
		return true;
	}

	/**Delete features in arcgis service with a integer array of feature's object id
	 * 
	 * @param features
	 *            to delete as name listed in an array
	 * @return boolean if it is success
	 * @throws JSONException
	 */

	public boolean deleteFeaturesFromTable(int layerId, int[] idArr) throws JSONException {

		List<NameValuePair> parameters = initParameters();

		StringBuilder strB = new StringBuilder();

		int i;
		for (i = 0; i < idArr.length - 1; i++) {
			strB.append(idArr.toString()).append(", ");

		}

		strB.append(idArr[i]);

		parameters.add(new BasicNameValuePair("objectIds", strB.toString()));

		JSONArray result = doSth2Feature("delete", layerId, parameters).getJSONArray("deleteResults");
		if (result == null || result.length() == 0) {
			return false;
		}

		return true;
	}

/**
 * Delete features in arcgis service with a string array of feature name
 * @param layerId
 * @param nameArr    array of names of features to be deleted
 * @return boolean, if action is success
 * @throws JSONException
 */
	public boolean deleteFeaturesFromTable(int layerId, String[] nameArr) throws JSONException {

		List<NameValuePair> parameters = initParameters();

		StringBuilder strB = new StringBuilder();

		int i;
		for (i = 0; i < nameArr.length - 1; i++) {
			strB.append("name = '").append(nameArr).append("' ").append("OR");

		}

		strB.append("name = '").append(nameArr).append("' ");
		parameters.add(new BasicNameValuePair("where", strB.toString()));
		parameters.add(new BasicNameValuePair("returnIdsOnly", "true"));

		JSONObject result = doSth2Feature("info", layerId, parameters);
		if (result == null || result.length() == 0) {
			// todo: STANDARDLIZE ERR MSG
			System.out.println("ERR: CAN NOT FIND THE FEATURES IN DATABASE");

			return false;
		}
		int[] idList = new int[nameArr.length];

		JSONArray idJArr = result.getJSONArray("objectIds");

		for (int j = 0; j < idJArr.length(); j++) {
			idList[j] = idJArr.getInt(j);
		}

		return deleteFeaturesFromTable(layerId, idList);
	}

/***
 * Add new fields to service table with a json array that includes fields info
 * See arcgis rest API for reference to format of the required json array
 * @param layerId
 * @param FieldList    an constructed json array of fields to be add
 * @return boolean success or not
 * @throws JSONException
 */
	public boolean addFields2Table(int layerId, JSONArray FieldList) throws JSONException {
		List<NameValuePair> parameters = initParameters();
		JSONObject wrapper = new JSONObject();
		wrapper.put("fields", FieldList);

		parameters.add(new BasicNameValuePair(addDefinitionInputName, wrapper.toString()));

		return doSth2Fields("add", layerId, parameters).getBoolean("success");
	}

	
	/***
	 * Delete existing fields from service table with a json array that includes field info
	 * @param layerId
	 * @param FieldList
	 * @return
	 * @throws JSONException
	 */
	public boolean deleteFieldsFromTable(int layerId, JSONArray FieldList) throws JSONException {

		List<NameValuePair> parameters = initParameters();
		JSONObject wrapper = new JSONObject();
		wrapper.put("fields", FieldList);

		parameters.add(new BasicNameValuePair(deleteDefinitionInputName, wrapper.toString()));

		return doSth2Fields("delete", layerId, parameters).getBoolean("success");
	}


	public boolean deleteFieldsFromTable(int layerId, String[] nameList) throws JSONException {
		if (nameList.length == 0 || nameList == null) {
			System.out.println("ERR: namelist is not a proper array or is empty");
			return false;
		}

		JSONArray fieldList = new JSONArray();
		for (String name : nameList) {

			JSONObject temp = new JSONObject();
			temp.put("name", name);
			fieldList.put(temp);
		}
		return deleteFieldsFromTable(layerId, fieldList);
	}

	/*****
	 * sent an httpRequest via POST method
	 * 
	 * @param url
	 *            url the request to be sent to
	 * @param contentType
	 *            content type to be sent
	 * @param parameters
	 *            parameters to be carried in the request
	 * @return
	 */
	private JSONObject httpRequest(String url, String contentType, List<NameValuePair> parameters) {
		// "application/x-www-form-urlencoded; charset=utf-8"
		HttpClient httpClient = new DefaultHttpClient();
		HttpResponse response = null;

		try {

			System.out.println("_____________________________________________");
			System.out.println("Start POST request");

			HttpPost request = new HttpPost(url);
			// System.out.println("input URL:"+url);

			// StringEntity params = new StringEntity(testFeatureJSON);
			// request.setEntity(params);

			///////////////////// set
			///////////////////// request///////////////////////////////////////////
			request.addHeader("content-type", contentType);
			if (parameters != null)
				request.setEntity(new UrlEncodedFormEntity(parameters, "UTF-8"));
			//System.out.println(parameters.toString());

			//System.out.println(request.getEntity().toString());

			// debug:request header
			System.out.println("Request URL: " + request.getURI());
			System.out.println("Request Header:");

			for (Header h : request.getAllHeaders())
				//System.out.println(h.toString());

			/////// debug:print out response

			response = httpClient.execute(request);
			if (response != null) { // handle response

				/////// debug: response header/////////////
				System.out.println("Response header:");
				for (Header h : response.getAllHeaders())
					System.out.println(h.toString());
				////////// process response//////////////
				InputStream in = response.getEntity().getContent(); // Get the
																	// data in
																	// the
																	// entity
				BufferedReader rd = new BufferedReader(new InputStreamReader(in));
				StringBuilder result = new StringBuilder(); // or StringBuffer
															// if Java version
															// 5+
				String line;

				while ((line = rd.readLine()) != null) {
					result.append(line);
					result.append('\r');
				}
				rd.close();
				//System.out.println("response JSON:");

				//System.out.println(result.toString());
				JSONObject obj = new JSONObject(result.toString());
				return obj;

			}

		} catch (Exception ex) {
			// handle exception here
			System.out.println("err in http request");
			return null;

		} finally {

		}
		return null;

	}

	/***
	 * 
	 * @param option
	 *            option [delete|add|query|info], action to be taken to the
	 *            attributes in the layer
	 * @param layerInd
	 *            index of layer to be taken action of
	 * @param parameters
	 *            parameters to be carried in the request, see the complete list
	 *            of possible parameters on Arcgis REST API DOC
	 * @return
	 * @throws JSONException
	 */
	public JSONObject doSth2Fields(String option, int layerInd, List<NameValuePair> parameters) throws JSONException {
		if (token == null) {
			token = generateToken();
			//System.out.println("token acquired: " + token);
		}

		if (!baseURL.contains("rest/")) {
			System.out.println("Error: BaseURL does not contain rest.");
			return null;
		}
		String[] parts = baseURL.split("(?<=rest/)");

		String mURL = parts[0] + "admin/" + parts[1];
		//System.out.println("format admin url: " + mURL);

		String addDefURL = "/addToDefinition";
		String updateDefURL = "/updateDefinition";
		String deleteDefURL = "/deleteFromDefinition";
		String layerURL = "/" + layerInd;
		mURL += layerURL;

		switch (option.toLowerCase()) {
		case "delete":
			mURL += deleteDefURL;
			break;
		case "update":
			mURL += updateDefURL;
			break;
		case "add":
			mURL += addDefURL;
			break;
		case "info":
			break;

		default:
			System.out.println("Err: option of action is not allowed in call dosth2Field");
			return null;
		}

		parameters.add(new BasicNameValuePair("token", token));
		JSONObject result = httpRequest(mURL, "application/x-www-form-urlencoded; charset=utf-8", parameters);

		return result;
	}

	/***
	 * 
	 * @param option
	 *            option [delete|add|query|info], action to be taken to the
	 *            service
	 * @param parameters
	 *            parameters to be carried in the request, see the complete list
	 *            of possible parameters on Arcgis REST API DOC
	 * @return
	 * @throws JSONException
	 */
	public JSONObject doSth2Service(String option, List<NameValuePair> parameters) throws JSONException {

		if (token == null) {
			token = generateToken();
			//System.out.println("token acquired: " + token);
			parameters.add(new BasicNameValuePair("token", token));

		}

		if (!baseURL.contains("rest/")) {
			System.out.println("Error: BaseURL does not contain rest.");
			return null;
		}
		String[] parts = baseURL.split("(?<=rest/)");

		String mURL = parts[0] + "admin/" + parts[1];
		//System.out.println("format admin url: " + mURL);
		// String baseURL =
		// "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/test003/FeatureServer";
		String addDefURL = "/addToDefinition";
		String updateDefURL = "/updateDefinition";
		String deleteDefURL = "/deleteFromDefinition";
		switch (option.toLowerCase()) {
		case "delete":
			mURL += deleteDefURL;
			break;
		case "update":
			mURL += updateDefURL;
			break;
		case "add":
			mURL += addDefURL;
			break;
		case "info":
			break;

		default:
			System.out.println("Err: option of action is not allowed in call dosth2Field");
			return null;
		}

		JSONObject result = httpRequest(mURL, "application/x-www-form-urlencoded; charset=utf-8", parameters);

		return result;
	}

	// TODO:TEST
	public boolean deleteLayers(int[] idList) throws JSONException {

		List<NameValuePair> parameters = initParameters();
		JSONObject wrapper = new JSONObject();
		JSONArray layerArr = new JSONArray();
		for (int layerId : idList) {

			JSONObject temp = new JSONObject();
			temp.put("id", layerId + "");
			layerArr.put(temp);
		}

		wrapper.put("layers", layerArr);

		parameters.add(new BasicNameValuePair(deleteDefinitionInputName, wrapper.toString()));

		return doSth2Service("delete", parameters).getBoolean("success");

	}

	// TODO:TEST
	public boolean deleteLayers(String[] nameList) throws JSONException {

		List<NameValuePair> parameters = initParameters();
		JSONObject wrapper = new JSONObject();
		JSONArray layerArr = new JSONArray();
		for (String layerName : nameList) {

			JSONObject temp = new JSONObject();
			temp.put("name", layerName);
			layerArr.put(temp);
		}

		wrapper.put("layers", layerArr);

		parameters.add(new BasicNameValuePair(deleteDefinitionInputName, wrapper.toString()));

		return doSth2Service("delete", parameters).getBoolean("success");

	}

	public boolean deleteAllLayers() throws JSONException {

		if (token == null) {
			token = generateToken();
			System.out.println("token acquired: " + token);
		}
		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("token", token));

		JSONArray thisLayers = doSth2Service("info", parameters).getJSONArray("layers");

		JSONArray idList = new JSONArray();
		for (int i = 0; i < thisLayers.length(); i++) {

			int id = thisLayers.getJSONObject(i).getInt("id");
			JSONObject tmp = new JSONObject();
			tmp.put("id", id + "");
			idList.put(tmp);
		}

		JSONObject wrapper = new JSONObject();

		wrapper.put("layers", idList);
		parameters.add(new BasicNameValuePair(deleteDefinitionInputName, wrapper.toString()));

		JSONObject result = doSth2Service("delete", parameters);
		return (result.has("success"));
	}

	public boolean isLayerExist(String layerName) throws JSONException {
		List<NameValuePair> parameters = initParameters();

		// parameters.add(new BasicNameValuePair("layerDefs","0:name =
		// '"+layerName+"'"));
		// parameters.add(new BasicNameValuePair("returnCountOnly","true"));

		JSONObject result = doSth2Feature("serviceInfo", 2, parameters);

		if (result != null) {
			System.out.println(result.toString());

			JSONArray layers = result.getJSONArray("layers");

			for (int i = 0; i < layers.length(); i++) {
				JSONObject layer = (JSONObject) layers.get(i);
				if (layer.getString("name").equals(layerName)) {
					return true;
				}
			}

		}

		return false;
	}

	public boolean[] areLayersExist(String[] layerNames) throws JSONException {
		List<NameValuePair> parameters = initParameters();

		// parameters.add(new BasicNameValuePair("layerDefs","0:name =
		// '"+layerName+"'"));
		// parameters.add(new BasicNameValuePair("returnCountOnly","true"));
		boolean[] results = new boolean[layerNames.length];
		Arrays.fill(results, false);

		JSONObject returned = doSth2Feature("serviceInfo", 2, parameters);

		if (returned != null) {
			System.out.println(returned.toString());

			JSONArray layers = returned.getJSONArray("layers");

			for (int i = 0; i < layers.length(); i++) {
				JSONObject layer = (JSONObject) layers.get(i);
				for (int j = 0; j < layerNames.length; j++) {
					if (layer.getString("name").equals(layerNames[j])) {
						results[j] = true;
					}
				}
			}

		}
		return results;
	}

	/*****
	 * generate a new layer from certain template, now is test004
	 * 
	 * @param numOfAttrs
	 *            number of input lists of attribute values. All other
	 *            attributes without a input list will be default value.
	 * @param attrValueListsMap:Map<String,
	 *            String[]> a name map of all the input attribute values.
	 * @throws JSONException
	 */
	
	public enum LayerType{
		POLYLINE,
		POLYGON,
		POINT
	}
	
	private  JSONObject PolygonTemplateMother = null;
	private  JSONObject PolylineTemplateMother = null;
	private  JSONObject PointTemplateMother = null;
	String PolygonTemplateServiceURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/test004/FeatureServer";
	String PolylineTemplateServiceURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/airline/FeatureServer";
	String PointTemplateServiceURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/Point/FeatureServer";
	public void generateLayer(int numOfAttrs, LayerType layertype, Map<String, String[]> attrValueListsMap,
			String newLayerName) throws JSONException {

		//System.out.println("Generating Layer with name: "+newLayerName);
		JSONObject mTemplateJSON;
		if (token == null) {
			token = generateToken();
			System.out.println("token acquired: " + token);
		}
		// check if every value string is of required length
		Iterator it = attrValueListsMap.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry pair = (Map.Entry) it.next();

			if (((String[]) pair.getValue()).length != numOfAttrs) {
				//System.out.println("Array for attributes are not of required length: " + numOfAttrs);
				return;
			}

		}

		


		//// TODO:now get layer template from test004, in the future the
		//// template url will be a parameter instead

		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("token", token));

		if(PolygonTemplateMother == null) {
			System.out.println("start request for Polygon template");

			PolygonTemplateMother = httpRequest(PolygonTemplateServiceURL, "application/x-www-form-urlencoded; charset=utf-8",
				parameters).getJSONArray("layers").getJSONObject(0);

		}
		
		if(PolylineTemplateMother == null) {
			System.out.println("start request for Polyline template");

			PolylineTemplateMother = httpRequest(PolylineTemplateServiceURL, "application/x-www-form-urlencoded; charset=utf-8",
				parameters).getJSONArray("layers").getJSONObject(0);

		}
		
		if(PointTemplateMother == null) {
			System.out.println("start request for Point template");

			PointTemplateMother = httpRequest(PointTemplateServiceURL, "application/x-www-form-urlencoded; charset=utf-8",
				parameters).getJSONArray("layers").getJSONObject(0);

		}
		
		
		
		if(layertype == LayerType.POLYGON){
			mTemplateJSON = new JSONObject(PolygonTemplateMother.toString());
		}else if(layertype == LayerType.POLYLINE){
			mTemplateJSON = new JSONObject(PolylineTemplateMother.toString());;

		}else if(layertype == LayerType.POINT)
		{
			mTemplateJSON = new JSONObject(PointTemplateMother.toString());
		}
		else{
			System.out.println("ERR: REQUEST TO GENERATE LAYER WITH NON-EXISTING TYPE");
			return;
		}
		
		JSONArray thisLayers = doSth2Service("info", parameters).getJSONArray("layers");

		int curLayersNum = thisLayers.length();

		mTemplateJSON.put("id", curLayersNum);
		layerID = String.valueOf(curLayersNum);

		curLayersNum++;

		// templateLayer.put("name", newLayerName + curLayersNum);
		mTemplateJSON.put("name", newLayerName);

		mTemplateJSON.remove("adminLayerInfo");

		// String newTableName =
		// templateLayer.getJSONObject("adminLayerInfo").getString("tableName")+curLayersNum;

		// templateLayer.getJSONObject("adminLayerInfo").put("tableName",
		// newTableName);
		mTemplateJSON.remove("fields");
		JSONArray FieldArray = new JSONArray();

		// construct globalID and objectID definition to JSON
		JSONObject globalID = new JSONObject();
		globalID.put("name", "GlobalID");
		globalID.put("alias", "GlobalID");
		globalID.put("type", "esriFieldTypeGlobalID");
		globalID.put("SQL Type", "sqlTypeOther");
		globalID.put("length", 38);
		globalID.put("nullable", false);
		globalID.put("editable", false);
		globalID.put("domain", (Object) null);

		globalID.put("defaultValue", (Object) null);
		FieldArray.put(globalID);

		JSONObject objectID = new JSONObject();
		globalID.put("name", "OBJECTID");
		globalID.put("alias", "OBJECTID");
		globalID.put("type", "esriFieldTypeOID");
		globalID.put("SQL Type", "sqlTypeOther");
		globalID.put("length", 38);
		globalID.put("nullable", false);
		globalID.put("editable", false);
		globalID.put("domain", (Object) null);

		globalID.put("defaultValue", (Object) null);
		FieldArray.put(objectID);

		JSONObject[] newFields = new JSONObject[numOfAttrs];

		// {"name","type","alias","sqlType","nullable","editable","domain","defaultValue","length"};
		// populate JSONObjects with default values
		for (int i = 0; i < numOfAttrs; i++) {
			newFields[i] = new JSONObject();
			newFields[i].put(fieldAttrs[0], "");
			newFields[i].put(fieldAttrs[1], "");

			newFields[i].put(fieldAttrs[2], "");

			newFields[i].put(fieldAttrs[3], "sqlTypeOther");

			newFields[i].put(fieldAttrs[4], true);

			newFields[i].put(fieldAttrs[5], true);
			newFields[i].put(fieldAttrs[6], (Object) null);

			newFields[i].put(fieldAttrs[7], (Object) null);
			newFields[i].put(fieldAttrs[8], 50);

		}

		// populate field definitions with input value arrays
		for (int attrIndex = 0; attrIndex < fieldAttrs.length; attrIndex++) {

			if (attrValueListsMap.containsKey(fieldAttrs[attrIndex])) {
				String[] valueStr = attrValueListsMap.get(fieldAttrs[attrIndex]);
				// add the value string to respective field definiton
				for (int i = 0; i < numOfAttrs; i++) {
					newFields[i].put(fieldAttrs[attrIndex], valueStr[i]);
				}

			}
		}

		for (int i = 0; i < numOfAttrs; i++) {

			FieldArray.put(newFields[i]);

		}

		mTemplateJSON.put("fields", FieldArray);

		JSONObject container = new JSONObject();
		JSONArray containerLayerArr = new JSONArray();// container for correct
														// JSON data format

		containerLayerArr.put(mTemplateJSON);
		container.put("layers", containerLayerArr);
		/// put our new one via update

		//System.out.println("data2Sent:");
		//System.out.println(container.toString());

		parameters.add(new BasicNameValuePair(addDefinitionInputName, container.toString()));

		doSth2Service("add", parameters);

	}
}