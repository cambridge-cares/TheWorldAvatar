package cam.dev.zhouxiaochi;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;


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

public class FeatureServiceUpdater {

	final static String[] fieldAttrs = { "name", "type", "alias", "sqlType", "nullable", "editable", "domain",
			"defaultValue", "length" };//All attributes of a field definition

	final static String defaultLayerName = "Blower";//default name for layer + index
	static String token;// security token to be acquired
	
	final static String addDefinitionInputName = "addToDefinition";
	final static String deleteDefinitionInputName = "deleteFromDefinition";
	final static String updateDefinitionInputName = "updateDefinition";


	public static void main(String[] args) {
		
		/*********************previous testing, left only for reference********************///

		// List<NameValuePair> parameters = new ArrayList<NameValuePair>();
		// parameters.add(new BasicNameValuePair("f", "json"));
		// parameters.add(new BasicNameValuePair("where", "OBJECTID = 99"));

		// get current field list
		/**********
		 * JSONObject featureInfo = DoSth2Table("layer", parameters);
		 * List<String> fieldList = new ArrayList<String>();
		 * 
		 * try {
		 * 
		 * JSONArray fieldJArr = (JSONArray)featureInfo.getJSONArray("fields");
		 * for (int i = 0 ; i < fieldJArr.length(); i++){ String attrName =
		 * fieldJArr.getJSONObject(i).getString("name");
		 * fieldList.add(attrName); }
		 * 
		 * //make a new feature
		 * 
		 * 
		 * JSONObject attri = new JSONObject(); JSONObject geo = new
		 * JSONObject(); JSONObject newF = new JSONObject();
		 * 
		 * try {
		 * 
		 * 
		 * attri.put("name", "E-200"); geo.put("x", 1); geo.put("y", 2);
		 * //newF.put("geometry",geo); newF.put("attributes",attri );
		 * System.out.println("Check newFeature JSON before adding:"+
		 * newF.toString());
		 * 
		 * } catch (JSONException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 * 
		 * 
		 * 
		 * parameters.add(new BasicNameValuePair("features", newF.toString()));
		 * DoSth2Table("add", parameters);
		 * 
		 * 
		 * /*** } catch (JSONException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 * 
		 * 
		 * 
		 * 
		 * /*****************testAddAttribute************************ final int
		 * NUM_OF_RECORDS = 98; final int NUM_OF_NEWATTRI = 2;
		 * 
		 * 
		 * List<String> mAttriNameList = new ArrayList<String>(NUM_OF_NEWATTRI);
		 * 
		 * mAttriNameList.add("dummy"); mAttriNameList.add("stupid");
		 * 
		 * List<String> dummyList = new ArrayList<String>(NUM_OF_RECORDS);
		 * List<String> stupidList = new ArrayList<String>(NUM_OF_RECORDS);
		 * List<List<String>> mListList =new
		 * ArrayList<List<String>>(NUM_OF_NEWATTRI); for(int i = 0; i <
		 * NUM_OF_RECORDS; i++){ dummyList.add("I am a dummy");
		 * stupidList.add("I am stupid");
		 * 
		 * } mListList.add(dummyList); mListList.add(stupidList);
		 * System.out.println(dummyList.size());
		 * 
		 * try { addAttribute(mAttriNameList,mListList); //now test with query
		 * List<NameValuePair> parameters = new ArrayList<NameValuePair>();
		 * parameters.add(new BasicNameValuePair("f", "json"));
		 * parameters.add(new BasicNameValuePair("where", "OBJECTID = 610"));
		 * DoSth2Table("query", parameters);
		 * 
		 * 
		 * 
		 * 
		 * } catch (JSONException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 * 
		 * 
		 * 
		 * List<NameValuePair> parameters = new ArrayList<NameValuePair>();
		 * parameters.add(new BasicNameValuePair("f", "json")); try { JSONArray
		 * fieldArr = DoSth2Table("layer", parameters).getJSONArray("fields");
		 * 
		 * JSONObject newField = new JSONObject(); newField.put("name",
		 * "dummy"); newField.put("type", "esriFieldTypeString");
		 * newField.put("alias", "DUMMY"); newField.put("editable", "true");
		 * newField.put("nullable", "true"); fieldArr.put(newField);
		 * parameters.add(new BasicNameValuePair("fields", "fieldArr"));
		 * 
		 * DoSth2Table("layer", parameters);
		 * 
		 * parameters.clear(); parameters.add(new BasicNameValuePair("f",
		 * "json"));
		 * 
		 * 
		 * 
		 * } catch (JSONException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 * 
		 * 
		 * ///get fields //put fields back
		 * 
		 * 
		 *******************************/
		/// test with change field
		/**
		 * 
		 * { "name" : "OBJECT3", "type" : "esriFieldTypeString", "alias" :
		 * "HI5", "sqlType" : "sqlTypeOther", "length" : 0, "nullable" : true,
		 * "editable" : true, "domain" : null, "defaultValue" : null }
		 * 
		 */

		/**
		 * 
		 * try { //get token String token = generateToken().getString("token");
		 * System.out.println("token acquired: "+token);
		 * 
		 * 
		 * //put in parameters to be sent via REST List<NameValuePair>
		 * parameters = new ArrayList<NameValuePair>(); parameters.add(new
		 * BasicNameValuePair("f", "json")); parameters.add(new
		 * BasicNameValuePair("token", token));
		 * 
		 * //create a json obj of the field to be deleted JSONObject container =
		 * new JSONObject(); JSONArray fields = new JSONArray(); JSONObject
		 * aField = new JSONObject(); try { aField.put("name", "hello");
		 * aField.put("type", "esriFieldTypeString");
		 * 
		 * aField.put("alias", "HI");
		 * 
		 * aField.put("sqlType", "sqlTypeOther");
		 * 
		 * aField.put("length", 0);
		 * 
		 * aField.put("nullable", true);
		 * 
		 * aField.put("editable", true);
		 * 
		 * aField.put("domain", (Object) null);
		 * 
		 * aField.put("defaultValue", (Object) null); fields.put(aField);
		 * 
		 * container.put("fields", fields); System.out.println("field to be
		 * deleted: "+container.toString());
		 * 
		 * parameters.add(new BasicNameValuePair("deleteFromDefinition",
		 * container.toString()));
		 * 
		 * //call delete via REST doSth2Service("delete", parameters); } catch
		 * (JSONException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 * 
		 * } catch (JSONException e) { // TODO Auto-generated catch block
		 * System.out.println("ERR:Geting TOken json exception");
		 * e.printStackTrace(); }
		 * 
		 **/

		/*************test generate new layer*******************************/
		//cosntruct test dummy list
		String[] nameList = { "dummy", "stupid" }; 
		String[] typeList = { "esriFieldTypeDouble", "esriFieldTypeDouble" };

		//construct name map of lists
		Map<String, String[]> attrLists = new HashMap();

		attrLists.put("name", nameList);
		attrLists.put("type", typeList);
		attrLists.put("alias", nameList);
		int lengthOfEachList = 2;

		try {
			generateLayer(lengthOfEachList , attrLists);
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}



	/**************
	 * retreive token from web token service
	 * @return String: token
	 * @throws JSONException
	 */
	static String generateToken() throws JSONException {
		String mURL = "https://www.arcgis.com/sharing/generateToken";
		// "https://www.arcgis.com/sharing/rest/generateToken
		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("username", "kleinelanghorstmj"));

		parameters.add(new BasicNameValuePair("password", "h3OBhT0gR4u2k22XZjQltp"));
		parameters.add(new BasicNameValuePair("client", "referer"));
		parameters.add(new BasicNameValuePair("referer", "http://www.arcgis.com"));

		return httpRequest(mURL, "application/x-www-form-urlencoded; charset=utf-8", parameters).getString("token");
	}

	/****
	 *  Delete, Add, Query features or request for the layer info
	 * @param option  [delete|add|query|info], action to be taken to the features in the layer
	 * @param layerInd   index of layer to be taken action of
	 * @param parameters   parameters to be carried in the request, see the complete list of possible parameters on Arcgis REST API DOC
	 * @return
	 */
	static JSONObject doSth2Feature(String option, int layerInd, List<NameValuePair> parameters) {

		String baseURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/test003/FeatureServer";
		String deleteFeatureURL = "/deleteFeatures";
		String queryFeatureURL = "/query";
		String addFeatureURL = "/addFeatures";
		String layerURL = "/" + layerInd;

		String mURL = baseURL + layerURL;

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
		case "info":
			break;

		default:
			return null;
		}

		return httpRequest(mURL, "application/x-www-form-urlencoded; charset=utf-8", parameters);

	}


	/***
	 * Delete ALL Features in a table
	 * @param layerInd    index of a layer
	 * @return  success flag: bool
	 */
	static boolean deleteAllDataInTable(int layerInd) {
		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("where", "OBJECTID > -1"));

		if (doSth2Feature("delete", layerInd, parameters) == null) {
			return false;
		}
		return true;
	}

	/*****
	 * sent an httpRequest via POST method
	 * @param url         url the request to be sent to
	 * @param contentType       content type to be sent
	 * @param parameters          parameters to be carried in the request
	 * @return
	 */
	static JSONObject httpRequest(String url, String contentType, List<NameValuePair> parameters) {
		// "application/x-www-form-urlencoded; charset=utf-8"
	HttpClient httpClient = new DefaultHttpClient();
		HttpResponse response = null;

		try {

		//	System.out.println("_____________________________________________");
		//	System.out.println("Start POST request");

			HttpPost request = new HttpPost(url);
			// System.out.println("input URL:"+url);

			// StringEntity params = new StringEntity(testFeatureJSON);
			// request.setEntity(params);

			///////////////////// set
			///////////////////// request///////////////////////////////////////////
			request.addHeader("content-type", contentType);
			if (parameters != null)
				request.setEntity(new UrlEncodedFormEntity(parameters, "UTF-8"));
	//		System.out.println(parameters.toString());

	//		System.out.println(request.getEntity().toString());

			// debug:request header
	//		System.out.println("Request URL: " + request.getURI());
	//		System.out.println("Request Header:");

			for (Header h : request.getAllHeaders())
	//			System.out.println(h.toString());

			/////// debug:print out response

			response = httpClient.execute(request);
			if (response != null) { // handle response

				/////// debug: response header/////////////
		//		System.out.println("Response header:");
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
//				System.out.println("response JSON:");

				System.out.println(result.toString());
				JSONObject obj = new JSONObject(result.toString());
				return obj;

			}

		} catch (Exception ex) {
			// handle exception here
	//		System.out.println("err in http request");
			return null;

		} finally {
		}
		return null;

	}

	/***
	 * 
	 * @param option   option  [delete|add|query|info], action to be taken to the attributes in the layer
	 * @param layerInd    index of layer to be taken action of
	 * @param parameters     parameters to be carried in the request, see the complete list of possible parameters on Arcgis REST API DOC
	 * @return
	 * @throws JSONException
	 */
	static JSONObject doSth2Fields(String option, int layerInd, List<NameValuePair> parameters) throws JSONException {

		String baseURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/test003/FeatureServer";
		String addDefURL = "/addToDefinition";
		String updateDefURL = "/updateDefinition";
		String deleteDefURL = "/deleteFromDefinition";
		String layerURL = "/" + layerInd;
		String mURL = baseURL + layerURL;
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

	/***
	 * 
	 * @param option   option  [delete|add|query|info], action to be taken to the service
	 * @param parameters     parameters to be carried in the request, see the complete list of possible parameters on Arcgis REST API DOC
	 * @return
	 * @throws JSONException
	 */
	static JSONObject doSth2Service(String option, List<NameValuePair> parameters) throws JSONException {

		String baseURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/test003/FeatureServer";
		String addDefURL = "/addToDefinition";
		String updateDefURL = "/updateDefinition";
		String deleteDefURL = "/deleteFromDefinition";
		String mURL = baseURL;
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

	/***
	 * deprecated(Left only for reference )
	 * @param AttrNameList         
	 * @param listsOfValues
	 * @param layerInd
	 * @return
	 * @throws JSONException
	 */
	@Deprecated
	static boolean repopulateTable(List<String> AttrNameList, List<List<String>> listsOfValues, int layerInd)
			throws JSONException {

		// query and save orginal data

		if (AttrNameList.size() != listsOfValues.size()) {
			System.out.println("ERR: length of value list does not match length of attr name list");
			return false;
		}

		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("where", "OBJECTID > -1"));

		// get current field list & pack new JSON

		JSONArray AllFeatures = null;

		AllFeatures = doSth2Feature("query", layerInd, parameters).getJSONArray("features");

//		System.out.println("num of features " + AllFeatures.length());
	//	System.out.println("num of new attris " + AttrNameList.size());

		for (int iFeature = 0; iFeature < AllFeatures.length(); iFeature++) {

			JSONObject aFeature = AllFeatures.getJSONObject(iFeature);
			JSONObject attriList = AllFeatures.getJSONObject(iFeature).getJSONObject("attributes");

			for (int inewAttr = 0; inewAttr < AttrNameList.size(); inewAttr++) {

				List<String> valueList = listsOfValues.get(inewAttr);
				attriList.put(AttrNameList.get(inewAttr), valueList.get(iFeature));
				// aFeature

//				System.out.println("adding new attr " + AttrNameList.get(inewAttr) + ": " + valueList.get(iFeature));

			}
		}
	//	System.out.println("New Table: ");
	//	System.out.println(AllFeatures.toString());

		// deleteFullTable
		if (!deleteAllDataInTable(0)) {
			System.out.println("ERR:Deleting previous table failed ");

			return false;
		}

		// repopulate table from JSON

		parameters.clear();
		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("features", AllFeatures.toString()));

		if (doSth2Feature("add", layerInd, parameters) == null)
			return false;

		return true;

	}

	/*****
	 * generate a new layer from certain template, now is test004
	 * @param numOfAttrs   length of each attriValueList = num of total attributes
	 * All other attributes without a input list will be default value.   
	 * @param attrValueListsMap:Map<String, String[]>      a name map of all the input attribute values.
	 * @throws JSONException
	 */
	static void generateLayer(int numOfAttrs, Map<String, String[]> attrValueListsMap) throws JSONException {

		if (token == null) {
			token = generateToken();
//			System.out.println("token acquired: " + token);
		}
		// check if every value string is of required length
		Iterator it = attrValueListsMap.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry pair = (Map.Entry) it.next();

			if (((String[]) pair.getValue()).length != numOfAttrs) {
//				System.out.println("Array for attributes are not of required length: " + numOfAttrs);
				return;
			}

		}

		//// TODO:now get layer template from test004, in the future the template url will be a  parameter instead

		List<NameValuePair> parameters = new ArrayList<NameValuePair>();

		parameters.add(new BasicNameValuePair("f", "json"));
		parameters.add(new BasicNameValuePair("token", token));

		String templateServiceURL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/admin/services/test004/FeatureServer";
		JSONObject templateLayer = httpRequest(templateServiceURL, "application/x-www-form-urlencoded; charset=utf-8",
				parameters).getJSONArray("layers").getJSONObject(0);

		JSONArray thisLayers = doSth2Service("info", parameters).getJSONArray("layers");

		int curLayersNum = thisLayers.length();

		templateLayer.put("id", curLayersNum);
		curLayersNum++;

		templateLayer.put("name", defaultLayerName + curLayersNum);

		templateLayer.remove("adminLayerInfo");

		// String newTableName =
		// templateLayer.getJSONObject("adminLayerInfo").getString("tableName")+curLayersNum;

		// templateLayer.getJSONObject("adminLayerInfo").put("tableName",
		// newTableName);
		templateLayer.remove("fields");
		JSONArray FieldArray = new JSONArray();

		//construct globalID and objectID definition to JSON
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

		//populate field definitions with  input value arrays
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

		templateLayer.put("fields", FieldArray);

		JSONObject container = new JSONObject();
		JSONArray containerLayerArr = new JSONArray();//container for correct JSON data format

		containerLayerArr.put(templateLayer);
		container.put("layers", containerLayerArr);
		/// put our new one via update

	//	System.out.println("data2Sent:");
//		System.out.println(container.toString());

		parameters.add(new BasicNameValuePair(addDefinitionInputName, container.toString()));

		doSth2Service("add", parameters);

	}
}