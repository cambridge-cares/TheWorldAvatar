package uk.ac.cam.cares.jps.network.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;

public class AssetNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(AssetNetworkSource.class);

    String retrievePath = "asset-manager-agent/retrieve";
    String addAssetPath = "asset-manager-agent/instantiate";

    Connection connection;

    @Inject
    public AssetNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getAssetInfoByIri(String iri, Response.Listener<AssetInfoModel> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(retrievePath)
                .build().toString();
        LOGGER.info(requestUri);

        Response.Listener<JSONObject> onSuccess = rawResponse -> {
            Gson gson = new Gson();
            Type type = new TypeToken<HashMap<String, String>>() {}.getType();
            try {
                AssetInfoModel assets = new AssetInfoModel(keyConversion(gson.fromJson(rawResponse.getJSONArray("Result").get(0).toString(), type)));
                assets.getProperties().put(INVENTORY_ID, rawResponse.getJSONArray("ID").get(0).toString());
                assets.setHasTimeSeries((Boolean) rawResponse.getJSONArray("Result").get(1));
                onSuccessUpper.onResponse(assets);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        JSONObject requestBody = new JSONObject();
        JSONObject assetData = new JSONObject();
        try {
            assetData.put("ID", iri);
            requestBody.put("assetData", assetData);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

        JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, requestUri, requestBody, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }

    public void addAsset(JSONObject param, Response.Listener<JSONObject> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(addAssetPath).build().toString();

        Response.Listener<JSONObject> onSuccess = response -> {
            try {
                if (response.getJSONArray("Result").getJSONObject(0).has("deviceIRI")) {
                    onSuccessUpper.onResponse(response.getJSONArray("Result").getJSONObject(0));
                }
            } catch (JSONException e) {
                onFailureUpper.onErrorResponse(new VolleyError("Failed to create new asset"));
            }
        };

        JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, requestUri, param, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }

    private Map<String, String> keyConversion(Map<String, String> rawInput) {
        Map<String, String> result = new HashMap<>();
        for (String key : rawInput.keySet()) {
            switch (key) {
                case "ServiceCategoryName":
                    result.put(SERVICE_CODE, rawInput.get(key));
                    break;
                case "PurchaseOrderNum":
                    result.put(PURCHASE_ORDER_NUMBER, rawInput.get(key));
                    break;
                case "price":
                    result.put(PURCHASE_PRICE, rawInput.get(key) + " S$");
                    break;
                case "itemComment":
                    result.put(COMMENTS, rawInput.get(key));
                    break;
                case "serialNum":
                    result.put(SERIAL_NUMBER, rawInput.get(key));
                    break;
                case "ManufacturerName":
                    result.put(MANUFACTURER, rawInput.get(key));
                    break;
                case "manufacturerURL":
                    result.put(MANUFACTURE_URL, rawInput.get(key));
                    break;
                case "roomName":
                    result.put(ROOM, rawInput.get(key));
                    break;
                case "buildingName":
                    result.put(BUILDING, rawInput.get(key));
                    break;
                case "DeliveryOrderNum":
                    result.put(DELIVERY_ORDER_NUMBER, rawInput.get(key));
                    break;
                case "SpecSheetPage":
                    result.put(SPEC_SHEET_PAGE_NO, rawInput.get(key));
                    break;
                case "storage":
                    result.put(STORED_IN, rawInput.get(key));
                    break;
                case "assignedTo":
                    result.put(ASSIGNED_TO, rawInput.get(key));
                    break;
                case "Manual":
                    result.put(MANUAL_SECTION_TITLE, rawInput.get(key));
                    break;
                case "workspaceName":
                    result.put(WORKSPACE, rawInput.get(key));
                    break;
                case "facilityName":
                    result.put(FACILITY, rawInput.get(key));
                    break;
                case "label":
                    result.put(REFERENCE_LABEL, rawInput.get(key));
                    break;
                case "SupplierName":
                    result.put(VENDOR, rawInput.get(key));
                    break;
                case "modelNumber":
                    result.put(MODEL_NUMBER, rawInput.get(key));
                    break;
                case "Location":
                    // NOTE: this key is for the asset in people's home or other places that are not in CREATE buildings so without IFC representation
                    break;
                case "InvoiceNum":
                    result.put(INVOICE_NUMBER, rawInput.get(key));
                    break;
                case "ServiceCategoryType":
                    result.put(SERVICE_CATEGORY, rawInput.get(key));
                    break;
                case "SpecSheet":
                    result.put(SPEC_SHEET_SECTION_TITLE, rawInput.get(key));
                    break;
                case "deviceIRI":
                    result.put(IRI, rawInput.get(key));
                    break;
                default:
                    break;
            }
        }
        return result;
    }
}
