package uk.ac.cam.cares.jps.network.otherinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.model.building.Workspace;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;

public class OtherInfoNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(OtherInfoNetworkSource.class);

    String path = "asset-manager-agent/getuidata";


    Connection connection;

    @Inject
    public OtherInfoNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    /**
     * Get all types, users, suppliers, manufacturers, invoice number, purchase order number and delivery order from KG via AssetManagerAgent
     * @param onSuccessUpper upper level onSuccess listener. It is created by Repository and used to pass data to repository for processing
     * @param onFailureUpper upper level onFailure listener. It is created by Repository and used to handle error
     */
    public void getOtherInfo(Response.Listener<OtherInfoResponse> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(path).build().toString();
        LOGGER.info(requestUri);

        Response.Listener<String> onSuccess = response -> {
            Gson gson = new Gson();
            Type type = new TypeToken<List<HashMap<String, String>>>() {}.getType();
            try {
                OtherInfoResponse result = new OtherInfoResponse();
                JSONObject resultJson = new JSONObject(response).getJSONObject("result");
                // todo: check type related code after agent is done
//                result.otherInfo.put(TYPE, keyConversion(gson.fromJson(resultJson.getJSONArray("Type").toString(), type), TYPE));
                result.otherInfo.put(ASSIGNED_TO, keyConversion(gson.fromJson(resultJson.getJSONArray("User").toString(), type), ASSIGNED_TO));
                result.otherInfo.put(VENDOR, keyConversion(gson.fromJson(resultJson.getJSONArray("Supplier").toString(), type), SUPPLIER_SECTION_TITLE));
                result.otherInfo.put(MANUFACTURER, keyConversion(gson.fromJson(resultJson.getJSONArray("Manufacturer").toString(), type), MANUFACTURER));
                result.otherInfo.put(INVOICE_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("Invoice").toString(), type), INVOICE_NUMBER));
                result.otherInfo.put(PURCHASE_ORDER_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("PurchaseOrder").toString(), type), PURCHASE_ORDER_NUMBER));
                result.otherInfo.put(DELIVERY_ORDER_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("DeliveryOrder").toString(), type), DELIVERY_ORDER_NUMBER));
//                result.otherInfo.put(PURCHASE_REQUEST_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("PurchaseRequest").toString(), type), PURCHASE_REQUEST_NUMBER));

                // todo: items should be retrieved with a separate call after docs are selected
                result.workspaces  = processElement(gson.fromJson(resultJson.getJSONArray("Element").toString(), type));

                onSuccessUpper.onResponse(result);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        JSONObject requestBody = new JSONObject();
        try {
            requestBody.put("assetData", new JSONObject());
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

        StringRequest request = new StringRequest(Request.Method.POST, requestUri, onSuccess, onFailureUpper) {
            @Override
            public String getBodyContentType() {
                return "application/json; charset=utf-8";
            }

            @Override
            public byte[] getBody() {

                try {
                    return requestBody.toString().getBytes("utf-8");
                } catch (UnsupportedEncodingException e) {
                    LOGGER.error("Unsupported Encoding while trying to get the bytes of" + requestBody + " using utf-8");
                    throw new RuntimeException(e);
                }
            }
        };
        connection.addToRequestQueue(request);
    }

    /**
     * Convert List<HashMap<String, String>> to Map<String, String>
     *     Example:
     *     [{"PersonIRI": "iri1", "PersonNameIRI": "nameIRI1", "PersonName": "name1"},
     *     {"PersonIRI": "iri2", "PersonNameIRI": "nameIRI2", "PersonName": "name2"}]
     *     to
     *     {"iri1": "name1", "iri2": "name2"}
     * @param rawInput
     * @return
     */
    private HashMap<String, String> keyConversion(List<HashMap<String, String>> rawInput, String infoType) {
        String iriKey, nameKey;
        if (infoType.equals(TYPE)) {
            iriKey = "TypeIRI";
            nameKey = "TypeName";
        }
        else if (infoType.equals(ASSIGNED_TO)) {
            iriKey = "PersonIRI";
            nameKey = "PersonName";
        } else if (infoType.equals(SUPPLIER_SECTION_TITLE)) {
            iriKey = "SupplierOrgIRI";
            nameKey = "SupplierName";
        } else if (infoType.equals(MANUFACTURER)) {
            iriKey = "ManufacturerIRI";
            nameKey = "ManufacturerName";
        } else {
            iriKey = "x0";
            nameKey = "x1";
        }

        HashMap<String, String> result = new HashMap<>();
        for (Map<String, String> obj : rawInput) {
            result.put(obj.get(iriKey), obj.get(nameKey));
        }
        return result;
    }

    private List<Workspace> processElement(List<HashMap<String, String>> rawInput) {
        HashMap<String, List<String>> workspaceToElement = new HashMap<>();
        for (HashMap<String, String> obj : rawInput) {
            String iri = obj.get("x0");
            String val = obj.get("x1"); // todo: need to update to id once the agent is done

            workspaceToElement.computeIfAbsent(iri, k -> new ArrayList<>()).add(val);
        }

        List<Workspace> workspaces = new ArrayList<>();
        for (String iri : workspaceToElement.keySet()) {
            Workspace temp = new Workspace(iri);
            temp.constructElements(workspaceToElement.get(iri));
            workspaces.add(temp);
        }
        return workspaces;
    }

}
