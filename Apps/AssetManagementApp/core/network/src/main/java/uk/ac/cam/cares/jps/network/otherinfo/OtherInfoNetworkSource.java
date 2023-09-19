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

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;

public class OtherInfoNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(OtherInfoNetworkSource.class);

//    String path = "asset-manager-agent/getuidata";
    String path = "android-status-agent/testdata";


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
    public void getOtherInfoFromAssetAgent(Response.Listener<Map<String, HashMap<String, String>>> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(path).build().toString();
        LOGGER.info(requestUri);

        Response.Listener<String> onSuccess = response -> {
            Gson gson = new Gson();
            Type type = new TypeToken<List<HashMap<String, String>>>() {}.getType();
            try {
                Map<String, HashMap<String, String>> results = new HashMap<>();
                JSONObject resultJson = new JSONObject(response).getJSONObject("result");
                // todo: check type related code after agent is done
                results.put(TYPE, keyConversion(gson.fromJson(resultJson.getJSONArray("Type").toString(), type), TYPE));
                results.put(ASSIGNED_TO, keyConversion(gson.fromJson(resultJson.getJSONArray("User").toString(), type), ASSIGNED_TO));
                results.put(VENDOR, keyConversion(gson.fromJson(resultJson.getJSONArray("Supplier").toString(), type), SUPPLIER_SECTION_TITLE));
                results.put(MANUFACTURER, keyConversion(gson.fromJson(resultJson.getJSONArray("Manufacturer").toString(), type), MANUFACTURER));
                results.put(INVOICE_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("Invoice").toString(), type), INVOICE_NUMBER));
                results.put(PURCHASE_ORDER_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("PurchaseOrder").toString(), type), PURCHASE_ORDER_NUMBER));
                results.put(DELIVERY_ORDER_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("DeliveryOrder").toString(), type), DELIVERY_ORDER_NUMBER));
                results.put(PURCHASE_REQUEST_NUMBER, keyConversion(gson.fromJson(resultJson.getJSONArray("PurchaseRequest").toString(), type), PURCHASE_REQUEST_NUMBER));

                // todo: test data
//                results.put(ITEM_NAME, keyConversion(gson.fromJson(resultJson.getJSONArray("Items").toString(), type), ITEM_NAME));
                results.put(LOCATED_IN, keyConversion(gson.fromJson(resultJson.getJSONArray("Rooms").toString(), type), LOCATED_IN));
                results.put(SEAT_LOCATION, keyConversion(gson.fromJson(resultJson.getJSONArray("Seats").toString(), type), SEAT_LOCATION));
//                results.put(STORED_IN, keyConversion(gson.fromJson(resultJson.getJSONArray("Containers").toString(), type), STORED_IN));

                onSuccessUpper.onResponse(results);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest request = new StringRequest(Request.Method.GET, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }

    public void getLocationRelatedInfoFromBMSAgent(Response.Listener<Map<String, HashMap<String, String>>> onSuccessUpper, Response.ErrorListener onFailureUpper) {

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

}
