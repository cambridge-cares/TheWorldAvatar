package uk.ac.cam.cares.jps.network.datasheet;

import android.content.Context;
import android.net.Uri;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;
import uk.ac.cam.cares.jps.network.R;
import uk.ac.cam.cares.jps.utils.FileUtils;
import uk.ac.cam.cares.jps.utils.SerializationUtils;

public class DataSheetNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(DataSheetNetworkSource.class);

    Connection connection;
    Context context;

    @Inject
    public DataSheetNetworkSource(Connection connection, Context context) {
        BasicConfigurator.configure();
        this.connection = connection;
        this.context = context;
    }

    /**
     *
     * @param assetData JSONObject contains targetID, comments, documentType, fileUri
     * @param onSuccessUpper pass the result back to AssetInfoRepository
     * @param onFailureUpper pass the failure message back to AssetInfoRepository
     */
    public void addDataSheet(JSONObject assetData, Response.Listener<Boolean> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        try {
            // convert fileUri to fileName and encodedPDF Base64 string
            Uri fileUri = Uri.parse((String) assetData.remove("fileUri"));
            assetData.put("fileName", FileUtils.getFileNameFromUri(fileUri, context));
            assetData.put("encoded", SerializationUtils.serializeFileToString(fileUri, context));

            JSONObject param = new JSONObject();
            param.put("assetData", assetData);

            String requestUri = NetworkConfiguration.constructUrlBuilder(context.getString(R.string.add_manual), context)
                    .build().toString();
            LOGGER.info(requestUri);

            JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, requestUri, param, response -> {
                try {
                    String resultMessage = response.getString("Result");
                    if (resultMessage.contains("PDF saved successfully")) {
                        onSuccessUpper.onResponse(true);
                    } else {
                        onFailureUpper.onErrorResponse(new VolleyError(resultMessage));
                    }
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }, error -> {
                onFailureUpper.onErrorResponse(error);
            });
            connection.addToRequestQueue(request);

        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }
}
