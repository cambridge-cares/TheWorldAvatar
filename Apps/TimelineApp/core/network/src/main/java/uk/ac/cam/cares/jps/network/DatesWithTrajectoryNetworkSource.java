package uk.ac.cam.cares.jps.network;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.JsonRequest;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;

/**
 * Network source for trajectory dates related functions
 */
public class DatesWithTrajectoryNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(DatesWithTrajectoryNetworkSource.class);
    private final RequestQueue requestQueue;
    private final Context context;

    /**
     * Constructor of the class. The instantiation is handled by dependency injection.
     *
     * @param requestQueue Volley queue for network request.
     * @param context      App context
     */
    public DatesWithTrajectoryNetworkSource(RequestQueue requestQueue, Context context) {
        this.requestQueue = requestQueue;
        this.context = context;
    }

    /**
     * Get all dates where there is trajectory data
     *
     * @param timezone       Current timezone of the phone
     * @param onSuccessUpper Success callback
     * @param onFailureUpper Failure callback
     */
    public void getDates(String accessToken, String timezone, Response.Listener<Map<YearMonthCompositeKey, List<Integer>>> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String getDatesWithTrajectory = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryqueryagent_getDatesWithData))
                .addQueryParameter("timezone", timezone)
                .build().toString();
        LOGGER.info(getDatesWithTrajectory);

        StringRequest request = new StringRequest(getDatesWithTrajectory, s -> {
            try {
                JSONObject response = new JSONObject(s);
                JSONArray result = response.optJSONArray("result");
                if (result == null) {
                    onSuccessUpper.onResponse(new HashMap<>());
                    return;
                }

                Map<YearMonthCompositeKey, List<Integer>> datesWithTrajectory = new HashMap<>();
                for (int i = 0; i < result.length(); i++) {
                    JSONObject jo = result.getJSONObject(i);
                    datesWithTrajectory.put(new YearMonthCompositeKey(jo.getInt("year"),
                                    jo.getInt("month")),
                            stringToDates(jo.getString("days")));
                }
                onSuccessUpper.onResponse(datesWithTrajectory);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        }, onFailureUpper) {
            @Override
            public Map<String, String> getHeaders() {
                Map<String, String> headers = new HashMap<>();
                headers.put("Authorization", "Bearer " + accessToken);
                return headers;
            }
        };
        requestQueue.add(request);
    }

    private List<Integer> stringToDates(String daysStr) {
        return Arrays.stream(daysStr.replace("{", "").replace("}", "").split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
    }
}
