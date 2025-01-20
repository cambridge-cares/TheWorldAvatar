package cares.cam.ac.uk.ouraring;

import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Instant;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import cares.cam.ac.uk.ouraring.data.User;

public class OuraRingApi {
    void setHeartRateData(User user, Instant lowerbound, Instant upperbound) {
        URIBuilder uriBuilder;
        try {
            uriBuilder = new URIBuilder("https://api.ouraring.com/v2/usercollection/heartrate");
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
        uriBuilder.addParameter("start_datetime", lowerbound.toString());
        uriBuilder.addParameter("end_datetime", upperbound.toString());

        HttpGet get;
        try {
            get = new HttpGet(uriBuilder.build());

        } catch (URISyntaxException e) {
            throw new RuntimeException();
        }

        get.setHeader("Authorization", "Bearer " + user.getOuraApiKey());

        CloseableHttpClient httpClient = HttpClients.createDefault();

        try (CloseableHttpResponse response = httpClient.execute(get)) {
            JSONTokener tokener = new JSONTokener(response.getEntity().getContent());
            JSONObject jsonObject = new JSONObject(tokener);
            JSONArray data = jsonObject.getJSONArray("data");

            for (int i = 0; i < data.length(); i++) {
                JSONObject dataElement = data.getJSONObject(i);
                user.getHeartRateData().addValue(Instant.parse(dataElement.getString("timestamp")),
                        dataElement.getDouble("bpm"));
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
