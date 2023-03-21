package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unimi.dsi.fastutil.Hash;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.crypto.Data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;
import uk.ac.cam.cares.downsampling.Downsampling;




@WebServlet(urlPatterns = "/update")

public class SensorLoggerMobileAppAgent extends JPSAgent {
    private QueryClientSPARQL queryClientSPARQL;
    private RemoteStoreClient storeClient;
    private LinkedList<HttpServletRequest> requestQueue;
    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClientSPARQL = new QueryClientSPARQL(storeClient);
        requestQueue = new LinkedList<>();
    }


    private static final long serialVersionUID = 1L;

    private static HashMap smartphoneHashmap = new HashMap();

    protected void doPost(HttpServletRequest request, HttpServletResponse response)  throws ServletException, IOException {

        // Add incoming request to queue
        requestQueue.add(request);

        // Process queue if necessary
        processRequestQueue(response);

    }

    private void processRequestQueue(HttpServletResponse response) throws IOException {
        // Process requests in queue until empty
        while (!requestQueue.isEmpty()) {
            HttpServletRequest request = requestQueue.remove();

            //Declare variables
            String DEVICEID = null;
            //Accelerometer list
            ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>();
            List<Double> accelList_x = new ArrayList<>();
            List<Double> accelList_y = new ArrayList<>();
            List<Double> accelList_z = new ArrayList<>();

            //Magnetometer list
            ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>();
            List<Double> magnetometerList_x = new ArrayList<>();
            List<Double> magnetometerList_y = new ArrayList<>();
            List<Double> magnetometerList_z = new ArrayList<>();

            //Gravity sensor list
            ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>();
            List<Double> gravityList_x = new ArrayList<>();
            List<Double> gravityList_y = new ArrayList<>();
            List<Double> gravityList_z = new ArrayList<>();

            //Location list
            ArrayList<OffsetDateTime> location_tsList = new ArrayList<>();
            List<Double> bearingList = new ArrayList<>();
            List<Double> speedList = new ArrayList<>();
            List<Double> altitudeList = new ArrayList<>();
            List<Point> geomLocationList = new ArrayList<>();

            //Microphone lists
            ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>();
            List<Double> dBFSList = new ArrayList<>();

            //Light value lists
            ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>();
            List<Double> lightValueList = new ArrayList<>();

            ArrayList<OffsetDateTime> brightness_tsList = new ArrayList<>();
            List<Double> brightnessList = new ArrayList<>();

            // Send a response back to the client with a 200 status code
            response.setStatus(HttpServletResponse.SC_OK);
            PrintWriter out = response.getWriter();
            out.println("HTTP POST request processed.");

            //Parse incoming data and add them to respective list
            StringBuilder data = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(request.getInputStream(), StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    data.append("[");
                    data.append(line);
                    data.append("]");

                    JSONArray dataHTTP = new JSONArray(data.toString());
                    ObjectMapper mapper = new ObjectMapper();
                    JsonNode root = mapper.readTree(String.valueOf(dataHTTP));

                    JsonNode payload = null;
                    for (JsonNode webhook : root) {
                        Integer messageId = webhook.get("messageId").intValue();
                        String sessionId = webhook.get("sessionId").textValue();
                        String deviceId = webhook.get("deviceId").textValue();
                        DEVICEID = deviceId;
                        payload = webhook.get("payload");
                    }

                    for (JsonNode node : payload) {

                        JsonNode timeEPOCH = node.get("time");
                        JsonNode sensor = node.get("name");
                        JsonNode values = node.get("values");
                        Instant instant = Instant.ofEpochSecond(timeEPOCH.longValue() / 1000000000, timeEPOCH.longValue() % 1000000000);
                        OffsetDateTime timestamp = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC);

                        if (sensor.textValue().equals("accelerometer")) {
                            accel_tsList.add(timestamp);
                            accelList_x.add(values.get("x").doubleValue());
                            accelList_y.add(values.get("y").doubleValue());
                            accelList_z.add(values.get("z").doubleValue());
                        }

                        if (sensor.textValue().equals("magnetometer")) {
                            magnetometer_tsList.add(timestamp);
                            magnetometerList_x.add(values.get("x").doubleValue());
                            magnetometerList_y.add(values.get("y").doubleValue());
                            magnetometerList_z.add(values.get("z").doubleValue());

                        }

                        if (sensor.textValue().equals("gravity")) {
                            gravity_tsList.add(timestamp);
                            gravityList_x.add(values.get("x").doubleValue());
                            gravityList_y.add(values.get("y").doubleValue());
                            gravityList_z.add(values.get("z").doubleValue());

                        }

                        if (sensor.textValue().equals("location")) {
                            location_tsList.add(timestamp);
                            bearingList.add(values.get("bearing").doubleValue());
                            speedList.add(values.get("speed").doubleValue());
                            altitudeList.add(values.get("altitude").doubleValue());

                            //Parse latitude and longitude into geom_location
                            double latitude = values.get("latitude").doubleValue();
                            double longitude = values.get("longitude").doubleValue();
                            Point point = new Point(longitude, latitude);
                            point.setSrid(4326);

                            geomLocationList.add(point);
                        }

                        if (sensor.textValue().equals("microphone")) {
                            dBFS_tsList.add(timestamp);
                            dBFSList.add(values.get("dBFS").doubleValue());
                        }

                        if (sensor.textValue().equals("light")) {
                            lightValue_tsList.add(timestamp);
                            lightValueList.add(values.get("lux").doubleValue());
                        }

                        if (sensor.textValue().equals("brightness")) {
                            brightness_tsList.add(timestamp);
                            brightnessList.add(values.get("brightness").doubleValue());
                        }
                    }
                }
            }

            HashMap dataHashmap = new HashMap<>();
            dataHashmap.put("deviceID", DEVICEID);
            dataHashmap.put("accel_tsList", accel_tsList);
            dataHashmap.put("accelList_x", accelList_x);
            dataHashmap.put("accelList_y", accelList_y);
            dataHashmap.put("accelList_z", accelList_z);
            dataHashmap.put("magnetometer_tsList", magnetometer_tsList);
            dataHashmap.put("magnetometerList_x", magnetometerList_x);
            dataHashmap.put("magnetometerList_y", magnetometerList_y);
            dataHashmap.put("magnetometerList_z", magnetometerList_z);
            dataHashmap.put("gravity_tsList", gravity_tsList);
            dataHashmap.put("gravityList_x", gravityList_x);
            dataHashmap.put("gravityList_y", gravityList_y);
            dataHashmap.put("gravityList_z", gravityList_z);
            dataHashmap.put("location_tsList", location_tsList);
            dataHashmap.put("bearingList", bearingList);
            dataHashmap.put("speedList", speedList);
            dataHashmap.put("altitudeList", altitudeList);
            dataHashmap.put("geomLocationList", geomLocationList);
            dataHashmap.put("dBFS_tsList", dBFS_tsList);
            dataHashmap.put("dBFSList", dBFSList);
            dataHashmap.put("lightValue_tsList", lightValue_tsList);
            dataHashmap.put("lightValueList", lightValueList);
            dataHashmap.put("brightness_tsList", brightness_tsList);
            dataHashmap.put("brightnessList", brightnessList);



            
            try {
                if (queryClientSPARQL.getSmartPhoneIRI(DEVICEID).isEmpty() && !smartphoneHashmap.containsKey(DEVICEID)) {
                    TSInstantiation firstTSInstantiation = new TSInstantiation(dataHashmap);
                    smartphoneHashmap.put(DEVICEID, firstTSInstantiation);
                    firstTSInstantiation.startTimer();
                } else {
                    TSInstantiation retrievedTSInstantiation = (TSInstantiation) smartphoneHashmap.get(DEVICEID);
                    synchronized(this) {
                    retrievedTSInstantiation.setData(dataHashmap);
                    }
                }
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        

            // Log the request data
            System.out.println("Received POST request with data");

            // Build the response string
            String responseString = "Received POST request with data:\n" + data;

            // Set the content type of the response
            response.setContentType("text/plain");

            // Write the response string to the output stream
            out.print(responseString);
            out.flush();
    
        }
    }
}