package uk.ac.cam.cares.jps.agent.mobileappagent;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.util.Timer;
import java.util.TimerTask;

import org.json.JSONArray;
import org.postgis.Point;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;

public class WebhookServer {

    static ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>();
    static ArrayList<Double> accelList_x = new ArrayList<>();
    static ArrayList<Double> accelList_y = new ArrayList<>();
    static ArrayList<Double> accelList_z = new ArrayList<>();

    static ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>();
    static ArrayList<Double> magnetometerList_x = new ArrayList<>();
    static ArrayList<Double> magnetometerList_y = new ArrayList<>();
    static ArrayList<Double> magnetometerList_z = new ArrayList<>();

    static ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>();
    static ArrayList<Double> gravityList_x = new ArrayList<>();
    static ArrayList<Double> gravityList_y = new ArrayList<>();
    static ArrayList<Double> gravityList_z = new ArrayList<>();


    static ArrayList<OffsetDateTime> location_tsList = new ArrayList<>();
    static ArrayList<Double> bearingList = new ArrayList<>();
    static ArrayList<Double> speedList = new ArrayList<>();
    static ArrayList<Double> altitudeList = new ArrayList<>();
    static ArrayList<Point> geomLocationList = new ArrayList<>();

    static ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>();
    static ArrayList<Double> dBFSList = new ArrayList<>();

    static ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>();
    static ArrayList<Double> lightValueList = new ArrayList<>();


    public static void main(String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);
        server.createContext("/", new MyHandler());
        server.setExecutor(null);
        server.start();
        System.out.println("Server started on port 8000");

        //Start multithread programme here
        Timer timer = new Timer();
        timer.scheduleAtFixedRate(new ScheduledTask(), 0, 5 * 1000);
        //Step 1: Check if timeseries exists
        //Step 1.1: Create timeseries IRI
        //Step 1.2: Inititailize timeseries
        //Step 2: Hashmap the dataIRI

        //Step 3: Add timeseries data
        //Step 3.1: Downsample data instead

    }

    /**
     * Timertasks
     */
    public static class ScheduledTask extends TimerTask {
        public void run() {
            System.out.println("Printing every 10 seconds");
            accel_tsList=null;
        }
    }


    /**
     * Server that handles HTTP Post requests and parse them into a list
     */

    public static class MyHandler implements HttpHandler {

        @Override
        public void handle(HttpExchange exchange) throws IOException {

            String requestMethod = exchange.getRequestMethod();
                if (requestMethod.equalsIgnoreCase("POST")) {
                    // handle POST request
                    StringBuilder data = new StringBuilder();
                    try (BufferedReader reader = new BufferedReader(new InputStreamReader(exchange.getRequestBody(), StandardCharsets.UTF_8))) {
                        String line;
                        while ((line = reader.readLine()) != null) {
                            data.append("[");
                            data.append(line);
                            data.append("]");
                        }
                    }

                    JSONArray dataHTTP = new JSONArray(data.toString());
                    ObjectMapper mapper = new ObjectMapper();
                    JsonNode root = mapper.readTree(String.valueOf(dataHTTP));

                    JsonNode payload = null;
                    for (JsonNode webhook : root) {
                        Integer messageId = webhook.get("messageId").intValue();
                        String sessionId = webhook.get("sessionId").textValue();
                        String deviceId = webhook.get("deviceId").textValue();
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

                            System.out.println("Microphone value has been added");
                        }

                        if (sensor.textValue().equals("light")) {
                            lightValue_tsList.add(timestamp);
                            lightValueList.add(values.get("lux").doubleValue());
//                            System.out.println("Light value has been added");
                        }
                    }

                    String response = "<html><body><pre>" + "The server is listening" + "</pre></body></html>";
                    exchange.sendResponseHeaders(200, response.length());
                    try (OutputStream os = exchange.getResponseBody()) {os.write(response.getBytes());}
                } else {
                    String response = "<html><body><pre>" + "This is not HTTP Post request" + "</pre></body></html>";
                    try (OutputStream os = exchange.getResponseBody()) {os.write(response.getBytes());}
            }
        }
    }


}
