package uk.ac.cam.cares.jps.virtualsensor.visualisation;

import java.io.BufferedWriter;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.virtualsensor.objects.Point;

public class VisualisationWriter {
    private static Logger LOGGER = LogManager.getLogger(VisualisationWriter.class);

    public static void makeDirectories(String visfolder, List<String> pollutants, Instant time) {
        Path visfolderPath = new File(visfolder).toPath();
        visfolderPath.toFile().mkdirs();

        for (String pollutant : pollutants) {
            visfolderPath.resolve(pollutant).resolve(String.valueOf(time.getEpochSecond())).toFile().mkdirs();
        }
    }
    
    public static void writeMainMetaFile(String visfolder, List<String> pollutants, Point centre) {
        Path file = Paths.get(visfolder, "meta.json");

        JSONObject json = new JSONObject();
        
        JSONObject global = new JSONObject();
        global.put("defaultCentre", new JSONArray().put(centre.getX()).put(centre.getY()));
        global.put("defaultZoom", 8.75);
        global.put("defaultBearing", 0.0);
        global.put("defaultPitch", 0.0);
    
        JSONObject local = new JSONObject();
        JSONArray groups = new JSONArray();

        for (String pol : pollutants) {
            JSONObject pol_scenario = new JSONObject();
            pol_scenario.put("name", pol);
            pol_scenario.put("directory", pol);
            groups.put(pol_scenario);
        }

        local.put("groups", groups);
        local.put("label", "Pollutants");

        json.put("global", global);
        json.put("local", local);

        writeToFile(file, json);
    }
    
    /**
     * @param visfolder - location to write file
     * @param pyresponse - response from python service
     */
    public static void writeTree(String visfolder) {
        Path file = Paths.get(visfolder, "tree.json");
    
        // ship layer
        JSONObject shiplayer = new JSONObject();
        shiplayer.put("layerName", "Ships");
        shiplayer.put("defaultState", "visible");
        shiplayer.put("layerIDs", new JSONArray().put("Ships"));
    
        // Pollutant layer
        JSONObject pollutantLayer = new JSONObject();
        pollutantLayer.put("layerName", "Pollutants");
        pollutantLayer.put("defaultState", "visible");
        pollutantLayer.put("layerIDs", new JSONArray().put("Pollutants"));
    
        JSONArray allLayers = new JSONArray();
        allLayers.put(shiplayer);
        allLayers.put(pollutantLayer);
    
        writeToFile(file, allLayers);
    }
    
    public static void writeTimestepMetaFile(String visfolder, List<String> pollutants, Instant timestep) {
        JSONObject json = new JSONObject();

        JSONObject group = new JSONObject();
        group.put("name", timestep);
        group.put("directory", String.valueOf(timestep.getEpochSecond()));

        json.put("groups", new JSONArray().put(group));
        json.put("label", "Timestep");
        
        for (String pol : pollutants) {
            Path file = Paths.get(visfolder, pol, "meta.json");
            writeToFile(file, json);
        }
    }

    /**
     * 1 level below timestep folder
     */
    public static void writeIndividualMetaFile(String visfolder, List<String> pollutants, Instant timestep) {
        JSONArray dataSets = new JSONArray();

        JSONObject contour = new JSONObject();
        contour.put("dataLocation", "contour.geojson");
        contour.put("name", "Pollutants");
        contour.put("locationType", "polygon");

        dataSets.put(contour);

        JSONObject ships = new JSONObject();
        ships.put("dataLocation", "ships.geojson");
        ships.put("name", "Ships");
        ships.put("locationType", "point");

        dataSets.put(ships);

        for (String pol : pollutants) {
            Path file = Paths.get(visfolder, pol, String.valueOf(timestep.getEpochSecond()), "meta.json");
            writeToFile(file, new JSONObject().put("dataSets", dataSets));
        }
    }

    public static void writeGeoJSONContours(String visfolder, List<String> pollutants, Instant timestep, JSONObject pyresponse) {
        for (String pol : pollutants) {
            Path file = Paths.get(visfolder, pol, String.valueOf(timestep.getEpochSecond()), "contour.geojson");
            
            // only getting ground level contour
            JSONObject geojson_from_python = pyresponse.getJSONArray(pol).getJSONObject(0);

            // modifications for digital twin visualisation framework
            JSONArray features = geojson_from_python.getJSONArray("features");

            // each contour is a collection of polygons
            for (int i = 0; i < features.length(); i++) {
                JSONObject properties = features.getJSONObject(i).getJSONObject("properties");
                properties.put("name", properties.getString("title"));
                properties.remove("title");
                properties.put("fill-color", properties.getString("fill"));
                properties.remove("fill");
            }

            writeToFile(file, geojson_from_python);
        }
    }

    public static void writeShipGeoJSON(String visfolder, List<String> pollutants, Instant timestep, JSONArray ships) {
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");
        JSONArray features = new JSONArray();

        for (int i = 0; i < ships.length(); i++) {
            // each ship will be a feature
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");
            //id for mapbox
            feature.put("id", i);

            // display properties
            JSONObject property = new JSONObject();
            property.put("circle-color", "#000000");
			property.put("circle-stroke-width", 1);
			property.put("circle-stroke-color", "#000000"); // black
			property.put("circle-opacity", 0.75);
            property.put("displayName", ships.getJSONObject(i).getString("IRI"));

            feature.put("properties", property);

            // geometry (coordinates)
            JSONObject geometry = new JSONObject();
            geometry.put("type", "Point");
            geometry.put("coordinates", ships.getJSONObject(i).getJSONArray("coordinates"));

            feature.put("geometry", geometry);
            features.put(feature);
        }

        featureCollection.put("features", features);

        // write the same file for each pollutant
        for (String pol : pollutants) {
            Path file = Paths.get(visfolder, pol, String.valueOf(timestep.getEpochSecond()), "ships.geojson");
            writeToFile(file, featureCollection);
        }
    }

    private static void writeToFile(Path file, JSONObject output) {
        try (BufferedWriter writer = Files.newBufferedWriter(file)) {
            output.write(writer, 4, 0);
            LOGGER.debug("Created " + file.toAbsolutePath());
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(e);
        }
    }
    
    private static void writeToFile(Path file, JSONArray output) {
        try (BufferedWriter writer = Files.newBufferedWriter(file)) {
            output.write(writer, 4, 0);
            LOGGER.debug("Created " + file.toAbsolutePath());
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException(e);
        }
    }
}
