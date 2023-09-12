package uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

/**
 * A class storing the required information to display the time series of any Facility in the dashboard.
 *
 * @author qhouyee
 */
public class Facility {
    // Key value pair is asset name and its stored information respectively
    private final Map<String, Asset> ASSETS = new HashMap<>();
    private final Map<String, Room> ROOMS = new HashMap<>();
    private final Queue<String[]> FACILITY_THRESHOLDS = new ArrayDeque<>();

    /**
     * Constructor to initialise a facility object with one room and measure.
     *
     * @param roomName      Name of the room to be included.
     * @param measureName   Name of the measure associated with the room.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the room.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public Facility(String roomName, String measureName, String unit, String measureIri, String timeSeriesIri) {
        addRoom(roomName, measureName, unit, measureIri, timeSeriesIri);
    }

    /**
     * Constructor to initialise a facility object with one asset and measure.
     *
     * @param assetName     Name of the asset to be included.
     * @param assetType     Type of the asset to be included.
     * @param measureName   Name of the measure associated with the asset.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the asset.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public Facility(String assetName, String assetType, String measureName, String unit, String measureIri, String timeSeriesIri) {
        addAsset(assetName, assetType, measureName, unit, measureIri, timeSeriesIri);
    }

    /**
     * A getter method to retrieve all available rooms, assets and their corresponding time series and information in the facility.
     * Format: {asset1: [measure1, dataIRI, timeseriesIRI, unit, assetType], [measure2, dataIRI, timeseriesIRI, null(if no unit), assetType]],
     * room1: [[measureName, dataIRI, timeseriesIRI, unit], [measureName, dataIRI, timeseriesIRI, unit]], ...],
     * thresholds: [[measureName, min, max],...]}
     *
     * @return A map linking all assets and rooms to their measures.
     */
    public Map<String, Queue<String[]>> getAllMeasures() {
        // For all assets, store them in a map with their asset type as a key and individual asset names as values
        Map<String, Queue<String[]>> measures = new HashMap<>();
        for (Asset asset : this.ASSETS.values()) {
            String assetName = asset.getAssetName();
            measures.put(assetName, asset.getAssetData());
        }
        for (Room room : this.ROOMS.values()) {
            String roomName = room.getRoomName();
            measures.put(roomName, room.getRoomData());
        }
        // Only add the thresholds if there are values
        if (!this.FACILITY_THRESHOLDS.isEmpty()) measures.put(StringHelper.THRESHOLD_KEY, this.FACILITY_THRESHOLDS);
        return measures;
    }

    /**
     * Add a room into this class.
     *
     * @param roomName      Name of the room to be included.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the room.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public void addRoom(String roomName, String measureName, String unit, String measureIri, String timeSeriesIri) {
        // Check if the room already exists in the map using its name as a key
        if (this.ROOMS.containsKey(roomName)) {
            // If there is a preceding room object, add only the measure to the right room
            Room room = this.ROOMS.get(roomName);
            room.addMeasure(measureName, unit, measureIri, timeSeriesIri);
        } else {
            // If it does not exist, create a new room and add it into the map
            Room room = new Room(roomName, measureName, unit, measureIri, timeSeriesIri);
            this.ROOMS.put(roomName, room);
        }
    }

    /**
     * Add an asset into this class.
     *
     * @param assetName     Name of the asset to be included.
     * @param assetType     Type of the asset to be included.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the asset.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public void addAsset(String assetName, String assetType, String measureName, String unit, String measureIri, String timeSeriesIri) {
        // Check if the asset already exists in the map using its name as a key
        if (this.ASSETS.containsKey(assetName)) {
            // If there is a preceding asset object, add only the measure to the right asset
            Asset asset = this.ASSETS.get(assetName);
            asset.addMeasure(measureName, unit, measureIri, timeSeriesIri);
        } else {
            // If it does not exist, create a new asset and add it into the map
            Asset element = new Asset(assetName, assetType, measureName, unit, measureIri, timeSeriesIri);
            this.ASSETS.put(assetName, element);
        }
    }

    /**
     * Add the thresholds based on measures into this class.
     *
     * @param measureName  Name of the measure.
     * @param minThreshold Min threshold set for this measure in this facility.
     * @param maxThreshold Max threshold set for this measure in this facility.
     */
    public void addThresholds(String measureName, String minThreshold, String maxThreshold) {
        String[] thresholdData = new String[]{measureName, minThreshold, maxThreshold};
        this.FACILITY_THRESHOLDS.offer(thresholdData);
    }
}
