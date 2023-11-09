package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;
import java.util.stream.Collectors;

/**
 * A class storing the required information to display the time series of all facilities associated with an organisation in the dashboard.
 *
 * @author qhouyee
 */
public class Organisation {
    // Key value pair is asset name and its stored information respectively
    private final Map<String, Asset> ASSETS = new HashMap<>();
    private final Map<String, Room> ROOMS = new HashMap<>();
    private final Queue<String[]> FACILITY_THRESHOLDS = new ArrayDeque<>();
    private final Set<String> UNIQUE_THRESHOLDS = new HashSet<>();
    private final Map<String, Facility> FACILITIES = new HashMap<>();

    /**
     * Constructor to initialise an organisation object.
     */
    public Organisation() {
    }

    /**
     * A getter method to retrieve all available rooms, assets and their corresponding time series and information in the facilities managed by an organisation.
     * Format: {asset1: [measure1, dataIRI, timeseriesIRI, unit, assetType], [measure2, dataIRI, timeseriesIRI, null(if no unit), assetType]],
     * room1: [[measureName, dataIRI, timeseriesIRI, unit], [measureName, dataIRI, timeseriesIRI, unit]], ...],
     * facilities: [[facility1, asset1InFacility1,...],[facility2, room1InFacility2,...]]
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
        // Retrieve all facility data through the use of streams and collect them as a queue
        Queue<String[]> facilityDataQueue = this.FACILITIES.values()
                .stream()
                .map(Facility::getFacilityData)
                .collect(Collectors.toCollection(ArrayDeque::new));
        // If the queue has values, add the facility key
        if (!facilityDataQueue.isEmpty()) measures.put(StringHelper.FACILITY_KEY, facilityDataQueue);
        // Only add the thresholds if there are values
        if (!this.FACILITY_THRESHOLDS.isEmpty()) measures.put(StringHelper.THRESHOLD_KEY, this.FACILITY_THRESHOLDS);
        return measures;
    }

    /**
     * Add a room into this class.
     *
     * @param facilityName  Name of the facility that the asset is found in.
     * @param roomName      Name of the room to be included.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the room.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public void addRoom(String facilityName, String roomName, String measureName, String unit, String measureIri, String timeSeriesIri) {
        this.addFacilityItem(facilityName, roomName);
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
     * @param facilityName  Name of the facility that the asset is found in.
     * @param assetName     Name of the asset to be included.
     * @param assetType     Type of the asset to be included.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the asset.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public void addAsset(String facilityName, String assetName, String assetType, String measureName, String unit, String measureIri, String timeSeriesIri) {
        this.addFacilityItem(facilityName, assetName);
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
        // Verify if this threshold has already been added for the same measure
        if (!this.UNIQUE_THRESHOLDS.contains(String.join("", thresholdData))) {
            // If not, add it to the queue and the set
            this.FACILITY_THRESHOLDS.offer(thresholdData);
            this.UNIQUE_THRESHOLDS.add(String.join("", thresholdData));
        }
    }

    /**
     * Add the associated asset or room for the facility into the facility mapping.
     *
     * @param facilityName Name of the facility.
     * @param itemName     Name of the asset or room in the facility.
     */
    private void addFacilityItem(String facilityName, String itemName) {
        // Check if there is an existing facility
        if (this.FACILITIES.containsKey(facilityName)) {
            // If there is one, add the room name to the existing contents
            Facility facility = this.FACILITIES.get(facilityName);
            facility.addItem(itemName);
        } else {
            // If it does not exist, create a new facility object with the facility and room name
            Facility facility = new Facility(facilityName, itemName);
            this.FACILITIES.put(facilityName, facility);
        }
    }
}
