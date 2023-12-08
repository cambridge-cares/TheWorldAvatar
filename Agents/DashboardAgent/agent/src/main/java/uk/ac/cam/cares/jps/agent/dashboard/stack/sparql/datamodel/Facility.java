package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import java.util.*;

/**
 * A class holding the required information to support the enforcement of the Organisation data model.
 * This class cannot be accessed outside the subpackage, and is intended to be a data model for holding facility information.
 *
 * @author qhouyee
 */
public class Facility {
    private final String facilityName;
    // Use a set to ensure unique items
    private final Set<String> roomsAndAssets = new HashSet<>();

    /**
     * Standard Constructor. This will store the facility name and its corresponding rooms or assets that is within the facility.
     *
     * @param facilityName     The name of the facility.
     * @param roomOrAssetName  The name of the room or asset found in the facility.
     */
    protected Facility(String facilityName, String roomOrAssetName) {
        this.facilityName = facilityName;
        this.roomsAndAssets.add(roomOrAssetName);
    }

    /**
     * Adds the name of a room or asset found within the same facility.
     *
     * @param roomOrAssetName  The name of the room or asset found in the facility.
     */
    protected void addItem(String roomOrAssetName) {
        this.roomsAndAssets.add(roomOrAssetName);
    }

    /**
     * A getter method for facility name.
     */
    protected String getFacilityName() {return this.facilityName;}

    /**
     * A getter method to retrieve all assets and rooms within this facility.
     *
     * @return An array containing the facility name in the first position, followed by the associated assets and rooms in the facility.
     */
    protected String[] getFacilityData() {
        // Initialise a new string array of the total count of rooms and assets and one extra slot for the facility name
        String[] metadata = new String[this.roomsAndAssets.size() + 1];
        // Add the facility name in the first position
        metadata[0] = this.getFacilityName();
        // Add all remaining items into the remaining slots of the array
        int counter = 1; // Use a counter to keep track of position
        for (String item : this.roomsAndAssets) {
            metadata[counter] = item;
            counter++;
        }
        return metadata;
    }
}
