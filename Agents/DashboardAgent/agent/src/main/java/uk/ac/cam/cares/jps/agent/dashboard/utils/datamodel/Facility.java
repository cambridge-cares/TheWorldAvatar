package uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel;

import java.util.*;

/**
 * A class storing the required information to display the time series of any Facility in the dashboard.
 *
 * @author qhouyee
 */
public class Facility {

    private final Map<String, Asset> ASSETS = new HashMap<>();

    /**
     * Standard Constructor to initialise a facility object with one asset and measure.
     *
     * @param assetName Name of the asset to be included.
     * @param assetType Type of the asset to be included.
     */
    public Facility(String assetName, String assetType) {
        addAsset(assetName, assetType);
    }

    /**
     * A getter method to retrieve all the assets available in the facility.
     *
     * @return An array of all available assets.
     */
    public String[] getAllAssets() {
        Set<String> assets = this.ASSETS.keySet();
        return assets.toArray(new String[assets.size()]);
    }

    /**
     * Add an asset into this class.
     *
     * @param assetName Name of the asset to be included.
     * @param assetType Type of the asset to be included.
     */
    public void addAsset(String assetName, String assetType) {
        // Check if the asset already exists in the map using its name as a key
        if (!this.ASSETS.containsKey(assetName)) {
            // If it does not exist, create a new asset and add it into the map
            Asset element = new Asset(assetType);
            this.ASSETS.put(assetName, element);
        }
        // else do nothing
    }
}
