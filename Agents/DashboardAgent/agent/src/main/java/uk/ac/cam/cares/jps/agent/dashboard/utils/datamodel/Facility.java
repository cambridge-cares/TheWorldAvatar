package uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel;

import java.util.*;

/**
 * A class storing the required information to display the time series of any Facility in the dashboard.
 *
 * @author qhouyee
 */
public class Facility {
    private final Set<String> TIME_SERIES_MEASURES = new HashSet<>();
    // Key value pair is asset name and its stored information respectively
    private final Map<String, Asset> ASSETS = new HashMap<>();

    /**
     * Standard Constructor to initialise a facility object with one asset and measure.
     *
     * @param assetName     Name of the asset to be included.
     * @param assetType     Type of the asset to be included.
     * @param measureName   Name of the measure associated with the asset.
     * @param measureIri    Corresponding dataIRI of the measure associated with the asset.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    public Facility(String assetName, String assetType, String measureName, String measureIri, String timeSeriesIri) {
        addAsset(assetName, assetType, measureName, measureIri, timeSeriesIri);
    }

    /**
     * A getter method to retrieve all the assets available in the facility.
     *
     * @return A map of {assetType: [asset1, asset2, ...]} categorising each asset to their type.
     */
    public Map<String, List<String>> getAllAssets() {
        Map<String, List<String>> assets = new HashMap<>();
        // For all assets, store them in a map with their asset type as a key and individual asset names as values
        for (Asset asset : this.ASSETS.values()) {
            String assetType = asset.getAssetType();
            // If the asset type has preceding values
            if (assets.containsKey(assetType)) {
                // Simply add the value to the current list
                List<String> tempList = assets.get(assetType);
                tempList.add(asset.getAssetName());
            } else {
                // If the asset type does not have preceding values, create a new list with the current asset name
                List<String> tempList = new ArrayList<>();
                tempList.add(asset.getAssetName());
                // Map asset types to their specific assets
                assets.put(assetType, tempList);
            }
        }
        return assets;
    }

    /**
     * A getter method to retrieve all available measures and their corresponding assets in the facility.
     * Format: {asset1: [measure1, dataIRI, timeseriesIRI], [measure2, dataIRI, timeseriesIRI]], asset2: [[measureName, dataIRI, timeseriesIRI]], ...]}
     *
     * @return A map linking all assets to their measures.
     */
    public Map<String, Queue<String[]>> getAllMeasures() {
        // For all assets, store them in a map with their asset type as a key and individual asset names as values
        Map<String, Queue<String[]>> assetMeasures = new HashMap<>();
        for (Asset asset : this.ASSETS.values()) {
            String assetName = asset.getAssetName();
            assetMeasures.put(assetName, asset.getMeasureInfo());
        }
        return assetMeasures;
    }

    /**
     * Add an asset into this class.
     *
     * @param assetName Name of the asset to be included.
     * @param assetType Type of the asset to be included.
     */
    public void addAsset(String assetName, String assetType, String measureName, String measureIri, String timeSeriesIri) {
        // Check if the asset already exists in the map using its name as a key
        if (this.ASSETS.containsKey(assetName)) {
            // If there is a preceding asset object, add only the measure to the right asset
            Asset asset = this.ASSETS.get(assetName);
            asset.addMeasure(measureName, measureIri, timeSeriesIri);
        } else {
            // If it does not exist, create a new asset and add it into the map
            Asset element = new Asset(assetName, assetType, measureName, measureIri, timeSeriesIri);
            this.ASSETS.put(assetName, element);
        }
        // Store all measure names as a set, which only allows unique values
        this.TIME_SERIES_MEASURES.add(measureName);
    }
}
