package uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel;

/**
 * A class holding the required information to support the enforcement of the Facility data model.
 * This class cannot be accessed outside the subpackage, and is intended to be a data model for holding asset information.
 *
 * @author qhouyee
 */
public class Asset {
    private final String ASSET_TYPE;

    /**
     * Standard Constructor. This will store the metadata retrieved from the SPARQL query.
     *
     * @param assetType The asset type.
     */
    protected Asset(String assetType) {
        this.ASSET_TYPE = assetType;
    }

    /**
     * A getter method for asset type.
     */
    protected String getAssetType() {
        return this.ASSET_TYPE;
    }
}
