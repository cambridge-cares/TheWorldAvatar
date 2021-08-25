package org.linkeddatafragments.datasource;

/**
 * The base class for an {@link IDataSource}
 *
 * @author Miel Vander Sande
 * @author Bart Hanssens
 */
public abstract class DataSourceBase implements IDataSource {

    /**
     * Get the datasource title
     */
    protected String title;

    /**
     * Get the datasource description
     */
    protected String description; 
    
    /**
     * Create a base for a {@link IDataSource}
     *
     * @param title the datasource title
     * @param description the datasource description
     */
    public DataSourceBase(String title, String description) {
        this.title = title;
        this.description = description;
    }

    /**
     * Get the datasource description
     * 
     * @return
     */
    @Override
    public String getDescription() {
        return this.description;
    };

    /**
     * Get the datasource title
     * 
     * @return
     */
    @Override
    public String getTitle() {
        return this.title;
    };

    @Override
    public void close() {}
}
