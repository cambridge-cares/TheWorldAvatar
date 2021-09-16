package org.linkeddatafragments.views;

import java.util.HashMap;
import java.util.Map;
import org.linkeddatafragments.datasource.IDataSource;

/**
 * Base class of any implementation of {@link ILinkedDataFragmentWriter}.
 *
 * @author Miel Vander Sande
 */
public abstract class LinkedDataFragmentWriterBase implements ILinkedDataFragmentWriter {
    private final Map<String, String> prefixes;
    private final HashMap<String, IDataSource> datasources;

    /**
     *
     * @param prefixes
     * @param datasources
     */
    public LinkedDataFragmentWriterBase(Map<String, String> prefixes, HashMap<String, IDataSource> datasources) {
        this.prefixes = prefixes;
        this.datasources = datasources;
    }

    /**
     *
     * @return
     */
    public Map<String, String> getPrefixes() {
        return prefixes;
    }

    /**
     *
     * @return
     */
    public HashMap<String, IDataSource> getDatasources() {
        return datasources;
    }
}
