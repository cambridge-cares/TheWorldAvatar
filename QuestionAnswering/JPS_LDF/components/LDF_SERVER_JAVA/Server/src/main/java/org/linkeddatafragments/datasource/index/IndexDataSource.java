package org.linkeddatafragments.datasource.index;

import java.util.HashMap;

import org.linkeddatafragments.datasource.DataSourceBase;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.fragments.IFragmentRequestParser;
import org.linkeddatafragments.fragments.tpf.TPFRequestParserForJenaBackends;

/**
 * An Index data source provides an overview of all available datasets.
 *
 * @author Miel Vander Sande
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class IndexDataSource extends DataSourceBase {

    /**
     * The request processor
     * 
     */
    protected final IndexRequestProcessorForTPFs requestProcessor;

    /**
     *
     * @param baseUrl
     * @param datasources
     */
    public IndexDataSource(String baseUrl, HashMap<String, IDataSource> datasources) {
        super("Index", "List of all datasources");
        requestProcessor = new IndexRequestProcessorForTPFs( baseUrl, datasources );
    }

    @Override
    public IFragmentRequestParser getRequestParser()
    {
        return TPFRequestParserForJenaBackends.getInstance();
    }

    @Override
    public IFragmentRequestProcessor getRequestProcessor()
    {
        return requestProcessor;
    }

}
