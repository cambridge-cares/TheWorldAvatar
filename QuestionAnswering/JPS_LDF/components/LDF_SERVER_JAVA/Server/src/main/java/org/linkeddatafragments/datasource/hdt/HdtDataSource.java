package org.linkeddatafragments.datasource.hdt;

import java.io.IOException;

import org.linkeddatafragments.datasource.DataSourceBase;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.fragments.IFragmentRequestParser;
import org.linkeddatafragments.fragments.tpf.TPFRequestParserForJenaBackends;

/**
 * An HDT data source of Basic Linked Data Fragments.
 *
 * @author Ruben Verborgh
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class HdtDataSource extends DataSourceBase {

    /**
     * The request processor
     * 
     */
    protected final HdtBasedRequestProcessorForTPFs requestProcessor;

    /**
     * Creates a new HdtDataSource.
     *
     * @param title title of the datasource
     * @param description datasource description
     * @param hdtFile the HDT datafile
     * @throws IOException if the file cannot be loaded
     */
    public HdtDataSource(String title, String description, String hdtFile) throws IOException {
        super(title, description);
        requestProcessor = new HdtBasedRequestProcessorForTPFs( hdtFile );
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
