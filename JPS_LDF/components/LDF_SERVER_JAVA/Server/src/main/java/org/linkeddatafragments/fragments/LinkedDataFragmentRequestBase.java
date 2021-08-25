package org.linkeddatafragments.fragments;

/**
 * Base class for implementations of {@link ILinkedDataFragmentRequest}.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public abstract class LinkedDataFragmentRequestBase
    implements ILinkedDataFragmentRequest
{

    /**
     *
     */
    public final String fragmentURL;

    /**
     *
     */
    public final String datasetURL;

    /**
     *
     */
    public final boolean pageNumberWasRequested;

    /**
     *
     */
    public final long pageNumber;
    
    /**
     *
     * @param fragmentURL
     * @param datasetURL
     * @param pageNumberWasRequested
     * @param pageNumber
     */
    public LinkedDataFragmentRequestBase( final String fragmentURL,
                                          final String datasetURL,
                                          final boolean pageNumberWasRequested,
                                          final long pageNumber )
    {
        this.fragmentURL = fragmentURL;
        this.datasetURL = datasetURL;
        this.pageNumberWasRequested = pageNumberWasRequested;
        this.pageNumber = (pageNumberWasRequested) ? pageNumber : 1L;
    }

    @Override
    public String getFragmentURL() {
        return fragmentURL;
    }

    @Override
    public String getDatasetURL() {
        return datasetURL;
    }

    @Override
    public boolean isPageRequest() {
        return pageNumberWasRequested;
    }

    @Override
    public long getPageNumber() {
        return pageNumber;
    }

    @Override
    public String toString()
    {
        return "LinkedDataFragmentRequest(" +
               "class: " + getClass().getName() +
               ", fragmentURL: " + fragmentURL +
               ", isPageRequest: " + pageNumberWasRequested +
               ", pageNumber: " + pageNumber +
               ")";
    }

}
