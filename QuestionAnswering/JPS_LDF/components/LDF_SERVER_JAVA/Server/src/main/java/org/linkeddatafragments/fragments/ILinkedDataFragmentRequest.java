package org.linkeddatafragments.fragments;

/**
 * Basis for representing a request of some type of Linked Data Fragment (LDF).
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public interface ILinkedDataFragmentRequest
{

    /**
     *
     */
	// page limit setting 
    public final static long TRIPLESPERPAGE = 20000L;

    /**
     *
     */
    public final static String PARAMETERNAME_PAGE = "page";

    /**
     * Returns the URL of the requested LDF.
     * @return 
     */
    String getFragmentURL();

    /**
     * Returns the URL of the dataset to which the requested LDF belongs.
     * @return 
     */
    String getDatasetURL();

    /**
     * Returns true if the request is for a specific page of the requested
     * fragment. In this case, {@link #getPageNumber()} can be used to obtain
     * the requested page number.
     * @return 
     */
    boolean isPageRequest();

    /**
     * Returns the number of the page requested for the LDF; if this is not a
     * page-based request (that is, if {@link #isPageRequest()} returns true),
     * then this method returns 1. 
     * @return 
     */
    long getPageNumber();
}
