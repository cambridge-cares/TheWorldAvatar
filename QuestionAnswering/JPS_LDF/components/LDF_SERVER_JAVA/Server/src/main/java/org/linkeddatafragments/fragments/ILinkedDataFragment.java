package org.linkeddatafragments.fragments;

import java.io.Closeable;
import org.apache.jena.rdf.model.StmtIterator;

/**
 * Represents any possible Linked Data Fragment.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public interface ILinkedDataFragment extends Closeable
{
    /**
     * Returns an iterator over the RDF data of this fragment (possibly only
     * partial if the data is paged, as indicated by {@link #isPageOnly()}).
     * @return 
     */
    StmtIterator getTriples();

    /**
     * Returns true if {@link #getTriples()} returns a page of data only.
     * In this case, {@link #getPageNumber()} can be used to obtain the
     * corresponding page number.
     * @return 
     */
    boolean isPageOnly();

    /**
     * Returns the number of the page of data returned by {@link #getTriples()}
     * if the data is paged (that is, if {@link #isPageOnly()} returns true).
     *
     * If the data is not paged, this method throws an exception.
     *
     * @return 
     * @throws UnsupportedOperationException
     *         If the data of this fragment is not paged. 
     */
    long getPageNumber() throws UnsupportedOperationException;

    /**
     * Returns true if {@link #getTriples()} returns a page of data only and
     * this is the last page of the fragment.
     *
     * If the data is not paged (i.e., if {@link #isPageOnly()} returns false),
     * this method throws an exception.
     *
     * @return 
     * @throws UnsupportedOperationException
     *         If the data of this fragment is not paged. 
     */
    boolean isLastPage() throws UnsupportedOperationException;

    /**
     * Returns the maximum number of triples per page if {@link #getTriples()}
     * returns a page of data only (that is, if {@link #isPageOnly()} returns
     * true).
     *
     * If the data is not paged, this method throws an exception.
     *
     * @return 
     * @throws UnsupportedOperationException
     *         If the data of this fragment is not paged. 
     */
    long getMaxPageSize() throws UnsupportedOperationException;

    /**
     * Returns an iterator over the metadata of this fragment.
     * @return 
     */
    StmtIterator getMetadata();

    /**
     * Returns an iterator over an RDF description of the controls associated
     * with this fragment.
     * @return 
     */
    StmtIterator getControls();
}
