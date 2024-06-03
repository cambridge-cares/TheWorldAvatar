package org.linkeddatafragments.datasource;

import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;

/**
 * Base class for implementations of {@link IFragmentRequestProcessor}.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
abstract public class AbstractRequestProcessor
    implements IFragmentRequestProcessor
{ 
    @Override
    public void close() {}

    /**
     * Create an {@link ILinkedDataFragment} from {@link ILinkedDataFragmentRequest}
     *
     * @param request
     * @return
     * @throws IllegalArgumentException
     */
    @Override
    final public ILinkedDataFragment createRequestedFragment(
            final ILinkedDataFragmentRequest request )
                    throws IllegalArgumentException
    {
        return getWorker( request ).createRequestedFragment();
    }

    /**
     * Get the {@link Worker} from {@link ILinkedDataFragmentRequest}
     *
     * @param request
     * @return
     * @throws IllegalArgumentException
     */
    abstract protected Worker getWorker(
            final ILinkedDataFragmentRequest request )
                    throws IllegalArgumentException;

    /**
     * Processes {@link ILinkedDataFragmentRequest}s
     * 
     */
    abstract static protected class Worker
    {

        /**
         * The  {@link ILinkedDataFragmentRequest} to process
         */
        public final ILinkedDataFragmentRequest request;
        
        /**
         * Create a Worker
         * 
         * @param request
         */
        public Worker( final ILinkedDataFragmentRequest request )
        {
            this.request = request;
        }

        /**
         * Create the requested {@link ILinkedDataFragment}
         * 
         * @return The ILinkedDataFragment
         * @throws IllegalArgumentException
         */
        abstract public ILinkedDataFragment createRequestedFragment()
                                               throws IllegalArgumentException;

    } // end of class Worker

}
