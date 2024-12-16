package org.linkeddatafragments.datasource;

import java.io.Closeable;

import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;
import org.linkeddatafragments.fragments.ILinkedDataFragmentRequest;

/**
 * Processes {@link ILinkedDataFragmentRequest}s and returns
 * the requested {@link ILinkedDataFragment}s.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public interface IFragmentRequestProcessor extends Closeable
{

    /**
     *
     * @param request
     * @return
     * @throws IllegalArgumentException
     */
    ILinkedDataFragment createRequestedFragment(
            final ILinkedDataFragmentRequest request )
                    throws IllegalArgumentException;
}
