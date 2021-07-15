package org.linkeddatafragments.fragments.tpf;

import org.linkeddatafragments.fragments.LinkedDataFragmentRequestBase;

/**
 * An implementation of {@link ITriplePatternFragmentRequest}.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 * @param <CTT>
 * @param <NVT>
 * @param <AVT>
 */
public class TriplePatternFragmentRequestImpl<CTT,NVT,AVT>
    extends LinkedDataFragmentRequestBase
    implements ITriplePatternFragmentRequest<CTT,NVT,AVT>
{

    /**
     *
     */
    public final ITriplePatternElement<CTT,NVT,AVT> subject;

    /**
     *
     */
    public final ITriplePatternElement<CTT,NVT,AVT> predicate;

    /**
     *
     */
    public final ITriplePatternElement<CTT,NVT,AVT> object;

    /**
     *
     * @param fragmentURL
     * @param datasetURL
     * @param pageNumberWasRequested
     * @param pageNumber
     * @param subject
     * @param predicate
     * @param object
     */
    public TriplePatternFragmentRequestImpl( final String fragmentURL,
                                             final String datasetURL,
                                             final boolean pageNumberWasRequested,
                                             final long pageNumber,
                                             final ITriplePatternElement<CTT,NVT,AVT> subject,
                                             final ITriplePatternElement<CTT,NVT,AVT> predicate,
                                             final ITriplePatternElement<CTT,NVT,AVT> object )
    {
        super( fragmentURL, datasetURL, pageNumberWasRequested, pageNumber );

        if ( subject == null )
            throw new IllegalArgumentException();

        if ( predicate == null )
            throw new IllegalArgumentException();

        if ( object == null )
            throw new IllegalArgumentException();

        this.subject = subject;
        this.predicate = predicate;
        this.object = object;
    }

    @Override
    public ITriplePatternElement<CTT,NVT,AVT> getSubject() {
        return subject;
    }

    @Override
    public ITriplePatternElement<CTT,NVT,AVT> getPredicate() {
        return predicate;
    }

    @Override
    public ITriplePatternElement<CTT,NVT,AVT> getObject() {
        return object;
    }

    @Override
    public String toString()
    {
        return "TriplePatternFragmentRequest(" +
               "class: " + getClass().getName() +
               ", subject: " + subject.toString() +
               ", predicate: " + predicate.toString() +
               ", object: " + object.toString() +
               ", fragmentURL: " + fragmentURL +
               ", isPageRequest: " + pageNumberWasRequested +
               ", pageNumber: " + pageNumber +
               ")";
    }

}
