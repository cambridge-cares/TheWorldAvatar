package org.linkeddatafragments.fragments.tpf;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.StmtIterator;



/**
 * Implementation of {@link ITriplePatternFragment}.
 *
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class TriplePatternFragmentImpl extends TriplePatternFragmentBase
{

    /**
     *
     */
    protected final Model triples;

    /**
     * Creates an empty Triple Pattern Fragment.
     * @param fragmentURL
     * @param datasetURL
     */
    public TriplePatternFragmentImpl( final String fragmentURL,
                                      final String datasetURL ) {
        this( null, 0L, fragmentURL, datasetURL, 1, true );
    }

    /**
     * Creates an empty Triple Pattern Fragment page.
     * @param fragmentURL
     * @param datasetURL
     * @param isLastPage
     * @param pageNumber
     */
    public TriplePatternFragmentImpl( final String fragmentURL,
                                      final String datasetURL,
                                      final long pageNumber,
                                      final boolean isLastPage ) {
        this( null, 0L, fragmentURL, datasetURL, pageNumber, isLastPage );
    }

    /**
     * Creates a new Triple Pattern Fragment.
     * @param triples the triples (possibly partial)
     * @param totalSize the total size
     * @param fragmentURL
     * @param datasetURL
     * @param isLastPage
     * @param pageNumber
     */
    public TriplePatternFragmentImpl( final Model triples,
                                      long totalSize,
                                      final String fragmentURL,
                                      final String datasetURL,
                                      final long pageNumber,
                                      final boolean isLastPage ) {
        super( totalSize, fragmentURL, datasetURL, pageNumber, isLastPage );
        this.triples = triples;
    }

    /**
     *
     * @return
     */
    @Override
    protected StmtIterator getNonEmptyStmtIterator() {
        return triples.listStatements();
    }

}
