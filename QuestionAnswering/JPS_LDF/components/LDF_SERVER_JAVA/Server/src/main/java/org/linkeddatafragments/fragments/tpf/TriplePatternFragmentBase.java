package org.linkeddatafragments.fragments.tpf;

import java.util.NoSuchElementException;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.NiceIterator;

import org.linkeddatafragments.fragments.LinkedDataFragmentBase;
import org.linkeddatafragments.util.CommonResources;



/**
 * Base class for implementations of {@link ITriplePatternFragment}.
 *
 * @author Ruben Verborgh
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
abstract public class TriplePatternFragmentBase extends LinkedDataFragmentBase
                                       implements ITriplePatternFragment
{
    private final long totalSize;

    /**
     * Creates an empty Triple Pattern Fragment.
     * @param fragmentURL
     * @param datasetURL
     */
    public TriplePatternFragmentBase( final String fragmentURL,
                                      final String datasetURL ) {
        this( 0L, fragmentURL, datasetURL, 1, true );
    }

    /**
     * Creates an empty Triple Pattern Fragment page.
     * @param fragmentURL
     * @param isLastPage
     * @param datasetURL
     * @param pageNumber
     */
    public TriplePatternFragmentBase( final String fragmentURL,
                                      final String datasetURL,
                                      final long pageNumber,
                                      final boolean isLastPage ) {
        this( 0L, fragmentURL, datasetURL, pageNumber, isLastPage );
    }

    /**
     * Creates a new Triple Pattern Fragment.
     * @param totalSize the total size
     * @param fragmentURL
     * @param datasetURL
     * @param pageNumber
     * @param isLastPage
     */
    public TriplePatternFragmentBase( long totalSize,
                                      final String fragmentURL,
                                      final String datasetURL,
                                      final long pageNumber,
                                      final boolean isLastPage ) {
        super( fragmentURL, datasetURL, pageNumber, isLastPage );
        this.totalSize = totalSize < 0L ? 0L : totalSize;
    }

    @Override
    public StmtIterator getTriples() {
        if ( totalSize == 0L )
            return emptyStmtIterator;
        else
            return getNonEmptyStmtIterator();
    }

    /**
     *
     * @return
     */
    abstract protected StmtIterator getNonEmptyStmtIterator();

    @Override
    public long getTotalSize() {
        return totalSize;
    }

    @Override
    public void addMetadata( final Model model )
    {
        super.addMetadata( model );

        final Resource fragmentId = model.createResource( fragmentURL );

        final Literal totalTyped = model.createTypedLiteral( totalSize,
                                                      XSDDatatype.XSDinteger );
        final Literal limitTyped = model.createTypedLiteral( getMaxPageSize(),
                                                      XSDDatatype.XSDinteger );

        fragmentId.addLiteral( CommonResources.VOID_TRIPLES, totalTyped );
        fragmentId.addLiteral( CommonResources.HYDRA_TOTALITEMS, totalTyped );
        fragmentId.addLiteral( CommonResources.HYDRA_ITEMSPERPAGE, limitTyped );
    }

    @Override
    public void addControls( final Model model )
    {
        super.addControls( model );

        final Resource datasetId = model.createResource( getDatasetURI() );

        final Resource triplePattern = model.createResource();
        final Resource subjectMapping = model.createResource();
        final Resource predicateMapping = model.createResource();
        final Resource objectMapping = model.createResource();

        datasetId.addProperty( CommonResources.HYDRA_SEARCH, triplePattern );

        triplePattern.addProperty( CommonResources.HYDRA_TEMPLATE, getTemplate() );
        triplePattern.addProperty( CommonResources.HYDRA_MAPPING, subjectMapping );
        triplePattern.addProperty( CommonResources.HYDRA_MAPPING, predicateMapping );
        triplePattern.addProperty( CommonResources.HYDRA_MAPPING, objectMapping );

        subjectMapping.addProperty( CommonResources.HYDRA_VARIABLE, ITriplePatternFragmentRequest.PARAMETERNAME_SUBJ );
        subjectMapping.addProperty( CommonResources.HYDRA_PROPERTY, CommonResources.RDF_SUBJECT );

        predicateMapping.addProperty( CommonResources.HYDRA_VARIABLE, ITriplePatternFragmentRequest.PARAMETERNAME_PRED );
        predicateMapping.addProperty( CommonResources.HYDRA_PROPERTY, CommonResources.RDF_PREDICATE );
        
        objectMapping.addProperty( CommonResources.HYDRA_VARIABLE, ITriplePatternFragmentRequest.PARAMETERNAME_OBJ );
        objectMapping.addProperty( CommonResources.HYDRA_PROPERTY, CommonResources.RDF_OBJECT );
    }

    /**
     *
     * @return
     */
    public String getTemplate() {
        return datasetURL + "{?" +
               ITriplePatternFragmentRequest.PARAMETERNAME_SUBJ + "," +
               ITriplePatternFragmentRequest.PARAMETERNAME_PRED + "," +
               ITriplePatternFragmentRequest.PARAMETERNAME_OBJ + "}";
    }

    /**
     *
     */
    public static final StmtIterator emptyStmtIterator = new EmptyStmtIterator();

    /**
     *
     */
    public static class EmptyStmtIterator
        extends NiceIterator<Statement>
        implements StmtIterator
    {

        /**
         *
         * @return
         */
        public Statement nextStatement() { throw new NoSuchElementException(); }
    }

}
