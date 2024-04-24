package org.linkeddatafragments.datasource.tdb;

import java.io.File;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.QuerySolutionMap;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.Syntax;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.tdb.TDBFactory;

import org.linkeddatafragments.datasource.AbstractRequestProcessorForTriplePatterns;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternElement;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;

/**
 * Implementation of {@link IFragmentRequestProcessor} that processes
 * {@link ITriplePatternFragmentRequest}s over data stored in Jena TDB.
 *
 * @author <a href="mailto:bart.hanssens@fedict.be">Bart Hanssens</a>
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class JenaTDBBasedRequestProcessorForTPFs
    extends AbstractRequestProcessorForTriplePatterns<RDFNode,String,String>
{
    private final Dataset tdb;
    private final String defaultGraph;
    private final String sparql = "CONSTRUCT WHERE { ?s ?p ?o } " +
                                    "ORDER BY ?s ?p ?o";

    private final String count = "SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }";

    private final Query query = QueryFactory.create(sparql, Syntax.syntaxSPARQL_11);
    private final Query countQuery = QueryFactory.create(count, Syntax.syntaxSPARQL_11);

    /**
     *
     * @param request
     * @return
     * @throws IllegalArgumentException
     */
    @Override
    protected Worker getTPFSpecificWorker(
            final ITriplePatternFragmentRequest<RDFNode,String,String> request )
                                                throws IllegalArgumentException
    {
        return new Worker( request );
    }

    /**
     *
     */
    protected class Worker
       extends AbstractRequestProcessorForTriplePatterns.Worker<RDFNode,String,String>
    {

        /**
         *
         * @param req
         */
        public Worker(
                final ITriplePatternFragmentRequest<RDFNode,String,String> req )
        {
            super( req );
        }

        /**
         *
         * @param subject
         * @param predicate
         * @param object
         * @param offset
         * @param limit
         * @return
         */
        @Override
        protected ILinkedDataFragment createFragment(
                   final ITriplePatternElement<RDFNode,String,String> subject,
                   final ITriplePatternElement<RDFNode,String,String> predicate,
                   final ITriplePatternElement<RDFNode,String,String> object,
                   final long offset,
                   final long limit )
        {
            // FIXME: The following algorithm is incorrect for cases in which
            //        the requested triple pattern contains a specific variable
            //        multiple times;
            //        e.g., (?x foaf:knows ?x ) or (_:bn foaf:knows _:bn)
            // see https://github.com/LinkedDataFragments/Server.Java/issues/24

            Model model;
            if (defaultGraph == null) {
                model = tdb.getDefaultModel();
            } else {
                model = tdb.getNamedModel(defaultGraph);
            }

            QuerySolutionMap map = new QuerySolutionMap();
            if ( ! subject.isVariable() ) {
                map.add("s", subject.asConstantTerm());
            }
            if ( ! predicate.isVariable() ) {
                map.add("p", predicate.asConstantTerm());
            }
            if ( ! object.isVariable() ) {
                map.add("o", object.asConstantTerm());
            }

            query.setOffset(offset);
            query.setLimit(limit);

            Model triples = ModelFactory.createDefaultModel();

            try (QueryExecution qexec = QueryExecutionFactory.create(query, model, map)) {
                qexec.execConstruct(triples);
            }

            if (triples.isEmpty()) {
                return createEmptyTriplePatternFragment();
            }

            // Try to get an estimate
            long size = triples.size();
            long estimate = -1;

            try (QueryExecution qexec = QueryExecutionFactory.create(countQuery, model, map)) {
                ResultSet results = qexec.execSelect();
                if (results.hasNext()) {
                    QuerySolution soln = results.nextSolution() ;
                    Literal literal = soln.getLiteral("count");
                    estimate = literal.getLong();
                }
            }

            /*GraphStatisticsHandler stats = model.getGraph().getStatisticsHandler();
            if (stats != null) {
                Node s = (subject != null) ? subject.asNode() : null;
                Node p = (predicate != null) ? predicate.asNode() : null;
                Node o = (object != null) ? object.asNode() : null;
                estimate = stats.getStatistic(s, p, o);
            }*/

            // No estimate or incorrect
            if (estimate < offset + size) {
                estimate = (size == limit) ? offset + size + 1 : offset + size;
            }

            // create the fragment
            final boolean isLastPage = ( estimate < offset + limit );
            return createTriplePatternFragment( triples, estimate, isLastPage );
        }

    } // end of class Worker


    /**
     * Constructor
     *
     * @param tdbdir directory used for TDB backing
     */
    public JenaTDBBasedRequestProcessorForTPFs(File tdbdir) {
        this(tdbdir, null);
    }

    public JenaTDBBasedRequestProcessorForTPFs(File tdbdir, String defaultGraph) {
        this.defaultGraph = defaultGraph;
        this.tdb = TDBFactory.createDataset(tdbdir.getAbsolutePath());
    }
}
