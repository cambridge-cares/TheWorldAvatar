package org.linkeddatafragments.datasource.sparql;

import java.net.URI;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;
import org.apache.jena.query.ParameterizedSparqlString;
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

import org.linkeddatafragments.datasource.AbstractRequestProcessorForTriplePatterns;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternElement;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;

/**
 * Implementation of {@link IFragmentRequestProcessor} that processes
 * {@link ITriplePatternFragmentRequest}s over data stored behind a SPARQL-Query endpoint.
 *
 * @author <a href="mailto:bart.hanssens@fedict.be">Bart Hanssens</a>
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class SparqlBasedRequestProcessorForTPFs
    extends AbstractRequestProcessorForTriplePatterns<RDFNode,String,String>
{
    private final URI endpointURI;
    private final String username;
    private final String password;

    private final String sparql = "CONSTRUCT WHERE { ?s ?p ?o } ";

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

            // Build the SPARQL-endpoint
            URIBuilder uriBuilder = new URIBuilder(endpointURI);
            addCredentials(uriBuilder);

            final String endpoint;
            try {
                endpoint = uriBuilder.build().toString();
            } catch (URISyntaxException e) {
                throw new RuntimeException(e);
            }

            ParameterizedSparqlString queryWithParams = new ParameterizedSparqlString(query.serialize(), map);

            try (QueryExecution qexec = QueryExecutionFactory.sparqlService(endpoint, queryWithParams.asQuery())) {
                qexec.execConstruct(triples);
            }

            if (triples.isEmpty()) {
                return createEmptyTriplePatternFragment();
            }

            // Try to get an estimate
            long size = triples.size();
            long estimate = -1;

            ParameterizedSparqlString countQueryWithParams = new ParameterizedSparqlString(countQuery.serialize(), map);

            try (QueryExecution qexec = QueryExecutionFactory.createServiceRequest(endpoint, countQueryWithParams.asQuery())) {
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

        /**
         * This method adds 'email' and 'password' parameters to the provided URIBuilder
         * Note: This credentials approach is very specific to the VIVO (https://github.com/vivo-project/VIVO)
         * application and should be refactored once another use-case/example is required.
         *
         * @param uriBuilder of SPARQL-Query endpoint
         */
        private void addCredentials(URIBuilder uriBuilder) {
            if (username != null && password != null) {
                uriBuilder.addParameter("email", username);
                uriBuilder.addParameter("password", password);
            }
        }

    } // end of class Worker


    /**
     * Constructor
     *
     * @param endpointURI of SPARQL-Query service
     * @param username for SPARQL-Query service
     * @param password for SPARQL-Query service
     */
    public SparqlBasedRequestProcessorForTPFs(URI endpointURI, String username, String password) {
        this.endpointURI = endpointURI;
        this.username = username;
        this.password = password;
    }
}
