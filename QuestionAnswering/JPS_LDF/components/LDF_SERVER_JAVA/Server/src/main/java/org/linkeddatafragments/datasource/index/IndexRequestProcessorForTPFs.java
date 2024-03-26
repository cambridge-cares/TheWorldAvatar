package org.linkeddatafragments.datasource.index;


import java.util.HashMap;
import java.util.Map;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.rdf.model.impl.PropertyImpl;
import org.apache.jena.rdf.model.impl.ResourceImpl;

import org.linkeddatafragments.datasource.AbstractRequestProcessorForTriplePatterns;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternElement;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;

/**
 * Implementation of {@link IFragmentRequestProcessor} that processes
 * {@link ITriplePatternFragmentRequest}s over an index that provides
 * an overview of all available datasets.
 *
 * @author Miel Vander Sande
 * @author <a href="http://olafhartig.de">Olaf Hartig</a>
 */
public class IndexRequestProcessorForTPFs
    extends AbstractRequestProcessorForTriplePatterns<RDFNode,String,String>
{
    final static String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    final static String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
    final static String DC = "http://purl.org/dc/terms/";
    final static String VOID = "http://rdfs.org/ns/void#";

    private final Model model;

    /**
     *
     * @param baseUrl
     * @param datasources
     */
    public IndexRequestProcessorForTPFs(
                               final String baseUrl,
                               final HashMap<String, IDataSource> datasources )
    {
        this.model = ModelFactory.createDefaultModel();

        for (Map.Entry<String, IDataSource> entry : datasources.entrySet()) {
            String datasourceName = entry.getKey();
            IDataSource datasource = entry.getValue();

            Resource datasourceUrl = new ResourceImpl(baseUrl + "/" + datasourceName);

            model.add(datasourceUrl, new PropertyImpl(RDF + "type"), new ResourceImpl(VOID + "Dataset"));
            model.add(datasourceUrl, new PropertyImpl(RDFS + "label"), datasource.getTitle());
            model.add(datasourceUrl, new PropertyImpl(DC + "title"), datasource.getTitle());
            model.add(datasourceUrl, new PropertyImpl(DC + "description"), datasource.getDescription());
        }
    }

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
     * Worker for the index
     */
    protected class Worker
       extends AbstractRequestProcessorForTriplePatterns.Worker<RDFNode,String,String>
    {

        /**
         * Creates a Worker for the datasource index
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
         * @param s
         * @param p
         * @param o
         * @param offset
         * @param limit
         * @return
         */
        @Override
        protected ILinkedDataFragment createFragment(
                           final ITriplePatternElement<RDFNode,String,String> s,
                           final ITriplePatternElement<RDFNode,String,String> p,
                           final ITriplePatternElement<RDFNode,String,String> o,
                           final long offset,
                           final long limit )
        {
            // FIXME: The following algorithm is incorrect for cases in which
            //        the requested triple pattern contains a specific variable
            //        multiple times;
            //        e.g., (?x foaf:knows ?x ) or (_:bn foaf:knows _:bn)
            // see https://github.com/LinkedDataFragments/Server.Java/issues/25

            final Resource subject   = s.isVariable() ? null
                                                      : s.asConstantTerm().asResource();
            final Property predicate = p.isVariable() ? null
                                                      : ResourceFactory.createProperty(p.asConstantTerm().asResource().getURI());
            final RDFNode object     = o.isVariable() ? null
                                                      : o.asConstantTerm();

            StmtIterator listStatements = model.listStatements(subject, predicate, object);
            Model result = ModelFactory.createDefaultModel();

            long index = 0;
            while (listStatements.hasNext() && index < offset) {
                listStatements.next();
                index++;
            }

            while (listStatements.hasNext() && index < (offset + limit)) {
                result.add(listStatements.next());
            }

            final boolean isLastPage = ( result.size() < offset + limit );
            return createTriplePatternFragment( result, result.size(), isLastPage );
        }

    } // end of class Worker

}
