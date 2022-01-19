package org.linkeddatafragments.test.datasource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.junit.Assert;
import org.junit.Test;
import org.linkeddatafragments.datasource.IDataSource;
import org.linkeddatafragments.datasource.IFragmentRequestProcessor;
import org.linkeddatafragments.datasource.index.IndexRequestProcessorForTPFs;
import org.linkeddatafragments.fragments.IFragmentRequestParser;
import org.linkeddatafragments.fragments.ILinkedDataFragment;
import org.linkeddatafragments.fragments.tpf.ITriplePatternFragmentRequest;
import org.linkeddatafragments.fragments.tpf.TriplePatternElementFactory;
import org.linkeddatafragments.fragments.tpf.TriplePatternFragmentRequestImpl;

/**
 * Test cases for the IndexRequestProcessorForTPFs
 *
 * @author Lars G. Svensson
 */
public class IndexRequestProcessorForTPFsTest {

	static class TestIndexRequestProcessor extends IndexRequestProcessorForTPFs {
		TestIndexRequestProcessor(final String baseUrl,
				final HashMap<String, IDataSource> datasources) {
			super(baseUrl, datasources);
		}

		@Override
		protected TestWorker getTPFSpecificWorker(
				final ITriplePatternFragmentRequest<RDFNode, String, String> request)
				throws IllegalArgumentException {
			return new TestWorker(request);
		}

		class TestWorker extends IndexRequestProcessorForTPFs.Worker {
			public TestWorker(
					final ITriplePatternFragmentRequest<RDFNode, String, String> req) {
				super(req);
			}
		}
	}

	/**
	 * Check that the type void:Dataset is returned as a URI and not as a
	 * literal
	 */
	@Test
	public void shouldGiveVoidDatasetAsAURI() {
		final IDataSource datasource = new IDataSource() {
			@Override
			public String getDescription() {
				return "This is a dummy datasource";
			};

			@Override
			public IFragmentRequestParser getRequestParser() {
				return null;
			}

			@Override
			public IFragmentRequestProcessor getRequestProcessor() {
				return null;
			}

			@Override
			public String getTitle() {
				return "Dummy Dataource";
			}

			@Override
			public void close() {
				// does nothing
			}
		};
		final HashMap<String, IDataSource> datasources = new HashMap<String, IDataSource>();
		datasources.put("dummy", datasource);
		final String baseUrl = "dummy";
		final TestIndexRequestProcessor processor = new TestIndexRequestProcessor(
				baseUrl, datasources);
		final TriplePatternElementFactory<RDFNode, String, String> factory = new TriplePatternElementFactory<RDFNode, String, String>();
		final ITriplePatternFragmentRequest<RDFNode, String, String> request = new TriplePatternFragmentRequestImpl<RDFNode, String, String>(
				null, "dummy", false, 1, factory.createUnspecifiedVariable(),
				factory.createUnspecifiedVariable(),
				factory.createUnspecifiedVariable());
		final TestIndexRequestProcessor.TestWorker worker = processor
				.getTPFSpecificWorker(request);
		final ILinkedDataFragment fragment = worker.createRequestedFragment();
		final StmtIterator iterator = fragment.getTriples();
		final Collection<Statement> statements = new ArrayList<Statement>();
		while (iterator.hasNext()) {
			statements.add(iterator.next());
		}

		final Resource subject = ResourceFactory.createResource("dummy/dummy");
		final Property predicate = ResourceFactory
				.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
		final Resource object = ResourceFactory
				.createResource("http://rdfs.org/ns/void#Dataset");
		final Statement expected = ResourceFactory.createStatement(subject,
				predicate, object);
		Assert.assertTrue("triple not contained in model",
				statements.contains(expected));
		processor.close();
	}
}
