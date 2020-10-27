package uk.ac.cam.cares.jps.base.query;

import java.util.HashSet;
import java.util.Set;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.riot.system.StreamRDF;
import org.apache.jena.sparql.core.Quad;

/* Copied from:
 * https://github.com/apache/jena/blob/master/jena-examples/src/main/java/org/apache/jena/example/streaming/StreamRDFToConnection.java#L47
 */
public class StreamRDFToConnection implements StreamRDF {

	private RDFConnection connection;
	private int bufferSize = 1000;
	private Set<Quad> quads = new HashSet<Quad>();
	private Model model = ModelFactory.createMemModelMaker().createFreshModel();
	
	/**
	 * Constructs the StreamRDFToConnection using default 1000 quad buffer size.
	 * @param connection the connection to talk to.
	 */
	public StreamRDFToConnection( RDFConnection connection ) {
		this.connection = connection;
	}
	
	/**
	 * Constructs the StreamRDFToConnection with the specified buffer size
	 * @param connection the connection to talk to.
	 * @param bufferSize the buffersize.
	 */
	public StreamRDFToConnection( RDFConnection connection, int bufferSize ) {
		this.connection = connection;
		this.bufferSize = bufferSize;
	}
	
	/**
	 * See if we should flush the buffer.
	 */
	private void isBufferFull() {
		if (model.size() + quads.size() >= bufferSize)
		{
			flush();
		}
	}
	
	/**
	 * Flushes the buffer to the connection.
	 */
	private void flush() {
		UpdateBuilder builder = new UpdateBuilder();
		builder.addPrefixes( model );
		builder.addInsert( model );
		builder.addInsertQuads( quads );
		connection.begin( TxnType.WRITE );
		connection.update( builder.build() );
		connection.commit();
		model.removeAll();
		quads.clear();
	}
	
	@Override
	public void start() {
		// does nothing.
	}
	@Override
	public void triple(Triple triple) {
		model.add( model.asStatement(triple));
		isBufferFull();
	}
	@Override
	public void quad(Quad quad) {
		quads.add(quad);
		isBufferFull();
	}
	@Override
	public void base(String base) {
		// do nothing
	}
	@Override
	public void prefix(String prefix, String iri) {
		model.setNsPrefix(prefix, iri);
	}
	@Override
	public void finish() {
		flush();
	}
	
}
