package uk.ac.cam.cares.jps.ontomatch.streamRDFs;

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.riot.other.StreamRDFBatchHandler;
import org.apache.jena.sparql.core.Quad;

import uk.ac.cam.cares.jps.ontomatch.alignment.AlignmentNamespace;

@FunctionalInterface 
interface FunctionInterface{  
   public void call(String[] values);  
}  

/***
 * Stream handler for alignment file, which extracts alignment info from an alignment file by batches of triples
 * @author shaocong
 *
 */
public class StreamAlignmentHandler implements StreamRDFBatchHandler{
	private FunctionInterface yield;
	public StreamAlignmentHandler(Object yieldO) {
		// TODO Auto-generated constructor stub
		this.yield = (FunctionInterface) yieldO;
	}

	@Override
	public void base(String arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void batchQuads(Node arg0, Node arg1, List<Quad> arg2) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void batchTriples(Node subject, List<Triple> triples) {
		// TODO Auto-generated method stub
		String entity1 = null, entity2 = null, measure = null;
		for(Triple t : triples) {
			if(t.getPredicate().getURI().equals(AlignmentNamespace.fullIRI(AlignmentNamespace.ENTITY1))) {
				entity1 = t.getObject().toString();
			} else if(t.getPredicate().getURI().equals(AlignmentNamespace.fullIRI(AlignmentNamespace.ENTITY2))){
				entity2 =  t.getObject().toString(); 
			}else if(t.getPredicate().getURI().equals(AlignmentNamespace.fullIRI(AlignmentNamespace.MEASURE))) {
				measure = t.getObject().toString();
			}
		}
		if(entity1 != null && entity2 !=null && measure !=null) {
			String[] newvalues  = {entity1, entity2, measure};
			//we need to output here
			this.yield.call(newvalues);
		}
	}

	@Override
	public void finish() {
		// TODO Auto-generated method stub
	}

	@Override
	public void prefix(String arg0, String arg1) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void start() {
		// TODO Auto-generated method stub

	}


}
