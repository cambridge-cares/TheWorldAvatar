package uk.ac.cam.cares.jps.ontomatch.streamRDFs;

import java.io.FileInputStream;

import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.other.BatchedStreamRDF;
import org.apache.jena.riot.system.StreamRDF;

import io.herrmann.generator.Generator;

/****
 * memory-efficient generator for stream reading RDF
 * @author shaocong
 *
 */
public class AlignmentGenerator {

	public static Generator<String[]> getGenerator(String uri){
		return new Generator<String[]>() {
		    public void run() throws InterruptedException {
				FunctionInterface ref;
				ref = values -> {
					try {
						yield(values);
					} catch (InterruptedException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				};  
				StreamAlignmentHandler a = new StreamAlignmentHandler(ref);
				StreamRDF stream = new BatchedStreamRDF(a);
		        try {
					FileInputStream fin=new FileInputStream(uri);
					RDFDataMgr.parse(stream, fin,Lang.TTL);
				}catch(Exception e) {
				e.printStackTrace();
				}
		    }
		};
	}
		
}
