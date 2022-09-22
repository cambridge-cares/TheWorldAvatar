package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.WhereBuilder;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;

/**
 * The purpose of this class is to use the tagging cloning tool 
 * to clone from file based stores to other stores 
 * Triples in file based store do not need to be untagged
 * @author csl37
 *
 */
public class FromFileCloningTool extends SourceTaggingCloningTool{

	FromFileCloningTool(int stepSize, boolean isQuads) {
		super(stepSize, isQuads);
	}

	public void clone(FileBasedStoreClient sourceStore, String sourceGraph, StoreClientInterface targetStore, String targetGraph) {
		
		//Don't write back to file
		sourceStore.setAutoWrite(false);
	
		performClone(sourceStore, sourceGraph, targetStore, targetGraph);	
	}
	
	@Override
	protected void performClone(StoreClientInterface sourceKB, String sourceGraph, StoreClientInterface targetKB, String targetGraph) {
	
		// Count triples excluding blank nodes
		WhereBuilder whereCount = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(exprFilterOutBlanks());
	    int count = countTriples(sourceKB, sourceGraph, whereCount);
		int steps = count/stepSize;
		if(count%stepSize > 0) {steps++;}
		
		cloneLoop(sourceKB, sourceGraph, targetKB, targetGraph, steps, stepSize);
		
		// Clone everything that is not tagged i.e. blank nodes
		cloneUntaggedTriples(sourceKB, sourceGraph, targetKB, targetGraph);
		
		//source store is not untagged
		
		//TODO throw error
		checkCount(sourceKB,sourceGraph);
		checkCount(targetKB,targetGraph);
		checkNoTags(sourceKB,sourceGraph);
	}
	
}
