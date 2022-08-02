package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * 
 * @author csl37
 *
 */
public class CloningTool {

	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(CloningTool.class);

    private static final int MAX_ATTEMPTS = 5; //Maximum number of attempts to increase overlap before failing
    
	private int stepSize;
	private int overlap;
	private final double defaultOverlapRatio = 0.1; //10% overlap by default
	
	//TODO ==false condition to be implemented
	private boolean emptyTargetRequired = true; //require the target store is empty to start clone
	
	static ExprFactory exprFactory = new ExprFactory();
	
	//Default constructor
	public CloningTool(){
		stepSize = 100000;
		overlap = (int) (stepSize*defaultOverlapRatio);
	}
		
	public CloningTool(int stepSize, int overlap){
		this.stepSize = stepSize;
		this.overlap = overlap;
		if(!checkStepAndOverlap()) {
			cloneFailed("overlap cannot be larger than stepsize!",0,0);
		}
	}
	
	/////////////////
	//Setter methods
	
	/**
	 * Set the step size with the default overlap ratio 
	 * @param stepsize
	 */
	public void setStepsize(int stepSize) {
		this.stepSize = stepSize;
		this.overlap = (int) (stepSize*defaultOverlapRatio);
	}
	
	/**
	 * Set the step size and overlap
	 * @param stepSize
	 * @param overlap
	 */
	public void setStepsizeAndOverlap(int stepSize, int overlap) {
		this.stepSize = stepSize;
		this.overlap = overlap;
		if(!checkStepAndOverlap()) {
			cloneFailed("overlap cannot be larger than stepsize!",0,0);
		}
	}
	
	/**
	 * Override the requirement for the target store to be empty
	 * to start the clone
	 */
	//TODO implement this
	/*
	public void overrideEmptyTargetRequirement() {
		emptyTargetRequired = false;
		LOGGER.info("Warning: overriding empty target store requirement.");
	}
	*/
	
	/////////////////
	
	/**
	 * Check step size is larger than overlap
	 */
	public boolean checkStepAndOverlap() {
		if(overlap >= stepSize) {
			return false;
		}else {
			return true;
		}
	}
	
	/**
	 * Perform clone from source to target store
	 * @param source store client
	 * @param target store client
	 */
	public void clone(StoreClientInterface source, StoreClientInterface target) {
		
		int targetCount = target.getTotalNumberOfTriples();
		
		//Target store must be empty
		if(targetCount > 0) {
			cloneFailed("Target store is not empty!",targetCount,0);
		}
		
		int sourceCount = source.getTotalNumberOfTriples();		
		int sourceBlankCount = countBlanks(source);
		int sourceCountExBlanks = sourceCount - sourceBlankCount;
		
		LOGGER.info("Total source count: "+Integer.toString(sourceCount)
				+", blanks: "+Integer.toString(sourceBlankCount));		
		LOGGER.info("Starting clone..."
				+"Step size: "+Integer.toString(stepSize)
				+", overlap: "+Integer.toString(overlap));
		
		int attempts = 0;
		int nExpected = 0;
		int offset = 0;
		while(nExpected<sourceCountExBlanks) {
			
			int oldnExpected = nExpected;
			if(nExpected>0) {
				offset = nExpected - overlap;
			}
			nExpected = Math.min(offset+stepSize,sourceCountExBlanks);

			performCloneStep(source, target, getSparqlConstructNoBlanks(stepSize, offset));

			//Overlap correction
			targetCount = target.getTotalNumberOfTriples();			
			if(targetCount < nExpected) {
				adjustOverlap(targetCount, nExpected, attempts);
				//Reset nExpected to retry step 
				nExpected = oldnExpected;
				attempts ++;
			}else {
				LOGGER.info("Cloned count: "+Integer.toString(targetCount)
				+", expected count: "+Integer.toString(nExpected)
				+", of "+Integer.toString(sourceCountExBlanks)
				+" (exludes triples with blank nodes).");
			}
		}	
		
		//Clone blank nodes
		LOGGER.info("Cloning triples with blank nodes...");
		performCloneStep(source, target, getSparqlConstructBlanks());
	
		//Check target count
		targetCount = target.getTotalNumberOfTriples();
		if(targetCount != sourceCount) {
			String reason = "Please clear the target store and try again. "
					+ " Consider increasing the step size and overlap."; 
			cloneFailed(reason, targetCount, nExpected);
		}
		LOGGER.info("Clone successful! Cloned "+Integer.toString(targetCount)
		+" of "+Integer.toString(sourceCount));		
	}
	
	/**
	 * Perform a single clone step
	 * @param source store
	 * @param target store
	 * @param constructQuery
	 */
	public void performCloneStep(StoreClientInterface source, StoreClientInterface target, String constructQuery) {
		Model model = source.executeConstruct(constructQuery);
		target.executeUpdate(getSparqlInsert(model));
	}
	
	/**
	 * Adjust overlap
	 * @param targetCount
	 * @param nExpected
	 * @param attempts
	 */
	public void adjustOverlap(int targetCount, int nExpected, int attempts) {
		
		//Increase overlap by the larger of 10% of the step size or the size of countError
		overlap += Math.max((int) (defaultOverlapRatio*stepSize), nExpected - targetCount);
		
		LOGGER.info("Cloned count: "+Integer.toString(targetCount)
				+" does not match expected count "+Integer.toString(nExpected)
				+". Increasing overlap to "+Integer.toString(overlap)
				+" and retrying.");
		
		//fail if max attempts to correct overlap is reached
		//or if overlap is larger than the step size
		if(attempts>=MAX_ATTEMPTS || overlap >= stepSize) {
			String reason = Integer.toString(attempts)
					+" attempt(s) to increase overlap...\n" 
					+" Please clear the target store! Consider increasing step size."; 
			cloneFailed(reason, targetCount, nExpected);
		}
	}
	
	/**
	 * Log clone failed error and throw JPSRuntimeException
	 * @param reason
	 * @param targetCount
	 * @param nExpected
	 */
	public void cloneFailed(String reason, int targetCount, int nExpected) {
		String errorMessage = "Cloning tool: clone failed...\n"
		+"Reason: "+reason+"\n"
		+"Parameters: stepSize="+Integer.toString(stepSize)
		+", overlap="+Integer.toString(overlap)
		+", target count="+Integer.toString(targetCount)
		+", expected count="+Integer.toString(nExpected);		
		LOGGER.error(errorMessage);
		throw new JPSRuntimeException(errorMessage);
	}
		
	///////////////
	// SPARQL  
	
	/**
	 * Get SPARQL update to insert data
	 * @param model
	 * @return
	 */
	private UpdateRequest getSparqlInsert(Model model) {
		
		UpdateBuilder builder = new UpdateBuilder().addInsert(model);
		return builder.buildRequest();
	}
	
	/**
	 * SPARQL construct query excluding triples with blank nodes.
	 * @param stepsize
	 * @param offset
	 * @return
	 */
	public String getSparqlConstructNoBlanks(int stepsize, int offset) {
		
		return "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  !isblank(?s) && !isblank(?o) )}\n"+
		"LIMIT "+Integer.toString(stepsize)+" OFFSET "+Integer.toString(offset);
	}
	
	/**
	 * SPARQL construct query return only triples with blank nodes. 
	 * @return
	 */
	public String getSparqlConstructBlanks() {
		
		return "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  isblank(?s) || isblank(?o) )}";
	}
	
	/**
	 * Count triples with blank nodes
	 * @param storeClient
	 * @return
	 */
	public Integer countBlanks(StoreClientInterface storeClient) {
		String query = "SELECT (COUNT(*) AS ?triples)"
				+"WHERE { ?s ?p ?o."
				+ "FILTER(  isblank(?s) || isblank(?o) )}";
    	JSONArray results = storeClient.executeQuery(query);
    	int triples = Integer.parseInt(results.getJSONObject(0).get("triples").toString());
    	return triples;
	}
	
}
