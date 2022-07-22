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

public class CloningTool {

	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(CloningTool.class);

	private int stepSize;
	private int overlap;
	private final double defaultOverlapRatio = 0.1; //10% overlap by default
	
	//TODO ==false condition to be implemented
	private boolean emptyTargetRequired = true; //require the target store is empty to start clone
	
	static ExprFactory exprFactory = new ExprFactory();
	
	//Default constructor
	CloningTool(){
		stepSize = 100000;
		overlap = (int) (stepSize*defaultOverlapRatio);
	}
		
	CloningTool(int stepSize, int overlap){
		this.stepSize = stepSize;
		this.overlap = overlap;
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
	 * Perform clone from source to target store
	 * @param source store client
	 * @param target store client
	 */
	public void clone(StoreClientInterface source, StoreClientInterface target) {
		
		int targetCount = target.getTotalNumberOfTriples();
		
		//Target store must be empty
		if(targetCount > 0) {
			LOGGER.error("Target store is not empty. Abort!");
			throw new JPSRuntimeException("Target store is not empty");
		}
		
		int sourceCount = source.getTotalNumberOfTriples();		
		int sourceBlankCount = countBlanks(source);
		int sourceCountExBlanks = sourceCount - sourceBlankCount;
		
		LOGGER.info("Total source count: "+Integer.toString(sourceCount)
				+", blanks: "+Integer.toString(sourceBlankCount));
		
		LOGGER.info("Starting clone..."
				+"Step size: "+Integer.toString(stepSize)
				+", overlap: "+Integer.toString(overlap));
		
		int nExpected = 0;
		int offset = 0;
		while(nExpected<sourceCountExBlanks) {
			
			if(nExpected>0) {
				offset = nExpected - overlap;
			}
			nExpected = offset+stepSize;

			performCloneStep(source, target, getSparqlConstructNoBlanks(stepSize, offset));

			//TODO implement overlap correction
			targetCount = target.getTotalNumberOfTriples();
			LOGGER.info("Cloned count: "+Integer.toString(targetCount)
					+", expected count: "+Integer.toString(nExpected)
					+", of "+Integer.toString(sourceCountExBlanks)
					+" (exludes triples with blank nodes).");
		}	
		
		//Clone blank nodes
		LOGGER.info("Cloning triples with blank nodes...");
		performCloneStep(source, target, getSparqlConstructBlanks());
	
		//Check target count
		targetCount = target.getTotalNumberOfTriples();
		if(targetCount != sourceCount) {
			String errorMessage = "Clone unsuccessful! Target count: "
					+Integer.toString(targetCount)
					+", does not match source count: "
					+Integer.toString(sourceCount)
					+". Please clear target store!"; 
			LOGGER.error(errorMessage);
			throw new JPSRuntimeException(errorMessage);
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
	private void performCloneStep(StoreClientInterface source, StoreClientInterface target, String constructQuery) {
		Model model = source.executeConstruct(constructQuery);
		target.executeUpdate(getSparqlInsert(model));
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
	private String getSparqlConstructNoBlanks(int stepsize, int offset) {
		
		return "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  !isblank(?s) && !isblank(?o) )}\n"+
		"LIMIT "+Integer.toString(stepsize)+" OFFSET "+Integer.toString(offset);
	}
	
	/**
	 * SPARQL construct query return only triples with blank nodes. 
	 * @return
	 */
	private String getSparqlConstructBlanks() {
		
		return "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  isblank(?s) || isblank(?o) )}";
	}
	
	/**
	 * Count triples with blank nodes
	 * @param storeClient
	 * @return
	 */
	Integer countBlanks(StoreClientInterface storeClient) {
		String query = "SELECT (COUNT(*) AS ?triples)"
				+"WHERE { ?s ?p ?o."
				+ "FILTER(  isblank(?s) || isblank(?o) )}";
    	JSONArray results = storeClient.executeQuery(query);
    	int triples = Integer.parseInt(results.getJSONObject(0).get("triples").toString());
    	return triples;
	}
	
}
