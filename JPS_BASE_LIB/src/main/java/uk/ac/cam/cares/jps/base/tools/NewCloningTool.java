package uk.ac.cam.cares.jps.base.tools;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.update.UpdateRequest;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class NewCloningTool {

	int limit = 100000;
	int overlap = (int) (limit*0.1); //10% overlap
	
	static ExprFactory exprFactory = new ExprFactory();
	
	static Var varS = Var.alloc("s");
	static Var varP = Var.alloc("p");
	static Var varO = Var.alloc("o");
	static Var varG = Var.alloc("g");
	static ExprVar exprS = new ExprVar(varS);
	static ExprVar exprP = new ExprVar(varP);
	static ExprVar exprO = new ExprVar(varO);
	static ExprVar exprG = new ExprVar(varG);
	
	class timeStep{
		
		long timeStamp;
		int targetCount;
		int cloneCount;
		
		timeStep(int cloneCount, int targetCount){
			this.timeStamp = System.nanoTime();
			this.cloneCount = cloneCount;
			this.targetCount = targetCount;
		}
		
		String get() {
			String line = Long.toString(timeStamp)+","+Integer.toString(targetCount)+","+Integer.toString(cloneCount)+",";
			return line;
		}
	}
	
	ArrayList<timeStep> times = new ArrayList<timeStep>();
	
	public void setLimit(int limit) {
		this.limit = limit;
		this.overlap = (int) (limit*0.1);
	}
	
	public void setLimitOverlap(int limit, int overlap) {
		this.limit = limit;
		this.overlap = overlap;
	}
	
	public void cloneOverlap(String sourceURL, String targetURL ) {
		cloneOverlap(sourceURL, targetURL, targetURL);
	}
	
	public void cloneOverlap(String sourceURL, String targetURLQuery, String targetURLUpdate) {
		
		RemoteStoreClient source = new RemoteStoreClient(sourceURL);
		RemoteStoreClient target = new RemoteStoreClient(targetURLQuery,targetURLUpdate);
		
		//TODO should filter out blanks here
		int sourceCount = source.getTotalNumberOfTriples();		
		
		times.add(new timeStep(0,0));
		
		int n = 0;
		int offset = 0;
		while(n<sourceCount) {
			
			if(n>0) {
				offset = n - overlap;
				System.out.print("offset");
				System.out.print(offset);
			}
			n = offset+limit;
			System.out.print("n");
			System.out.print(n);
			
			String constructQuery = getSparqlConstruct(limit, offset);
			Model model = source.executeConstruct(constructQuery);
			target.executeUpdate(getSparqlInsert(model));
			
			int targetCount = target.getTotalNumberOfTriples();
			System.out.print("target count");
			System.out.print(targetCount);
			times.add(new timeStep(n,targetCount));
			
		}	
		
		//Clone blank nodes
		String constructQuery = getSparqlConstructBlank();
		Model model = source.executeConstruct(constructQuery);
		target.executeUpdate(getSparqlInsert(model));
		
		int targetCount = target.getTotalNumberOfTriples();
		times.add(new timeStep(n,targetCount));
		
	}
		
	
	public void doubleClone(String sourceURL, String targetURL) {
		
		RemoteStoreClient source = new RemoteStoreClient(sourceURL);
		RemoteStoreClient target = new RemoteStoreClient(targetURL,targetURL);
		
		int sourceCount = source.getTotalNumberOfTriples();		
		
		int n = sourceCount/limit + 1;
		
		times.add(new timeStep(0,0));
		
		for(int i=0; i<n; i++) {
			
			int N = i*limit;
			
			String constructQuery = getSparqlConstruct(limit, N);
			Model model = source.executeConstruct(constructQuery);
			target.executeUpdate(getSparqlInsert(model));
			
			int targetCount = target.getTotalNumberOfTriples();
			
			times.add(new timeStep(N+limit,targetCount));
			
			System.out.print(targetCount);
		}
		
		//second round offset by half the limit
		String constructQuery = getSparqlConstruct(limit/2, 0);
		Model model = source.executeConstruct(constructQuery);
		target.executeUpdate(getSparqlInsert(model));
		int targetCount = target.getTotalNumberOfTriples();
		times.add(new timeStep(limit/2,targetCount));
		System.out.print(targetCount);
		
		int N=0;
		for(int i=1; i<n+1; i++) {
			
			N = i*limit-limit/2;
			
			constructQuery = getSparqlConstruct(limit, N);
			model = source.executeConstruct(constructQuery);
			target.executeUpdate(getSparqlInsert(model));
			
			targetCount = target.getTotalNumberOfTriples();
			times.add(new timeStep(limit+N,targetCount));
			System.out.print(targetCount);
		}
		
	}
	

	public void writeTimesToFile(String path) throws IOException {
		
		FileWriter fileWriter = new FileWriter(path);
	    PrintWriter printWriter = new PrintWriter(fileWriter);
		
		for(int i=0; i<times.size(); i++) {
			printWriter.print(times.get(i).get());
			printWriter.println();
		}
		
		printWriter.close();
	}
	
	private UpdateRequest getSparqlInsert(Model model) {
		
		UpdateBuilder builder = new UpdateBuilder().addInsert(model);
		return builder.buildRequest();
	}
	
	private String getSparqlConstruct(int limit, int offset) {
		
		return "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  !isblank(?s) && !isblank(?o) )}\n"+
		"LIMIT "+Integer.toString(limit)+" OFFSET "+Integer.toString(offset);
	}
	
	private String getSparqlConstructBlank() {
		
		return "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  isblank(?s) || isblank(?o) )}";
	}
	


}

