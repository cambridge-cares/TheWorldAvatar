package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.tools.RenamingTool;

class RenameOntokin {
	
	
	private String queryEndpoint = "http://theworldavatar.com/blazegraph/namespace/ontocompchem/sparql";
	private String updateEndpoint = "http://theworldavatar.com/blazegraph/namespace/ontocompchem/update";
	
	private String target = "http://www.theworldavatar.com/kb/ontokin/ontokin.owl";
	private String replacement = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl";
	
	@Test
	public void testOntokin() throws ParseException {
		
		long startTime = System.nanoTime();
		String[] names = {"http://purl.org/gc/"};
		for (int i=0; i<names.length; i++) {
			performHash(names[i]);
			//performOntokin(strFilter);
		}
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println("Total time:" + duration);
	}
	
	public void performOntokin(String strFilter) throws ParseException {
		
		long startTime = System.nanoTime();
		
		KnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint,updateEndpoint);		
		
		RenamingTool renameTool = new RenamingTool(target, replacement);
		renameTool.renameString(kbClient);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}
	
	
	public void performHash(String strFilter) throws ParseException {
		
		
		ExprFactory exprFactory = new ExprFactory();
		ExprVar exprOldP = RenamingTool.exprP;
		
		Expr filter = exprFactory.strstarts(exprFactory.str(exprOldP), exprFactory.asExpr(strFilter));
		
		long startTime = System.nanoTime();
		
		KnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint,updateEndpoint);
		
		RenamingTool renameTool = new RenamingTool("/kb/", "#", "/");
		renameTool.setFilter(filter);
		renameTool.renameString(kbClient);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}
	
	
	/*
	@Test
	public void testKB() throws ParseException {
		
		long startTime = System.nanoTime();
		for (int i=0; i<names.length; i++) {
			String strFilter = "STRSTARTS(STR(?s),\"" + names[i] +"\")";
		//	String strFilter = "!STRSTARTS(STR(?s),\"http://www.theworldavatar.com/\")";
			performKB(strFilter);
		}
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println("Total time:" + duration);
	}
	
	public void performKB(String strFilter) throws ParseException {
		long startTime = System.nanoTime();
		
		KnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint,updateEndpoint);
	
		RenameTool.renameURIStringFilter(kbClient, "/kb/", "#", "/", strFilter);
		
		long endTime = System.nanoTime();
		long duration = (endTime - startTime)/1000000000;
		System.out.println(duration);
	}
	*/	
}
