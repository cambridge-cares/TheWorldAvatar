package uk.ac.cam.cares.jps.base.query;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Iterator;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * Local in-memory implementation of the StoreClientInterface,
 * designed to serve as a mock store client object for testing.
 * 
 * @author csl37
 *
 */
public class LocalStoreClient implements StoreClientInterface {

	private Dataset dataset;
	private RDFConnection conn;

	private String query;
	
	public LocalStoreClient() {
		dataset = DatasetFactory.create();
		conn = RDFConnectionFactory.connect(dataset);
	}

	///////////////////////////
	// Sparql query and update
	///////////////////////////
	
	@Override
	public int executeUpdate() {
		return executeUpdate(this.query);
	}

	@Override
	public int executeUpdate(String update) {
		try {
			conn.begin( TxnType.WRITE );
			conn.update(update);
			conn.commit();
		} finally {
			conn.end();
		}
		return 0;
	}
	
	@Override
	public int executeUpdate(UpdateRequest update) {
		return executeUpdate(update.toString());		
	}

	@Override
	public String execute(){
		return execute(this.query);
	}

	@Override
	public String execute(String query){
		JSONArray result = executeQuery(query);
		return result.toString();
	}

	@Override
	public JSONArray executeQuery(String sparql) {
		ResultSet results = performExecuteQuery(sparql);
		return convert(results);
	}	

	@Override
	public JSONArray executeQuery() {
		return executeQuery(this.query);
	}

	private ResultSet performExecuteQuery(String sparql) {		
		try {
			conn.begin( TxnType.READ );
			QueryExecution queryExec = conn.query(sparql);
			ResultSet results = queryExec.execSelect();
			return results;
		} finally {
			conn.end();
		}	
	}

	private JSONArray convert(ResultSet resultSet) {
	
		JSONArray json = new JSONArray();
		
		while (resultSet.hasNext()) {
			QuerySolution qs = resultSet.next();
			JSONObject obj = new JSONObject();
			Iterator<String> it = qs.varNames(); 
			while(it.hasNext()) {
				String var = it.next(); 
				RDFNode node = qs.get(var);
				if(node.isLiteral()) {
					obj.put(var, node.asLiteral().getValue());	
				}else {
					obj.put(var, node);
				}
			}
			json.put(obj);
		}
		return json;
	}

	@Override
	public Model executeConstruct(Query sparql) {
		return executeConstruct(sparql.toString());
	}

	@Override
	public Model executeConstruct(String sparql) {
		try {
			conn.begin( TxnType.READ );
			QueryExecution queryExec = conn.query(sparql);
			Model results = queryExec.execConstruct();
			return results;
		} finally {
			conn.end();
		}
	}
	
	@Override
	public String get(String resourceUrl, String accept) {
		
		Var varS = Var.alloc("s");
		Var varP = Var.alloc("p");
		Var varO = Var.alloc("o");
		
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct( varS, varP, varO);
		
		if (resourceUrl == null) {
			//Default graph
			builder.addWhere(varS, varP, varO);
		}else {	
			//Named graph
			String graphURI = "<" + resourceUrl + ">";
			builder.addGraph(graphURI, varS, varP, varO);	
		}
		
		Model model = executeConstruct(builder.build());
		
		Lang syntax;
		if (accept != null) {
			syntax = RDFLanguages.contentTypeToLang(accept);
		}else {
			//default to application/rdf+xml
			syntax = Lang.RDFXML; 
		}
		
		StringWriter out = new StringWriter();
		model.write(out, syntax.getName());
		return out.toString();
	}

	@Override
	public void insert(String graphName, String content, String contentType) {
		
		Model model = ModelFactory.createDefaultModel();
		
		InputStream in = new ByteArrayInputStream(content.getBytes());
		
		if (contentType == null) {
			//RDF/XML default
			//base=null, assume all uri are absolute
			model.read(in, null); 
		} else {
			Lang syntax = RDFLanguages.contentTypeToLang(contentType);
			model.read(in,null,syntax.getName());
		}
		
		UpdateBuilder builder = new UpdateBuilder();
		
		if (graphName == null) {
			builder.addInsert(model);
		} else {
			String graphURI = "<" + graphName + ">";
			builder.addInsert(graphURI, model);
		}
		
		executeUpdate(builder.buildRequest());	
	}

	///////////////////////////
	// Get and Set methods
	///////////////////////////
	
	@Override
	public String setQuery(String query) {
		this.query = query;
		return query;
	}

	@Override
	public String getQuery() {
		return query;
	}

	@Override
	public String getQueryEndpoint() {
		return null;
	}

	@Override
	public String setQueryEndpoint(String queryEndpoint) {
		return null;
	}

	@Override
	public String getUpdateEndpoint() {
		return null;
	}

	@Override
	public String setUpdateEndpoint(String updateEndpoint) {
		return null;
	}

	@Override
	public String getUser() {
		return null;
	}

	@Override
	public void setUser(String userName) {
	}

	@Override
	public String getPassword() {
		return null;
	}

	@Override
	public void setPassword(String password) {
	}
}
