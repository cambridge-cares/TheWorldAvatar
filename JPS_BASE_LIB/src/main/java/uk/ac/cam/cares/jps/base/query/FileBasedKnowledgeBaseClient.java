package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * This class uses RDFConnection to load and provide SPARQL access to file based datasets.
 * The behaviour is designed to be analogous to RemoteKnowledgeBaseClient with the methods
 * declared in KnowledgeBaseClientInterface.
 * Files are automatically loaded when a file path is supplied via the class constructor or 
 * set methods (in this case, files are loaded prior to sparql query/update).
 * By default, data is automatically written to file after a SPARQL update.
 * Further read/write functionality is provided through the load and writeToFile methods 
 * including support for multiple files loaded to different contexts/named graphs. 
 * Note that the FileBasedKnowledgeBaseClient only supports loading a single file to a single 
 * context (including the default graph) and will throw an error if a context already exists 
 * in the dataset (or the default graph is not empty). 
 * Files containing more than one context are also not supported.
 * 
 * @author Casper Lindberg
 *
 */
public class FileBasedKnowledgeBaseClient implements KnowledgeBaseClientInterface {

	private Dataset dataset;
	private RDFConnection conn;
	
	private String query;

	/**
	 *  Object to store the graph name, file path, and serialization language
	 */
	class GraphData{
		String name = null;
		String path = null;
		Lang lang = null;
		
		//Constructors
		GraphData(){}
		GraphData(String name, String filePath){
			this.name = name;
			this.path = filePath;
		}
	}
	
	// List of named graphs
	private ArrayList<GraphData> namedGraphs = new ArrayList<GraphData>();
	
	// Default graph
	private GraphData defaultGraph = new GraphData();
	
	// Dataset written to file after sparql update by default
	private boolean autoWrite = true;	
	
	///////////////////////////
	// Constructors
	///////////////////////////
	
	/**
	 * Default constructor. Creates a file-based client without loading a file.
	 */
	public FileBasedKnowledgeBaseClient() {
		init();
	}
	
	/**
	 * Constructor loads a triples to the default graph and quads to a named graph.
	 * @param filePath
	 */
	public FileBasedKnowledgeBaseClient(String filePath) {
		init();
		load(filePath);
	}
	
	/**
	 * Constructor loads a file to a named graph.
	 * @param graph name/context
	 * @param filePath
	 */
	public FileBasedKnowledgeBaseClient(String graph, String filePath) {
		init();
		load(graph, filePath);
	}
	
	/**
	 * Initialise RDFConnection. 
	 */
	private void init() {
		dataset = DatasetFactory.create();
		conn = RDFConnectionFactory.connect(dataset);
	}
	
	///////////////////////////
	// Load methods
	///////////////////////////
		
	/**
	 * Load files to memory.
	 */
	@Override
	public void load() {
		
		//Load multiple files if provided
		if( namedGraphs.size() > 0 ) {
			
			for(int i=0; i<namedGraphs.size(); i++) {
				loadGraph(namedGraphs.get(i));
			}
		}
		
		//Load single file to default graph		
		if(defaultGraph.path != null){
			loadGraph(defaultGraph);
		}
	}

	/**
	 * Load file to default graph.
	 * @param filePath
	 */
	public void load(String filePath) {
		//Load file
		load(null, filePath);
	}
	
	/**
	 * Load a named graph
	 * @param graph
	 * @param filePath
	 */
	public void load(String graphName, String filePath) {
	
		GraphData graph = new GraphData(graphName, filePath);
		
		loadGraph(graph);

		if(graph != null) {
			if(graph.name == null) {
				//Set default file path
				defaultGraph = graph;	
			}else {	
				//Add graph and filePath to array
				namedGraphs.add(graph);
			}
		}
	}
	
	/**
	 * Loads multiple files/contexts.
	 * @param graphs
	 * @param filePaths
	 */
	public void load(String[] names, String[] filePaths) {
		
		//graphs and file paths should be the same length
		if(names.length == filePaths.length) {
			for(int i=0; i<names.length; i++) {
				load(names[i], filePaths[i]);
			}
		}else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: file path or graph name missing (graphs.length != flePaths.length).");
		}
	}	
	
	/**
	 * Perform load to dataset
	 * @param graph
	 * @param filePath
	 */
	private void loadGraph(GraphData graph) {
	
		//If not connected then initialise a connection 
		if (!isConnected()) {
			init();
		}
			
		//Check that the file exists
		File f= new File(graph.path);
		if ( f.exists() ) {			
			
			//Get serialisation language
			Lang lang = RDFLanguages.filenameToLang(graph.path);
			System.out.println("FileBasedKnowledgeBaseClient: File language is: " + lang);
			
			//Set file output language to input language
			graph.lang = lang;	
			
			//Data is triples
			if(RDFLanguages.isTriples(lang)) {
	
				//error: graph/context already exists in the dataset
				if(graph.name == null) {
					if(!dataset.getDefaultModel().isEmpty()) {
						throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: default graph already exists!");
					}
				}else {
					if(dataset.containsNamedModel(graph.name)) { 
						throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: " + graph.name + " already exists!");
					}
				}
				
				conn.load(graph.name, graph.path);
				
			//Data is quads
			}else {
				
				//Load quads to separate dataset first 
				Dataset tempDataset = RDFDataMgr.loadDataset(graph.path);
				
				Iterator<String> it = tempDataset.listNames();
				
				int contextCount = 0;
				while(it.hasNext()) {
					
					contextCount ++;
					
					//error: multiple contexts in file
					if(contextCount > 1) {
						throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: multiple contexts in file not supported!");
					}
					
					String context = it.next();
					
					//error: context already exists in the dataset
					if(context == null) {
						if(!dataset.getDefaultModel().isEmpty()) {
							throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: default graph already exists!");
						}
					}else {
						if(dataset.containsNamedModel(context)) {
							throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: " + context + " already exists!");
						}
					}
					
					//context does not match the supplied graph name ... change graph name
					if(!context.equals(graph.name)) {					
						System.out.println("FileBasedKnowledgeBaseClient: graph name " + graph.name + " changed to " + context);
						graph.name = context;
					}
				}		
			
				//add the data to the connection dataset
				conn.loadDataset(tempDataset);
				
				//clear and close the temporary resource
				tempDataset.asDatasetGraph().clear(); 
				tempDataset.close();
			}
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: cannot load " + graph.path + ". File does not exist.");
		}
	}	
	
	///////////////////////////
	// Write methods
	///////////////////////////
	
	/**
	 * Writes the model to file and closes the connection.
	 */
	@Override
	public void end() {
		writeToFile();
		conn.close();
		dataset.close();
	}
	
	/**
	 * Write the model(s) back to file.
	 * @throws IOException
	 */
	public void writeToFile(){
		
		//named graphs
		if( namedGraphs.size() > 0 ) { 
			for(int i = 0; i < namedGraphs.size(); i++) {
				writeToFile(namedGraphs.get(i));
			}
		}
		
		//default graph
		if(defaultGraph.path != null){	
			writeToFile(defaultGraph);
		}
	}
	
	/**
	 * Write graph back to file. Null or "default" writes the default graph.
	 * @param graph
	 */
	public void writeToFile(String graphName) {
		
		if(graphName == null || graphName.equals("default")) {
			if(defaultGraph.path != null || defaultGraph.lang != null) {
				writeToFile(defaultGraph);
			}else {
				throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: no file path.");
			}
		}else {
			GraphData graph = getGraph(graphName); 
			if(graph!=null) {
				writeToFile(graph);
			}else {
				throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: graph not found.");
			}
		}
		
	}
	
	/**
	 * Write the default graph to file. File path and serialization language specified.
	 * @param filePath
	 * @param langOut 
	 * @throws IOException
	 */
	public void writeToFile(String filePath, Lang langOut) {
		writeToFile(null, filePath, langOut);
	}
	
	//Helper function.
	private void writeToFile(GraphData graph) {
		writeToFile(graph.name, graph.path, graph.lang);
	}
	
	/**
	 * Write a named graph to file. Graph URI, file path and serialization language specified.
	 * @param graph uri
	 * @param filePath
	 * @param langOut 
	 * @throws IOException
	 */
	public void writeToFile(String name, String filePath, Lang langOut) {
		
		try (OutputStream out = new FileOutputStream(filePath)){

			//Default graph
			if(name == null || name.equals("default")) {
				
					RDFDataMgr.write(out, dataset.getDefaultModel(), langOut);	
					out.flush();
			
			//Named graph
			}else if(dataset.containsNamedModel(name)) {
					
					RDFDataMgr.write(out, dataset.getNamedModel(name), langOut);
					out.flush();
			
			}else {
				throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: " + name + " does not exist.");
			}
		}catch(IOException e) {
			throw new JPSRuntimeException(e);
		}
	}
	
	///////////////////////////
	// Variable access methods
	///////////////////////////
	
	//Authentication 
	@Override
	public String getUser() {
		// no authentication for FileBasedKBClient
		return null;
	}

	@Override
	public void setUser(String userName) {
		// no authentication for FileBasedKBClient
	}

	@Override
	public String getPassword() {
		// no authentication for FileBasedKBClient
		return null;
	}

	@Override
	public void setPassword(String password) {
		// no authentication for FileBasedKBClient
	}
	
	//// Methods to access graphs
	
	/**
	 * Get graph matching name. Otherwise returns null.
	 * @param name
	 * @return
	 */
	private GraphData getGraph(String name) {
		
		if(name == null || name.equals("default")) {
			return defaultGraph;
		}else {
			for(int i=0; i<namedGraphs.size(); i++) {
				if (namedGraphs.get(i).name.equals(name)) {
					return namedGraphs.get(i);
				}
			}
		}
		return null;
	}
	
	/**
	 * Checks if the named graph exists in both the GraphData object and the Dataset
	 * @param name
	 * @return
	 */
	public boolean containsGraph(String name) {
		GraphData graph = getGraph(name);	//contained in GraphData
		if(graph != null) {
			return dataset.containsNamedModel(name); //Contained in dataset
		}else {
			return false;
		}
	}
	
	/**
	 * Returns list of graph names 
	 * @return
	 */
	public List<String> getGraphNames() {
		List<String> names = new ArrayList<String>();
		for(int i=0; i<namedGraphs.size(); i++) {
			names.add(namedGraphs.get(i).name);
		}
		return names;
	}
	
	/**
	 * Get serialization language of graph.
	 * @param name (null or "Default" for default graph)
	 * @return
	 */
	public Lang getLang(String name) {
		GraphData graph = getGraph(name);
		return graph.lang;
	}
	
	/**
	 * Get file path of given graph.
	 * @param name (null or "Default" for default graph)
	 * @return
	 */
	public String getPath(String name) {
		GraphData graph = getGraph(name);
		return graph.path;
	}
	
	////
	
	/**
	 * Toggle the automatic write to file after update
	 * @param value
	 */
	public void setAutoWrite(boolean value) {
		this.autoWrite = value;
	}
	
	/**
	 * Set output serialization language for default graph
	 * @param langOut
	 */
	public void setOutputLang(Lang langOut) {
		defaultGraph.lang = langOut;
	}
	
	/**
	 * Set output serialization language for named graph if it exists
	 * @param langOut
	 */
	public void setOutputLang(String name, Lang langOut) {
		
		GraphData graph = getGraph(name);
		if(graph != null) {
			graph.lang = langOut;
		}
	}
	
	/**
	 * Sets a query.
	 * @param query
	 * @return
	 */
	@Override
	public String setQuery(String query) {
		this.query = query;
		return this.query;
	}
	
	/**
	 * Returns the set query.
	 * @return
	 */
	@Override
	public String getQuery() {
		return query;
	}	
	
	/**
	 * Set the file path for default graph.
	 * @param filePath
	 */
	public void setPath(String filePath) {
		setPath(null, filePath);
	}
	
	/**
	 * Set the file path for named graph.
	 * @param name (null or "default" for the default graph)
	 * @param filePath
	 */
	public void setPath(String name, String filePath) {
		GraphData graph = getGraph(name);
		if(graph != null) {
			graph.path= filePath;
		}
	}
	
	/**
	 * Sets the default graph file path. 
	 * Query and and update file paths are the same.
	 * @param updateEndpoint
	 */
	@Override 
	public String setUpdateEndpoint(String updateEndpoint) {
		defaultGraph.path = updateEndpoint;
		return defaultGraph.path;
	}
	
	/**
	 * Return the default graph file path. Query and update file paths are the same.
	 */
	@Override
	public String getUpdateEndpoint() {
		return defaultGraph.path;
	}
	
	/**
	 * Sets the default graph file path.
	 * Query and and update file paths are the same.
	 * @param queryEndpoint
	 */
	@Override 
	public String setQueryEndpoint(String queryEndpoint) {
		defaultGraph.path = queryEndpoint;
		return defaultGraph.path;
	}
	
	/**
	 * Return the default graph file path. Query and update file paths are the same.
	 */
	@Override
	public String getQueryEndpoint() {
		return  getUpdateEndpoint();
	}
	
	//// 
	
	/**
	 * Checks the connection is active.
	 * @return
	 */
	public boolean isConnected() {
		return !conn.isClosed();
	}
	
	/**
	 * Checks the model contains data.
	 * @return
	 */
	public boolean isEmpty() {
		
		if(!isConnected()) {
			return true;
		}else {
			return dataset.isEmpty();
		}
	}	
	
	///////////////////////////
	// Sparql query and update
	///////////////////////////
	
	/**
	 * Executes the update operation using update supplied
	 * through the constructor or setter methods.
	 * writeToFile() or end() must be called to save changes to file.
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate() {
		return executeUpdate(this.query);
	}
	
	/**
	 * Executes the update operation supplied by the calling method.
	 * writeToFile() or end() must be called to save changes to file.
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate(String update) {
		
		//Attempt to load files if the dataset is empty.
		if(isEmpty()) {load();} 
				
		if( conn != null) {
			conn.begin( TxnType.WRITE );
			try {
				conn.update(update);
				conn.commit();
			} finally {
				conn.end();
			}
			if(autoWrite == true) {writeToFile();} //write changes to file (default behaviour)
			return 0; //return a useful integer?
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}

	/**
	 * Executes the update operation supplied by the calling method and returns results.
	 * 
	 * @param update as UpdateRequest
	 * @return
	 */
	@Override
	public int executeUpdate(UpdateRequest update) {
		
		//Attempt to load files if the dataset is empty.
		if(isEmpty()) {load();}
		
		if( conn != null) {
			conn.begin( TxnType.WRITE );
			try {
				conn.update(update);
				conn.commit();
			}finally {
				conn.end();
			}
			if(autoWrite == true) {writeToFile();} //write changes to file (default behaviour)
			return 0; //TODO return a useful integer?
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}
	
	/**
	 * Execute sparql query using the query variable
	 * 
	 * @return JSONArray as String 
	 * @throws SQLException
	 */
	@Override
	public String execute(){
		return execute(this.query);
	}
	
	/**
	 * Excute sparql query
	 * 
	 * @param sparql
	 * @return JSONArray as String
	 * @throws SQLException
	 */
	@Override
	public String execute(String query){
		JSONArray result = executeQuery(query);
		if(result==null){
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: sparql query result is null.");
		}else{
			return result.toString();
		}
	}
	
	/**
	 * Executes the query supplied by the calling method and returns results<p>
	 * as a JSONArray.
	 */
	@Override
	public JSONArray executeQuery(String sparql) {
		ResultSet results = perfromExecuteQuery(sparql);
		return convert(results);
	}	
	
	/**
	 * Executes the query that is provided through the constructors or setter<p>
	 * method.
	 */
	@Override
	public JSONArray executeQuery() {
		return executeQuery(this.query);
	}
	
	/**
	 * Performs sparql query execution.
	 * @param sparql
	 * @return
	 */
	private ResultSet perfromExecuteQuery(String sparql) {
		
		//Attempt to load files if the dataset is empty.
		if(isEmpty()) {load();} 
		
		if (conn != null) {
			conn.begin( TxnType.READ );	
			try {
				QueryExecution queryExec = conn.query(sparql);
				ResultSet results = queryExec.execSelect();
				return results;
			} finally {
				conn.end();
			}
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}
	
	/**
	 * Convert query results to JSONArray.
	 * @param resultSet
	 * @return
	 */
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
	
	/**
	 * Perform a sparql construct query
	 * @return RDF model
	 */
	@Override
	public Model executeConstruct(Query sparql) {
		return executeConstruct(sparql.toString());
	}
	
	/**
	 * Perform a sparql construct query
	 * @return RDF model
	 */
	@Override
	public Model executeConstruct(String sparql) {
		
		//Attempt to load files if the dataset is empty.
		if(isEmpty()) {load();}
		
		if (conn != null) {
			conn.begin( TxnType.READ );	
			try {
				QueryExecution queryExec = conn.query(sparql);
				Model results = queryExec.execConstruct();
				return results;
			} finally {
				conn.end();
			}
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}
}
