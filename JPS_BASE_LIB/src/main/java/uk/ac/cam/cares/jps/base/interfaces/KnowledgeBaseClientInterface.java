package uk.ac.cam.cares.jps.base.interfaces;

import java.sql.SQLException;

import org.json.JSONArray;

public interface KnowledgeBaseClientInterface {

	public JSONArray executeQuery(String query) throws SQLException;	
	
	public JSONArray executeQuery() throws SQLException;
	
	public String execute() throws SQLException;
	
	public String execute(String sparql) throws SQLException;
	
	public int executeUpdate() throws SQLException;
	
	public int executeUpdate(String update) throws SQLException;
	
	public void load();
	
	public void finish();
	
}
