package uk.ac.cam.cares.jps.base.tools.test;

import org.apache.jena.arq.querybuilder.AskBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

public class CloningToolTestHelper {

	//Test triple template
	public static final String s = "<http://www.example.com/s%s>";
	public static final String p = "<http://www.example.com/p%s>";
	public static final String o = "<http://www.example.com/o%s>";
	
	public static String createTriples(int N) {
		
		StringBuilder stringBuilder = new StringBuilder();
	
		for(int i = 0; i < N; i++) {
			String si = String.format(s, Integer.toString(i)); 
			String pi = String.format(p, Integer.toString(i));
			String oi = String.format(o, Integer.toString(i));
			stringBuilder.append(si+pi+oi+".\n");
		}
		return stringBuilder.toString();
	}
	
	public static String createInsertData(String triples) {
		StringBuilder stringBuilder = new StringBuilder()
									.append("INSERT DATA {")
									.append(triples)
									.append("}");
		return stringBuilder.toString();
	}
	
	public static boolean checkTriples(int N, StoreClientInterface targetStoreClient) {
		
		boolean check = true;
		
		for(int i = 0; i < N; i++) {
		
			String si = String.format(s, Integer.toString(i));
			String pi = String.format(p, Integer.toString(i));
			String oi = String.format(o, Integer.toString(i));
	
			WhereBuilder where = new WhereBuilder();
			where.addWhere(si, pi, oi);
			
	    	check = checkSingleTriple(targetStoreClient, where);
		}
		return check;
	}
	
	public static boolean checkSingleTriple(StoreClientInterface targetStoreClient, WhereBuilder where) {
		
		AskBuilder builder = new AskBuilder();
		String askQuery = builder.build().toString();
		String result = targetStoreClient.execute(askQuery);
		JSONObject obj =  new JSONArray(result).getJSONObject(0);
		return (boolean) obj.get("ASK");
	}
	
	public static String removeWhiteSpace(String string) {
		return string.replaceAll("\\s+","");
	}
}
