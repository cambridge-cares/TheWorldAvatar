package uk.ac.cam.cares.jps.base.query.sparql;

import java.util.ArrayList;
import java.util.List;

public class QueryBuilder {

	private final static String BREAK = "\r\n";
	private final static String POINT = " .\r\n";
	
	StringBuffer select = null;
	List<String> prefixes = new ArrayList<String>();
	List<StringBuffer> wherestatements = new ArrayList<StringBuffer>();
	
	public StringBuffer select(String... varNames) {
		select = new StringBuffer("SELECT ");
		for(String current : varNames) {
			select.append(current).append(" ");
		}
		return select;
	}
	
	public StringBuffer a(String fromVar, String prefix, String classname) {
		addPrefix(prefix);
		StringBuffer b = new StringBuffer(fromVar).append(" a ").append(prefix).append(":").append(classname);
		wherestatements.add(b);
		return b;	
	}
	
	public StringBuffer prop(String fromVar, String toVar, String... path) {
		
		StringBuffer b = new StringBuffer(fromVar).append(" ");
		
		for (int i=0; i<path.length; i=i+2) {
			if (i>0) {
				b.append("/");
			}
			String prefix = path[i];
			addPrefix(prefix);
			String property = path[i+1];
			b.append(prefix).append(":").append(property);
		}
		b.append(" ").append(toVar);
		
		wherestatements.add(b);
		return b;		
	}
	
	private void addPrefix(String prefix) {
		if (!prefixes.contains(prefix)) {
			prefixes.add(prefix);
		}
	}
	
	public StringBuffer build() {
		StringBuffer b = new StringBuffer();
		for (String current : prefixes) {
			String url = PrefixToUrlMap.getPrefixUrl(current);
			b.append("PREFIX ").append(current).append(":<").append(url).append("> ").append(BREAK);
		}
		
		b.append(select).append(BREAK);
		b.append("WHERE {").append(BREAK);
		
		for (StringBuffer current : wherestatements) {
			b.append(current).append(POINT);
		}
		
		b.append("}");
		
		return b;
	}
}
