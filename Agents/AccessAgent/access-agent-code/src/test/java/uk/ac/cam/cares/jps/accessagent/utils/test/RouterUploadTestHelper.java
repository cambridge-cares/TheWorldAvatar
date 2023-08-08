package uk.ac.cam.cares.jps.accessagent.utils.test;

public class RouterUploadTestHelper {

	public static String removeWhiteSpace(String string) {
		return string.replaceAll("\\s+","");
	}
	
	public static String getTestQueryExpected(String label) {
		return "[{\"o\":\""+label+"\"}]";
	}
	
	public static String getTestQuery(String label) {		
		return "SELECT ?o\n WHERE{<http://www.theworldavatar.com/kb/ontokgrouter/"
				+label+"> <http://www.w3.org/2000/01/rdf-schema#label> ?o}";
	}
	
	public static String getJsonString(String label1, String label2) {
		return "[\n"+
					"{\n"+
					"	\"label\": \""+label1+"\",\n"+
					"	\"queryEndpoint\": \""+getEndpoint(label1)+"\",\n"+
					"	\"updateEndpoint\": \""+getEndpoint(label1)+"\"\n"+
					"},\n"+
					"{\n"+
					"	\"label\": \""+label2+"\",\n"+
					"	\"queryEndpoint\": \""+getEndpoint(label2)+"\",\n"+
					"	\"updateEndpoint\": \""+getEndpoint(label2)+"\"\n"+
					"}\n"+
				"]";
	}
	
	public static String getEndpoint(String label) {
		return "http://www.theworldavatar.com/blazegraph/namespace/"+label+"/sparql";	
	}
}
