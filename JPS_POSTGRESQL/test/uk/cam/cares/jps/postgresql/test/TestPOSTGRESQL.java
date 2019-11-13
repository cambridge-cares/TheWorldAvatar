package uk.cam.cares.jps.postgresql.test;

import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.postgresql.RelationalDB;

public class TestPOSTGRESQL extends TestCase  {
	

	
	public void testextractdata() {
		//sg
		double xmin=11560879/*.832*/;
		double ymin=140107/*.739*/;
		double xmax=11563323/*.926*/;
		double ymax=143305/*.896*/;
		//hk
		 xmin=12693826.33/*.832*/;
		 ymin=2535141.08 /*.739*/;
		 xmax=12720831.57/*.926*/;
		 ymax=2562311.02 /*.896*/;
		   JSONObject jo = new JSONObject();
		   
		   JSONObject scope = new JSONObject();
		   JSONObject low = new JSONObject();
		   JSONObject up = new JSONObject();
		   up.put("upperx", xmax);
		   up.put("uppery", ymax);
		   low.put("lowerx", xmin);
		   low.put("lowery", ymin);
		   scope.put("lowercorner", low);
		   scope.put("uppercorner", up);
		   jo.put("region",scope);
		   
		   
		   
			JSONWriter jsonInput = new JSONStringer().object().
					key("region").object()
						.key("srsname").value("EPSG:3857")
						.key("lowercorner").object()
							.key("lowerx").value(xmin)
							.key("lowery").value(ymin).endObject()
						.key("uppercorner").object()
							.key("upperx").value(xmax)
							.key("uppery").value(ymax).endObject()
					.endObject()
					.key("reactionmechanism").value("http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001")
					.endObject(); 
		   
		   System.out.println("json="+jo.toString());
		   //System.out.println("json="+jsoninput.toString());
		   
		   String result = AgentCaller.executeGetWithJsonParameter("/JPS_POSTGRESQL/getEntitiesWithinRegion", jo.toString());
		  
		   System.out.println("result of the ship= "+result);
		   JSONObject res = new JSONObject(result);
		   System.out.println("amount of data ="+res.getJSONObject("collection").getJSONArray("items").length());
	}
	
	public void testCheckShipResultFromRelationalDB() {
		//sg
		double xmin=11560879/*.832*/;
		double ymin=140107/*.739*/;
		double xmax=11563323/*.926*/;
		double ymax=143305/*.896*/;
		
		//hk
//		 xmin=12693826.33/*.832*/;
//		 ymin=2535141.08 /*.739*/;
//		 xmax=12720831.57/*.926*/;
//		 ymax=2562311.02 /*.896*/;
		   JSONObject jo = new JSONObject();
		   
		   JSONObject scope = new JSONObject();
		   JSONObject low = new JSONObject();
		   JSONObject up = new JSONObject();
		   up.put("upperx", xmax);
		   up.put("uppery", ymax);
		   low.put("lowerx", xmin);
		   low.put("lowery", ymin);
		   scope.put("lowercorner", low);
		   scope.put("uppercorner", up);
		   jo.put("region",scope);
		   
		   System.out.println("json="+jo.toString());
		   //System.out.println("json="+jsoninput.toString());
		   
		   String result = AgentCaller.executeGetWithURLAndJSON("http://www.theworldavatar.com:80/JPS_POSTGRESQL/getEntitiesWithinRegion", jo.toString());
		   //String result = doGetRelationalDB(new JSONObject(jo.toString()));
		   System.out.println("result of the ship= "+result);
		   JSONObject res = new JSONObject(result);
		   System.out.println("amount of data ="+res.getJSONObject("collection").getJSONArray("items").length());
	}
	
	private String doGetRelationalDB(JSONObject input) {
        JSONObject region = input.getJSONObject("region");
        System.out.println(region.getJSONObject("lowercorner").get("lowerx"));
        double xmin = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowerx"));
        double xmax = Double.parseDouble("" + region.getJSONObject("uppercorner").get("upperx"));
        double ymin = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowery"));
        double ymax = Double.parseDouble("" + region.getJSONObject("uppercorner").get("uppery"));
        double[] pmin = CRSTransformer.transform("EPSG:3857", "EPSG:4326", new double[]{xmin, ymin});
        double[] pmax = CRSTransformer.transform("EPSG:3857", "EPSG:4326", new double[]{xmax, ymax});
        xmin = pmin[0];
        ymin = pmin[1];
        xmax = pmax[0];
        ymax = pmax[1];
        System.out.println("minimum point= " + pmin[0] + " " + pmin[1]);
        System.out.println("maximum point= " + pmax[0] + " " + pmax[1]);

        JSONStringer result = RelationalDB.getEntitiesWithinRegion(xmin, xmax, ymin, ymax);
        return result.toString();
	}
	
	public void testcalculationforterrain() {
		int add=2900;
		int xmin=833044;
		int ymin=816015;
		int xmax=834498;
		int ymax=817460;
		
		//newset
		xmin=821182;
		ymin=806448;
		xmax=846182;
		ymax=831448;
		
				
		int p=xmax-xmin;
		int l=ymax-ymin;
		int area=p*l;
		double area2=1.5*area;
//		double centerx=(xmin+xmax)/2;
//		double centery=(ymin+ymax)/2;
		int xminnew=(int)xmin-add;
		//System.out.println(xminnew);
		int xmaxnew=(int) xmax+add;
		int yminnew=(int) ymin-add;
		int ymaxnew=(int) ymax+add;
		double calc=(xmaxnew-xminnew)*(ymaxnew-yminnew);
//		System.out.println("calc="+calc);
//		System.out.println("area="+area2);
		if(calc>=area2){
			System.out.println("xmin for terrain= "+xminnew);
			System.out.println("ymin for terrain= "+yminnew);
			System.out.println("xmax for terrain= "+xmaxnew);
			System.out.println("ymax for terrain= "+ymaxnew);
			int counter=1;
			int distance=16;
			
			for(int b=yminnew;b<=ymaxnew;b+=distance) {
				for (int a=xminnew;a<=xmaxnew;a+=distance) {

					if(b>816840) {
						//System.out.println(counter+","+(xminnew+((a-1)*distance))+","+(yminnew+((b)*distance))+","+"0.0");
					}
					else {
						if(b>816600) {
							//System.out.println(counter+","+ a +","+ b +","+"50.0");
						}
						else{
							//System.out.println(counter+","+a+","+b+","+"200.0");
						}
					}
					counter++;
				}
			}
//			for(int b=0;b<100;b++) {
//				for (int a=1;a<=60;a++) {
//
//					if(yminnew+((b)*distance)>816840) {
//						//System.out.println(counter+","+(xminnew+((a-1)*distance))+","+(yminnew+((b)*distance))+","+"0.0");
//					}
//					else {
//						if(yminnew+((b)*distance)>816600) {
//							System.out.println(counter+","+(xminnew+((a-1)*distance))+","+(yminnew+((b)*distance))+","+"50.0");
//						}
//						else{
//							System.out.println(counter+","+(xminnew+((a-1)*distance))+","+(yminnew+((b)*distance))+","+"200.0");
//						}
//					}
//					counter++;
//				}
//			}
			//System.out.println(counter+","+(xmaxnew)+","+(ymaxnew)+","+"0.0");
			counter++;
			//System.out.println(counter+","+(xminnew)+","+(ymaxnew)+","+"0.0");
			
		}
		
		
		
	}
}
