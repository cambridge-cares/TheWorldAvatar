package uk.ac.cam.cares.jps.episode;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class EpisodeAgent extends DispersionModellingAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String separator="\t";
	
	double dx_rec=100; //TODO hardcoded? decide the dx for the receptor
	double dy_rec=100;//TODO hardcoded? decide the dy for the receptor
	double dx=1000.0 ;//decide the dx for the scope
	double dy=1000.0 ;//decide the dy for the scope	
	double dz=10;
	double nz=13;
	double upperheight=75.0;
	double lowerheight=2.0;
			
	
	@Override
	public void createWeatherInput(String dataPath, String filename,List<String>stniri) {	
		
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
				+ "PREFIX j6:<http://www.w3.org/2006/time#>" 
				+ "SELECT ?entity ?class ?propval ?proptimeval ?graph "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{ GRAPH ?graph "
				+ "{ "
				 
				+ "  ?entity j4:observes ?prop ." 
				+ " ?prop a ?class ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY DESC(?proptimeval)LIMIT 9";
//		String dataseturl="";//which is the weather stn dataset
//		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, sensorinfo);
//		 String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
//		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		RepositoryConnection con = WeatherAgent.repo.getConnection();
		 List<String[]> listmap = new ArrayList<String[]>();
			TupleQuery query = con.prepareTupleQuery(QueryLanguage.SPARQL, sensorinfo);
			TupleQueryResult result = query.evaluate();
			int d = 0;
			try {
				while (result.hasNext()) {
					BindingSet bindingSet = result.next();
					//String iri = bindingSet.getValue("graph").stringValue();
					String classprop = bindingSet.getValue("class").stringValue();
					String propval = bindingSet.getValue("propval").stringValue();
					String dateval = bindingSet.getValue("proptimeval").stringValue();
					String graphval = bindingSet.getValue("graph").stringValue();
					String[] content = { classprop,propval,dateval,graphval};
					listmap.add(content);

					d++;
				}
				System.out.println("total data=" + d);

			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		 System.out.println("size="+listmap.size());
		 
		 List<String[]> resultquery = new ArrayList<String[]>();
			
			
			
			
	        String[]header= {"*","yyyy","mm","dd","hh","FF1","DD1","T25m","DT","RH%","PP_mm","Cloud","Press","FF2","DD2"};
	        resultquery.add(0,header);
	        String time=listmap.get(0)[2];
	        String[]content=new String[15];
	        content[0]="";
	        content[1]=time.split("-")[0];
	        content[2]=time.split("-")[1];
	        content[3]=time.split("-")[2].split("T")[0];
	        content[4]=time.split("-")[2].split("T")[1].split(":")[0];
	        for(int r=0;r<listmap.size();r++) {
//	        	System.out.println(listmap.get(r)[0]);
//	        	System.out.println(listmap.get(r)[3]);
	        	if(listmap.get(r)[3].toLowerCase().contains(stniri.get(1))) {
	        		System.out.println("it goes number 2");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed"))
	        		content[13]=listmap.get(r)[1];
	        		else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
	        			content[14]=listmap.get(r)[1];
	        		}
	        	}else if(listmap.get(r)[3].toLowerCase().contains(stniri.get(0))) {
	        		System.out.println("it goes number 1");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed")) {
		        		content[5]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
		        		content[6]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("temperature")) {
		        		content[7]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("humidity")) {
		        		String decimalhumidity=listmap.get(r)[1];
		        		double percent=Double.valueOf(decimalhumidity)*100;
		        		content[9]=""+percent;
		        	}else if(listmap.get(r)[0].toLowerCase().contains("precipitation")) {
		        		content[10]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("cloud")) {
		        		content[11]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("pressure")) {
		        		content[12]=listmap.get(r)[1];
		        	}
	        	}
	        }
	        content[8]="0.022"; //currently hardcoded but can be substituted by model
	        resultquery.add(content);
	        StringBuilder sb= new StringBuilder();
	       //convert to tsv
	        for(int v=0;v<resultquery.size();v++) {
		        for (int c=0;c<resultquery.get(0).length;c++) {
		        	sb.append(resultquery.get(v)[c]);
		        	if(resultquery.get(0).length-c!=1) {
		        		sb.append(separator);
		        	}
		        	else {
		        		sb.append("\n");
		        	}
		        }
	        	
	        }
	        new QueryBroker().putLocal(dataPath + "/"+filename, sb.toString());  	
	}


	public void createControlTopologyFile(List<String>srtmlist,JSONObject region,String dataPath, String Filename) throws IOException {
		JSONObject in= new JSONObject();
		JSONArray srtm= new JSONArray();
		for(int x=0;x<srtmlist.size();x++) {
			srtm.put(srtmlist.get(x));
		}
		in.put("srtminput",srtm);
		in.put("regioninput",region);
		String  modifiedcontent=modifyTemplate(Filename,in);
		 new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createControlWeatherFile(JSONObject region,String dataPath, String Filename) throws IOException {
		JSONObject in= new JSONObject();
		in.put("regioninput",region);
		String  modifiedcontent=modifyTemplate(Filename,in);
		 new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createReceptorFile(JSONObject inputparameter,String dataPath, String Filename) {
		String lowx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowerx").toString();
		String lowy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowery").toString();
		String upx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("upperx").toString();
		String upy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("uppery").toString();
	       

		double z_rec=9.5;//TODO hardcoded?
		double nx_rec =(Double.valueOf(upx)-Double.valueOf(lowx))/dx_rec;
		double ny_rec =(Double.valueOf(upy)-Double.valueOf(lowy))/dy_rec;
		
		//assume no monitor station is needed
		List<String[]>resultquery= new ArrayList<String[]>();
		String[]key= {"simid","nstation","nx_rec","ny_rec","dx_rec","dy_rec","z_rec","xsw_main","ysw_main","rcmax"};
		resultquery.add(key);
		String[] content= new String[key.length];
		content[0]="{Citychem-Singapore--testnotapm-00001}";
		content[1]="0"; //assume no monitor station
		content[2]=""+nx_rec;	
		content[3]=""+ny_rec;
		content[4]=""+dx_rec;
		content[5]=""+dy_rec;
		content[6]=""+z_rec;
		content[7]=lowx;
		content[8]=lowy;	
		content[9]="-9900";
		resultquery.add(content);
		
		
		
		//convert to tsv
		 StringBuilder sb= new StringBuilder();
        for(int v=0;v<resultquery.get(0).length;v++) {
	        for (int c=0;c<resultquery.size();c++) {
	        	sb.append(resultquery.get(c)[v]);
	        	if(c!=1) {
	        		sb.append(separator);
	        	}
	        	else {
	        		sb.append("\n");
	        	}
	        }
        	
        }
        
        new QueryBroker().putLocal(dataPath + "/"+Filename, sb.toString());
        
	}
	
	private double[] calculateCenterPoint(double xup, double yup,double xdown, double ydown) {
		double xcenter=(xup+xdown)/2;
		double ycenter=(yup+ydown)/2;
		double[]res= {xcenter,ycenter};
		return res;
	}
	private double[] calculateLowerLeftInit(double xup, double yup,double xdown, double ydown) {
		double xlowerinit=-1*(xup-xdown)/2;
		double ylowerinit=-1*(yup-ydown)/2;
		double[]res= {xlowerinit,ylowerinit};
		return res;
	}
	private double[] calculatenumberCellDetails(double xup, double yup,double xdown, double ydown, double dx, double dy) {
		// dx and dy should be  min 1000 m
		double nx=(xup-xdown)/dx;
		double ny=(yup-ydown)/dy;
		if(nx%5!=0||ny%5!=0) { //nx and ny should be factor of 5
			System.out.println("boundary conditions is not fulfilled");
			System.out.println("nx= "+nx);
			System.out.println("ny= "+ny);
			
			return null;
		}
		double[]ncell= {nx,ny};
		return ncell;
	}
	
	public String modifyTemplate(String filename,JSONObject inputparameter) throws IOException { 
		
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		String fileContext = FileUtils.readFileToString(file);
		String epsg=inputparameter.getJSONObject("regioninput").getJSONObject("region").get("srsname").toString();
		String lowx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowerx").toString();
		String lowy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowery").toString();
		String upx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("upperx").toString();
		String upy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("uppery").toString();

		
		
		//maybe need to convert it to UTM first??
		double proclowx=Double.valueOf(lowx);
		double procupx=Double.valueOf(upx);
		double proclowy=Double.valueOf(lowy);
		double procupy=Double.valueOf(upy);
		double[] center=calculateCenterPoint(procupx,procupy,proclowx,proclowy);
		double[] leftcorner=calculateLowerLeftInit(procupx,procupy,proclowx,proclowy);
		double[] ncell=calculatenumberCellDetails(procupx,procupy,proclowx,proclowy,dx,dy);
		
		if(filename.contentEquals("aermap.inp")) {
			String srtmin=inputparameter.getJSONArray("srtminput").get(0).toString();
			//templlate tif=N01E103.tif;
			String tif1="DATAFILE  "+"./srtm3/"+srtmin+"   tiffdebug";
			String tif2="DATAFILE  "+"./srtm3/N01E104.tif"+"   tiffdebug";
			fileContext = fileContext.replaceAll(tif1,""); //replace with the new tif
			fileContext = fileContext.replaceAll(tif2,""); //replace with the new tif
			//fileContext = fileContext.replaceAll("48",""); //replace with the new value of UTM coordinate system if needed
			fileContext = fileContext.replaceAll("330600.0",""+(proclowx-1000)); //replace with the new value xmin-1000
			fileContext = fileContext.replaceAll("118000.0",""+(proclowy-1000)); //replace with the new value ymin-1000
			fileContext = fileContext.replaceAll("402600.0",""+(procupx+1000)); //replace with the new value xmax+1000
			fileContext = fileContext.replaceAll("190000.0",""+(procupy+1000)); //replace with the new value ymax+1000
			fileContext = fileContext.replaceAll("366600.0",""+center[0]); //replace with the new value x center point
			fileContext = fileContext.replaceAll("154000.0",""+center[1]); //replace with the new value y center point
			fileContext = fileContext.replaceAll("-35000.0a",""+leftcorner[0]);
			fileContext = fileContext.replaceAll("35b",""+ncell[0]);
			fileContext = fileContext.replaceAll("2000.0c",""+dx);
			fileContext = fileContext.replaceAll("-35000.0d",""+leftcorner[1]);
			fileContext = fileContext.replaceAll("35e",""+ncell[1]);
			fileContext = fileContext.replaceAll("2000.0f",""+dy);
			
			return fileContext;

		}else if(filename.contentEquals("run_file.asc")) {
			//System.out.println("line 0= "+filename);
			String[]line=fileContext.split("\n");
			List<String> lineoffile=Arrays.asList(line);
			List<String> newcontent= new ArrayList<String>();
			for( int r=0;r<23;r++) {
				newcontent.add(lineoffile.get(r));
			}
			
			double[] pmidconvert = CRSTransformer.transform("EPSG:32648", "EPSG:4326", new double[] {center[0], center[1]});
			 DecimalFormat df= new DecimalFormat("#.#") ;
			 String xmid=df.format(pmidconvert[0]);
			 System.out.println("xmid="+xmid);
			 String ymid=df.format(pmidconvert[1]);
			 System.out.println("ymid="+ymid);
			 String line23=lineoffile.get(23);
			 line23=line23.replace("1.4", ymid);
			 line23=line23.replace("103.8", xmid);
			 System.out.println(line23);
			 newcontent.add(line23);
			 
			 String line24=lineoffile.get(24);
			 line24=line24.replace("-8","-8");//change if timezone is different
			 newcontent.add(line24);
			 newcontent.add(lineoffile.get(25));
			 String line26b=lineoffile.get(26).split("!")[1];
			 String line26=ncell[0]+separator+ncell[1]+separator+nz+separator+line26b;
			 newcontent.add(line26);
			 
			 String line27b=lineoffile.get(27).split("!")[1];
			 String line27=dx+separator+dy+separator+line27b;
			 newcontent.add(line27);
			 newcontent.add(lineoffile.get(28));//base with stretch factor
			 newcontent.add(lineoffile.get(29));//base
			 for(int r=1;r<nz;r++) {
				 newcontent.add(dz+separator+"!"+"\n");
			 }
			for( int r=42;r<73;r++) {
				newcontent.add(lineoffile.get(r));
			}
//			newcontent.add(name1+separator+lineoffile.get(73).split("!")[1]);
//			newcontent.add(loc1x+separator+locy+separator+lineoffile.get(74).split("!")[1]);
			newcontent.add(lineoffile.get(75));
//			newcontent.add(heighttemp+separator+lineoffile.get(76).split("!")[1]);
			newcontent.add(upperheight+separator+lineoffile.get(77).split("!")[1]);
			newcontent.add(lowerheight+separator+lineoffile.get(78).split("!")[1]);
			for( int r=79;r<82;r++) {
				newcontent.add(lineoffile.get(r));
			}
//			newcontent.add(name2+separator+lineoffile.get(82).split("!")[1];)
//			newcontent.add(loc2x+separator+loc2y+separator+lineoffile.get(83).split("!")[1]);
			for( int r=84;r<89;r++) {
				newcontent.add(lineoffile.get(r));
			}
			 //assume wind measurement height=const for both
				
					
			 String contentupdate="";
			 for (int x=0;x<newcontent.size();x++) {
				 contentupdate+=newcontent.get(x);
			 }
			return contentupdate;
		}
		
		
		return fileContext;
	}
}
