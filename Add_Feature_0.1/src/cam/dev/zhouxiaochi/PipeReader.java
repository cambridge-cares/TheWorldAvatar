package cam.dev.zhouxiaochi;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.esri.core.geometry.Point;

import cam.dev.zhouxiaochi.PipeReader.PipeInfo;
/**
 * Read Pipe info from OWL. Currently not used because line segment info is not fully constructed in OWL.
 * @author Shaocong
 *
 */
public class PipeReader {

	private static PipeReader singelton;
	private List<String>  pipeNames = new ArrayList<String>();
	
	private Set<String> attriNameSet= new LinkedHashSet<String>();//A string set contains all attri names
	
	public class Coordi{
		
		public double x;
		public double y;
		public Coordi(double x, double y){
			this.x = x;
			this.y = y;
		}
	}
	
	
	public class PipeInfo{
		public Map<String,Object> attriList; 
		public List path;
		public PipeInfo(Map<String, Object> attriList, List path) {
			super();
			this.attriList = attriList;
			this.path = path;
		}
		
	}
	
	private List<PipeInfo> pipeList = new ArrayList<PipeInfo>();
	//private List pathLists = new ArrayList<ArrayList<Point>>();
	
	//private List attriLists = new ArrayList<HashMap<String,Object>>();
	public static PipeReader getInstance() throws IOException, Exception{
		
		if(singelton == null){
			singelton = new PipeReader();
		}
		return singelton;
	}
	
	
	private PipeReader() throws IOException, Exception{
		
		/********filter to get all pipeline nodes******/
		//////get all owlReader first level nodes
		ArrayList<String> allNodeNames = OWLReader.read_owl_file("owl/Bio3.owl", null);
		///////filter through to get a list of top level pipe nodes
		for(String nodeName : allNodeNames){////DOES THE NAME MATCH THIS PATTERN?
			if(nodeName.matches("^Pipe_\\d+-\\d+$")){//=>YES
				//////=>add name of the pipe name list
             pipeNames.add(nodeName);
			}			
		}
	
		/****extract data from pipeline nodes****/
		for(Object pipeName : pipeNames){		/////loop through all pipes
			OWLReader.read_owl_file("owl/Bio3.owl", (String)pipeName);
			                                                                    System.out.println("read pipe:"+(String)pipeName);
			//extract and pack inrto pathLists
			List<Point> path = new ArrayList<Point>();
			List<String> valueList = OWLReader.value_list;
			
			List<String> nameList = OWLReader.name_list;
		
			List<String> coordList = new ArrayList<String>();
			 Map<String,Object> attributes = new LinkedHashMap<String,Object>();

			
			//filter to get coordis value
			for(int idxValue = 0; idxValue < valueList.size(); idxValue++){
				String nameStr = (String)(nameList.get(idxValue));
				if(nameStr.matches("^V_[x|y]_.+$")){//Is this attribute a coordi value or unit?
					if(!nameStr.contains("Unit")){//add coordi to list only if is a value and not a unit
					coordList.add(valueList.get(idxValue));
					}
				} else{//values not coordis?
					//=>Add them to attribute list!
					String attriName = (String) (nameList.get(idxValue));
					
					//delete id and add attribute into attribute name list
					Pattern p = Pattern.compile("(\\w+)(_?\\d+_\\d+)(_?\\w*)");//define regex pattern
					Matcher m = p.matcher(attriName);
					StringBuffer result = new StringBuffer();
					boolean found = false;//flag: if indeed find a match of regex
					while (m.find()) {// find a match?				
						m.appendReplacement(result, m.group(1)+m.group(3));//delete the id part of attri name 
					found = true;//set flag found to be true
					}
					m.appendTail(result);
					if(found){//indeed has match?
						attriNameSet.add(result.toString());
						attributes.put(result.toString(), valueList.get(idxValue));

					}	
					
				}
			}

			if(coordList.size()>=2){//Indeed have correct number of coordinate values?[At least 2 to form a line]
				//=>YES! Now pack coordis into path and add this path and its attributes to overall list!
				System.out.println("found a line");

				for(int idxCoor = 0 ; idxCoor < coordList.size()/2; idxCoor++){//pack coordinate values into path(Point list)
				double x = Double.parseDouble((String) coordList.get(idxCoor*2));
				double y = Double.parseDouble((String) coordList.get(idxCoor*2+1));

				Point point = new Point(x,y);
				path.add(point);
			}

			pipeList.add(new PipeInfo(attributes, path));//pack pipe info(coordis & attributes) into list
			} 
		}
		

}
	
	public List getPipelist(){
		return pipeList;
		
	}
	
	public Set getAttriNameSet(){
		return attriNameSet;
	}

}
