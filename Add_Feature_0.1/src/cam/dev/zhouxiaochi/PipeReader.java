package cam.dev.zhouxiaochi;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.esri.core.geometry.Point;
//TODO: test pipeReader class
public class PipeReader {

	private static PipeReader singelton;
	private List  pipeNames = new ArrayList<String>();
	
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
	
	private List pipeList = new ArrayList<ArrayList<PipeInfo>>();
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
		ArrayList<String> allNodeNames = OWLReader.read_owl_file(App.PLANT_OWL_FILE_NAME, null);
		///////filter through to get a list of top level pipe nodes
		for(String nodeName : allNodeNames){////DOES THE NAME MATCH THIS PATTERN?
			if(nodeName.matches("^Pipe_\\d+-\\d+$")){//=>YES
				//////=>add name of the pipe name list
             pipeNames.add(nodeName);
			}			
		}
	
		/****extract data from pipeline nodes****/
		for(Object pipeName : pipeNames){		/////loop through all pipes
			OWLReader.read_owl_file(App.PLANT_OWL_FILE_NAME, (String)pipeName);
			                                                                    System.out.println("read pipe:"+(String)pipeName);
			//extract and pack inrto pathLists
			List path = new ArrayList<Point>();
			List valueList = OWLReader.value_list;
			
			List nameList = OWLReader.name_list;
		
			List coordList = new ArrayList<String>();
			 Map<String,Object> attributes = new HashMap<String,Object>();

			
			//filter to get coordis value
			for(int idxValue = 0; idxValue < valueList.size(); idxValue++){
				String nameStr = (String)(nameList.get(idxValue));
				if(nameStr.matches("^V_[x|y]_.+$")){//Is this attribute a coordi value or unit?
					if(!nameStr.contains("Unit")){//add coordi to list only if is a value and not a unit
					coordList.add(valueList.get(idxValue));
					}
				} else{//values not coordis?
					//=>Add them to attribute list!
					attributes.put((String) nameList.get(idxValue), valueList.get(idxValue));
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

}
