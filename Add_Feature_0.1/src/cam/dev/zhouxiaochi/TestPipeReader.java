package cam.dev.zhouxiaochi;

import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.esri.core.internal.util.e;

public class TestPipeReader {

	public static void main(String[] args){
		
		try {
			List result = PipeReader.getInstance().getPipelist();
			for (Object pipeOb : result){
				PipeReader.PipeInfo mPipeInfo = (PipeReader.PipeInfo)pipeOb;
				Map<String, Object> attributes = mPipeInfo.attriList;
				System.out.println("___________________________");
		       for (Map.Entry<String, Object> entry : attributes.entrySet()){
		    	   System.out.println("+++++++"+entry.getKey()+": "+entry.getValue());
		       }
		       System.out.println("Printing attributes name list:++++++++++++");
		       
			
			
			}
			
			LinkedHashSet<String> attriList = (LinkedHashSet<String>) PipeReader.getInstance().getAttriNameSet();
			Iterator<String> it = attriList.iterator();
			while(it.hasNext()){
				System.out.println(it.next());
			}
			
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
}
