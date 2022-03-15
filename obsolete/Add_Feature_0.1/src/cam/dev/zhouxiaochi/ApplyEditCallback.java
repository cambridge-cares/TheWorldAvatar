package cam.dev.zhouxiaochi;

import com.esri.core.map.CallbackListener;
import com.esri.core.map.FeatureEditResult;


public class ApplyEditCallback implements CallbackListener<FeatureEditResult[][]> {
private String head = "";
	enum EditType{
		ADD,
		DELETE,
		UPDATE;
		
		static EditType getEditType(int id){
			switch(id){
			case 0:
				return EditType.ADD;
			case 1:
				return EditType.DELETE;
			case 2:
				return EditType.UPDATE;
			default:
				return null;
			}
						
		}
	}
	public final static int FEATURE_EDIT_RESULT_ROW_NUM = 3;



	public ApplyEditCallback( ) {
	}
	
	public ApplyEditCallback(String head) {
		super();
		this.head = head;
	}
	
	@Override
	public void onError(Throwable e) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void onCallback(FeatureEditResult[][] results) {
		 for(int iRow =0 ; iRow < FEATURE_EDIT_RESULT_ROW_NUM; iRow++){
			 FeatureEditResult[]  resultRow = results[iRow];

			 if(resultRow.length > 0){//This row has result?
				//print out result
				 System.out.println(head+" Operation:"+EditType.getEditType(iRow));
				 
				 for(int iOp = 0; iOp < resultRow.length;iOp++){
					 System.out.print(iOp+": ");
					 if(resultRow[iOp].isSuccess()){//is operation successful?
					 System.out.print("success");//then print out success
					 } else{//not success
						 System.out.print(resultRow[iOp].getError().getCode());//print out err

					 }
					 System.out.println("");
					 System.out.println(resultRow[iOp].getError().getDescription());

				 }
			 }
		 }
		 
	}

	
}
