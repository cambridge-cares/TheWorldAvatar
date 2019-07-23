package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
/***
 * 
 * @author Shaocong
 *
 *Defines method to create action listener for button to load OWl data
 Implements BtnInfo Interface.
 */
public class LoadOwlBtnInfo implements BtnInfo {

	@Override
	/***
	 * creates Actionlistener for LoadOwnBtn
	 */
	public ActionListener createListener() {
		// TODO Auto-generated method stub
		return new ActionListener() {
	    	@Override
	    	public void actionPerformed(ActionEvent arg0) {
				try {
					App.main(null);
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
		    		
	    	}
	    };
	}

}
