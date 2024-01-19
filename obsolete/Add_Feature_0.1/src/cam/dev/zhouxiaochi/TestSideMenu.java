package cam.dev.zhouxiaochi;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;  
import javax.swing.JTree;  
import javax.swing.tree.DefaultTreeModel;  
  
public class TestSideMenu   
{  
    public static void main(String[] args)  
    {  
        JFrame frame = new JFrame("CheckBoxTreeDemo");  
        frame.setBounds(200, 200, 400, 400);  

        SideMenu panel;
		try {
			panel = new SideMenu();
			JScrollPane scroll = new JScrollPane(panel);
		       scroll.setBounds(0, 0, 300, 320);  

	        frame.getContentPane().add(scroll);  
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}  

          
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  
        frame.setVisible(true);  
    }  
    
    public static Boolean unselectCallBack(String a){
    	return true;
    }
    public static Boolean selectCallBack(String a){
    	return true;
    }
}  