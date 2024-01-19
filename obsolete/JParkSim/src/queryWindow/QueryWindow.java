package queryWindow;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

@SuppressWarnings("serial")
public abstract class QueryWindow extends JFrame implements ActionListener{
 
	static JLabel labels[] = new JLabel[10];
	static JLabel labelsA = new JLabel();
	
	public static void showQueryWindow(String queryResult){
		/**Separate the string by entity*/
		String[] StrTem = null;
		String[] StrI =null;
		StrTem = queryResult.split(",");
				
		
		//create a new JFrame container
		JFrame jfrm = new JFrame ("J-Park Simulator Query result");
		
		//jPanel1 = new javax.swing.JPanel();
		
		//specify flowlayout for the layout manager
		jfrm.setLayout(new FlowLayout());
		
		//give the frame an initial size
		jfrm.setSize(1050, 150);
		jfrm.setBackground(Color.BLUE);
		jfrm.setLocationRelativeTo(null);
		
		//terminate the program when the user closes the application
		jfrm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		//create a lable
		labels[0] = new JLabel ("Result");
		labels[0].setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
		labels[0].setText("The query result we want: \n");
        jfrm.add(labels[0]);
        for(int i=0; i< StrTem.length; i++){
        	StrI = StrTem[i].split("%");
        	for(int j=0; j<StrI.length;j++){
        		if(StrI[j]!=null){
        			labels[j+1] = new JLabel ("CenterLeft", SwingConstants.LEFT);
                	labels[j+1].setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N        
                	labels[j+1].setText("\n" + StrI[j]);
                	//add the label to the frame
                	jfrm.add(labels[j+1]);
        		}
        	}
        labelsA.setText("\n");
        jfrm.add(labelsA);
        }      
		            
		//display the frame
		jfrm.setVisible(true);
	}
	
}
