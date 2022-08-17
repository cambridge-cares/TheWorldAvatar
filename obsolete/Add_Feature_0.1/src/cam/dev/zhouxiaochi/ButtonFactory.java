package cam.dev.zhouxiaochi;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.DataOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JOptionPane;

import com.esri.map.ArcGISFeatureLayer;


public class ButtonFactory {


	String[] nameList;
	BtnInfo[] infoList;
	ArrayList<String[]> editStack;
	   ArcGISFeatureLayer[] completeLayerList;
	   public static final int WINDOW_WIDTH = 1200;
	   public static final int WINDOW_HEIGHT = 900;
 public static final int maxPerCol = 4;
	   public static final int BTN_WIDTH = 190;
	   public static final int BTN_WIDTH_PADDING = 10;
	   public static final int BTN_HEIGHT = 30;
	   public static final int BTN_HEIGHT_PADDING = 20;
	   public static final int BTN_AREA_LEFT_PADDING = 600;
	   public static final int BTN_AREA_UP_PADDING = 10;

	   

		
	

	public ButtonFactory(String[] nameList, BtnInfo[] infoList){
	this.nameList = nameList;
	this.infoList = infoList;	
	
	}	
		 
	public JButton[] createButtons(){
		JButton[] btns = new JButton[nameList.length];
		int tempX = BTN_AREA_LEFT_PADDING, tempY = BTN_AREA_UP_PADDING;
		for (int i = 0 ; i < nameList.length ; i++){
			
			btns[i] = createButton(nameList[i],infoList[i]);
			btns[i].setSize(BTN_WIDTH,BTN_HEIGHT);
			btns[i].setLocation(tempX, tempY);
			System.out.println("btn position:"+tempX + "  " + tempY);
	        tempY += BTN_HEIGHT+BTN_HEIGHT_PADDING;
	        if(i%maxPerCol == 3) {
	        	tempX += BTN_WIDTH+BTN_WIDTH_PADDING;
	        	tempY = BTN_AREA_UP_PADDING;//reset y to start a new col
	  	        }
			btns[i].setEnabled(true);
			btns[i].setVisible(true);
		}		
		return btns;
	}	
	
	private JButton createButton( String name, BtnInfo aBtnInfo){
		JButton aBtn = new JButton(name);
		//setSize
		//set Location
		///TODO: auto adjust size and location? do a grid?
		
		//ActionListener aAL = createActionListener(aBtnInfo.type, aBtnInfo);
		aBtn.addActionListener(aBtnInfo.createListener());
        return aBtn;		
	}
		

	
	
}
