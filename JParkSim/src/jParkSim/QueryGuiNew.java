/**
 * This part of code is to design a new query window for J-Park Simulator by following the code provided by Ming
 * author Li ZHOU
 */
package jParkSim;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;

import javax.swing.JFrame;
import javax.swing.JScrollPane;

import com.esri.core.symbol.SimpleLineSymbol;

public class QueryGuiNew extends javax.swing.JFrame {
    
	/**This part of code took sample code from Ming Pan
	 * author Li Zhou
	 */
	public static String GIS = new String("D:/JPS_Code_Test/GIS.CSV"); 
	private static final long serialVersionUID = 1L;
	private final static String newline = "\n";
	private javax.swing.JPanel jPanel;
	private javax.swing.JLabel jLabel;
	private javax.swing.JTextField QueryContent;
	private javax.swing.JButton btnQuery;
	javax.swing.JButton btnShowLocation;
	javax.swing.JButton btnClear;
	private javax.swing.JTextArea QueryResult;
	
	
	static ArrayList<String> GISLocation = null;
	
	public QueryGuiNew() {
        initComponentsNew();
    }		
	
	/**
	 * This method is called from within the constructor to initialize the form.
	 */
	 @SuppressWarnings("unchecked")
	 private void initComponentsNew() {
		 
		 jPanel = new javax.swing.JPanel();
		 jLabel = new javax.swing.JLabel();
		 QueryContent = new javax.swing.JTextField();
		 btnQuery = new javax.swing.JButton();
		 btnClear = new javax.swing.JButton();
		 btnShowLocation = new javax.swing.JButton();
		 QueryResult = new javax.swing.JTextArea();
		 		 
		 QueryResult.setRows(100);
		 QueryResult.setAutoscrolls(true);
		 QueryResult.setLineWrap(true);
		 
		 JScrollPane areaScrollPane = new JScrollPane(QueryResult);
		 areaScrollPane.setVerticalScrollBarPolicy(
		                 JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		 areaScrollPane.setPreferredSize(new Dimension(250, 250));
		 
		 setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		 jPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Natural Language Query  --Powered by J-Park Simulator", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Tahoma", 1, 14)));
		 
		 jLabel.setFont(new java.awt.Font("Tahoma", 1, 14));
	     jLabel.setText("Please enter your query here:");
		 
	     QueryContent.setFont(new java.awt.Font("Tahoma", 0, 14));
	     
		 javax.swing.GroupLayout jPanelLayout = new javax.swing.GroupLayout(jPanel);
		 jPanel.setLayout(jPanelLayout);
		 
		 btnQuery.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
		 btnQuery.setText("Query");
		 btnQuery.addActionListener(new java.awt.event.ActionListener() {
			    @Override
	            public void actionPerformed(java.awt.event.ActionEvent evt) {
			    	HttpURLConnection urlCon;
					OutputStreamWriter out;
					InputStreamReader in;
					URL url;
					
					StringBuilder QueryT = new StringBuilder();
					
					QueryT.append(QueryContent.getText());
										
		    		try {
		    			url = new URL("http://172.25.182.41/QUERYServlet/");  // URL of servlet
						urlCon = (HttpURLConnection) url.openConnection();
						urlCon.setRequestMethod("POST");
						urlCon.setDoOutput(true);				
						out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
						
						if(QueryContent.getText().equals("")){
							QueryResult.append("Sorry, we cannot proceed without your solid query question =]");
							
						}else{
							StringBuilder outputString = new StringBuilder();							
							outputString.append (URLEncoder.encode("QueryT", "UTF-8"));
							outputString.append ("=");				
							outputString.append (URLEncoder.encode(QueryT.toString(), "UTF-8"));
							
							System.out.println("outputString: "+outputString);
							
							DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
							wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
							wr.flush();
							wr.close();
							
							if(urlCon.getResponseCode()==200){
								
								in = new InputStreamReader(urlCon.getInputStream());
								final BufferedReader br = new BufferedReader(in);
								String strTemp = null;								
								
								String[] Str = null;
								String[] StrI = null;								
								
								strTemp = br.readLine();
								Str = strTemp.split(",");
								
								for(int i=0; i< Str.length; i++){
						        	StrI = Str[i].split("%");
						        	for(int j=0; j<StrI.length;j++){
						        		System.out.println("check here: "+StrI[j]);
						        		if(!StrI[j].equals(" null")){	
						        			QueryResult.append(StrI[j] + newline);
						        		}						        		
						        	}	
						        	QueryResult.append(newline);
						        } 
								GISLocation(Str);
							}
							out.close();
						}
						
		    		} catch (IOException e) {
						e.printStackTrace();
					}
	            }
	        });
		 
/** set a clear button in order to clear the query content and result in order to perform new query*/		 
		 btnClear.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
		 btnClear.setText("Clear");
		 btnClear.addActionListener(new java.awt.event.ActionListener() {
			 @Override
	            public void actionPerformed(java.awt.event.ActionEvent evt) {
				 QueryContent.setText("");
				 QueryResult.setText("");
			 }
		 });
 /** add new button "Show Location" to the new query window*/		 
		 btnShowLocation.setFont(new java.awt.Font("Tahoma", 1, 12)); // set font "Tahoma", bold, size 12
		 btnShowLocation.setText("Show entity location on the map");			
		 
/** The visual design of the new query JFrame*/		 
		 jPanelLayout.setHorizontalGroup(
				 jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
				 .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
						 .addGroup(jPanelLayout.createSequentialGroup()
						         .addGap(0, 10, 151)
						         .addComponent(jLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 210, javax.swing.GroupLayout.PREFERRED_SIZE)
						         .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
						         .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
						        		 .addComponent(QueryContent, javax.swing.GroupLayout.PREFERRED_SIZE, 400, javax.swing.GroupLayout.PREFERRED_SIZE)))
						 .addGroup(jPanelLayout.createSequentialGroup()
								 .addGap(0, 1, 15)
								 .addComponent(btnQuery)
								 .addGap(0, 2, 15)
			                     .addComponent(btnShowLocation)
			                     .addGap(0, 2, 15)
			                     .addComponent(btnClear))
						 .addGap(0, 2, 15)
						 .addComponent(QueryResult, javax.swing.GroupLayout.PREFERRED_SIZE, 650, javax.swing.GroupLayout.PREFERRED_SIZE))				  				 
				 );
		 
		 jPanelLayout.setVerticalGroup(
				 jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)	
				 .addGroup(jPanelLayout.createSequentialGroup()
						 .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								 .addComponent(jLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
		                         .addComponent(QueryContent, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
			                .addGap(1, 10, 100)
			              .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
			            		  .addComponent(btnQuery)
	            				  .addComponent(btnShowLocation)
			                      .addComponent(btnClear)) 
			              .addGap(1, 10, 100)
			              .addComponent(QueryResult, javax.swing.GroupLayout.PREFERRED_SIZE, 400, javax.swing.GroupLayout.PREFERRED_SIZE)
			              .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
			                       ) 
			                )				                                                   
				 );
		 		 
		 javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
	        getContentPane().setLayout(layout);
	        layout.setHorizontalGroup(
	            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
	            .addGroup(layout.createSequentialGroup()
	                .addContainerGap()
	                .addComponent(jPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
	                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
	        );
	        layout.setVerticalGroup(
	            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
	            .addGroup(layout.createSequentialGroup()
	                .addContainerGap()
	                .addComponent(jPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
	                .addGap(0, 10, 1000))
	        );
	        pack();
	 }	 
	 /** end of vidual design of the new query JFrame
	 * @throws IOException */		 
	 
	 //extract the GIS Coordinate and return it
	 public static void GISLocation(String[] String) throws IOException{
		 String[] Str = String;
		 String[] StrI = null;
		 String[] StrGIS = null;  //new string to store the GIS information 
		 String[] StrGISx = null;  //new string to store the GIS x coordinate 
		 String[] StrGISy = null;  //new string to store the GIS y coordinate 	
		    
		    FileWriter GPS = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
			GPS = new FileWriter(GIS);				
		    
			for(int i=0; i< Str.length; i++){
	        	StrI = Str[i].split("%");
	        	if(StrI.length>1){
	         		StrGIS = StrI[1].split(" ");
	    	        	System.out.println(StrGIS.length);
	    	        	StrGISx = new String[10];
	    	        	StrGISy = new String[10];							        	
	    	        		    	        	
	    	        	if(StrGIS[0].startsWith("G")){
	    	        		StrGISx[i]= StrGIS [1];
	    	        		StrGISy[i]= StrGIS [3];
	    	        		GPS.append(StrGISx[i]);
	    	        		GPS.append(",");
	    	        		GPS.append(StrGISy[i]);
	    	        	}
	    	        	GPS.append(newline);
	         	}
			}
			GPS.flush();
			GPS.close();		 		
 	}
/*	 
	 public static void main(String args[]) {
	        		 
	        try {
	            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
	                if ("Nimbus".equals(info.getName())) {
	                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
	                    break;
	                }
	            }
	        } catch (ClassNotFoundException ex) {
	            java.util.logging.Logger.getLogger(QueryGuiNew.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
	        } catch (InstantiationException ex) {
	            java.util.logging.Logger.getLogger(QueryGuiNew.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
	        } catch (IllegalAccessException ex) {
	            java.util.logging.Logger.getLogger(QueryGuiNew.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
	        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
	            java.util.logging.Logger.getLogger(QueryGuiNew.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
	        }
	        //</editor-fold>

	        
	        java.awt.EventQueue.invokeLater(new Runnable() {
	            public void run() {
	                new QueryGuiNew().setVisible(true);	                
	            }
	        });
	        System.out.println("here we are" + GISLocation);
	    }*/

}
