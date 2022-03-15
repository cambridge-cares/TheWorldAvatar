/**
 * This part of code is to design a new optimization Frame for J-Park Simulator
 * author Li ZHOU
 */
package jParkSim;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

public class OptimizationGUI extends javax.swing.JFrame {

	/**
	 * 
	 */
	public static String PlantGIS = new String("D:/JPS_Code_Test/PlantGIS.CSV");    // csv file to store the x and y coordinate of the endpoints for the polygons that representing the chemical plant
	
	private static final long serialVersionUID = 1L;
	private javax.swing.JPanel jPanel;
	private javax.swing.JTextField PlantName;
	private javax.swing.JTextField P1_Name;
	private javax.swing.JTextField P2_Name;
	private javax.swing.JTextField P1_Value;
	private javax.swing.JTextField P2_Value;
	private javax.swing.JTextField P1_Unit;
	private javax.swing.JTextField P2_Unit;
	private javax.swing.JTextField x1_Name;
	private javax.swing.JTextField x2_Name;
	private javax.swing.JTextField x3_Name;
	private javax.swing.JTextField x4_Name;
	private javax.swing.JTextField x5_Name;
	private javax.swing.JTextField x6_Name;
	private javax.swing.JTextField x1_Value;
	private javax.swing.JTextField x2_Value;
	private javax.swing.JTextField x3_Value;
	private javax.swing.JTextField x4_Value;
	private javax.swing.JTextField x5_Value;
	private javax.swing.JTextField x6_Value;
	private javax.swing.JTextField x1_Unit;
	private javax.swing.JTextField x2_Unit;
	private javax.swing.JTextField x3_Unit;
	private javax.swing.JTextField x4_Unit;
	private javax.swing.JTextField x5_Unit;
	private javax.swing.JTextField x6_Unit;
	private javax.swing.JLabel jLabel;	
	private javax.swing.JLabel jLabel_UserSpecify;
	private javax.swing.JLabel jLabel_OperationalParameters;
	private javax.swing.JLabel jLabel_P_Name;
	private javax.swing.JLabel jLabel_P_Value;
	private javax.swing.JLabel jLabel_P_Unit;
	private javax.swing.JLabel jLabel_P1;
	private javax.swing.JLabel jLabel_P2;
	private javax.swing.JLabel jLabel_x1;
	private javax.swing.JLabel jLabel_x2;
	private javax.swing.JLabel jLabel_x3;
	private javax.swing.JLabel jLabel_x4;
	private javax.swing.JLabel jLabel_x5;
	private javax.swing.JLabel jLabel_x6;
	javax.swing.JButton btnLoadModel;
	javax.swing.JButton btnOptimize;
	static javax.swing.JButton btnClear;
	static javax.swing.JButton btnShowPlantLocation;
	
	public OptimizationGUI() {
        initComponentsNew();
    }
	
	private void initComponentsNew() {
		jPanel = new javax.swing.JPanel();
		PlantName = new javax.swing.JTextField();
		P1_Name = new javax.swing.JTextField();
		P2_Name = new javax.swing.JTextField();
		P1_Value = new javax.swing.JTextField();
		P2_Value = new javax.swing.JTextField();
		P1_Unit = new javax.swing.JTextField();
		P2_Unit = new javax.swing.JTextField();
		x1_Name = new javax.swing.JTextField();
		x2_Name = new javax.swing.JTextField();
		x3_Name = new javax.swing.JTextField();
		x4_Name = new javax.swing.JTextField();
		x5_Name = new javax.swing.JTextField();
		x6_Name = new javax.swing.JTextField();
		x1_Value = new javax.swing.JTextField();
		x2_Value = new javax.swing.JTextField();
		x3_Value = new javax.swing.JTextField();
		x4_Value = new javax.swing.JTextField();
		x5_Value = new javax.swing.JTextField();
		x6_Value = new javax.swing.JTextField();
		x1_Unit = new javax.swing.JTextField();
		x2_Unit = new javax.swing.JTextField();
		x3_Unit = new javax.swing.JTextField();
		x4_Unit = new javax.swing.JTextField();
		x5_Unit = new javax.swing.JTextField();
		x6_Unit = new javax.swing.JTextField();
		jLabel = new javax.swing.JLabel();		
		jLabel_UserSpecify = new javax.swing.JLabel();
		jLabel_OperationalParameters = new javax.swing.JLabel();
		jLabel_P_Name = new javax.swing.JLabel();
		jLabel_P_Value = new javax.swing.JLabel();
		jLabel_P_Unit = new javax.swing.JLabel();
		jLabel_P1 = new javax.swing.JLabel();
		jLabel_P2 = new javax.swing.JLabel();
		jLabel_x1 = new javax.swing.JLabel();
		jLabel_x2 = new javax.swing.JLabel();
		jLabel_x3 = new javax.swing.JLabel();
		jLabel_x4 = new javax.swing.JLabel();
		jLabel_x5 = new javax.swing.JLabel();
		jLabel_x6 = new javax.swing.JLabel();
		
		btnLoadModel = new javax.swing.JButton();
		btnOptimize = new javax.swing.JButton();
		btnClear = new javax.swing.JButton();
		btnShowPlantLocation = new javax.swing.JButton();
		
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		jPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Plant Optimizer  --Powered by J-Park Simulator", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Tahoma", 1, 14)));
		 
		PlantName.setFont(new java.awt.Font("Tahoma", 0, 14)); 
		P1_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		P2_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		P1_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		P2_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		P1_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		P2_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		x1_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		x2_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		x3_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		x4_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		x5_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		x6_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
		x1_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		x2_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		x3_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		x4_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		x5_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		x6_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
		x1_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		x2_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		x3_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		x4_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		x5_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		x6_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
		
		jLabel.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel.setText("Plant Name:");	    	    	    
	    jLabel_UserSpecify.setFont(new java.awt.Font("Tahoma", 1, 15));
	    jLabel_UserSpecify.setText("User Specified Parameters");
	    jLabel_OperationalParameters.setFont(new java.awt.Font("Tahoma", 1, 15));
	    jLabel_OperationalParameters.setText("Operational Parameters");
	    jLabel_P_Name.setFont(new java.awt.Font("Tahoma", 0, 14));
	    jLabel_P_Name.setText("Name");	    
	    jLabel_P_Value.setFont(new java.awt.Font("Tahoma", 0, 14));
	    jLabel_P_Value.setText("Value");	    
	    jLabel_P_Unit.setFont(new java.awt.Font("Tahoma", 0, 14));
	    jLabel_P_Unit.setText("Unit");	 
	    
	    jLabel_P1.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_P1.setText("P1:");
	    jLabel_P2.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_P2.setText("P2:");
	    jLabel_x1.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_x1.setText("x1:");
	    jLabel_x2.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_x2.setText("x2:");
	    jLabel_x3.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_x3.setText("x3:");
	    jLabel_x4.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_x4.setText("x4:");
	    jLabel_x5.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_x5.setText("x5:");
	    jLabel_x6.setFont(new java.awt.Font("Tahoma", 1, 14));
	    jLabel_x6.setText("x6:");
	    
	    /** add new button "Load Model" to Load information for the model*/		 
	    btnLoadModel.setFont(new java.awt.Font("Tahoma", 1, 12)); // set font "Tahoma", bold, size 12
	    btnLoadModel.setText("Load Model");	    
	    btnLoadModel.addActionListener(new java.awt.event.ActionListener() {
	    	@Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
		    	HttpURLConnection urlCon;
				OutputStreamWriter out;
				InputStreamReader in;
				URL url;
				
				StringBuilder Flag = new StringBuilder();
				StringBuilder OptT = new StringBuilder();
				
				OptT.append(PlantName.getText());
				Flag.append("Load");
									
	    		try {
	    			url = new URL("http://172.25.182.41/OptServlet/");  // URL of servlet
					urlCon = (HttpURLConnection) url.openConnection();
					urlCon.setRequestMethod("POST");
					urlCon.setDoOutput(true);				
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					
					if(PlantName.getText().equals("")){
						PlantName.setText("Please specify mame.");
						
					}else{
						StringBuilder outputString = new StringBuilder();							
						outputString.append (URLEncoder.encode("OptT", "UTF-8"));
						outputString.append ("=");				
						outputString.append (URLEncoder.encode(OptT.toString(), "UTF-8"));
						outputString.append("&");
						outputString.append(URLEncoder.encode("Flag", "UTF-8"));
						outputString.append("=");
						outputString.append(URLEncoder.encode(Flag.toString(), "UTF-8"));
						outputString.append("&");
						outputString.append(URLEncoder.encode("Parameter1", "UTF-8"));
						outputString.append("=");
						outputString.append(URLEncoder.encode("", "UTF-8"));
						outputString.append("&");
						outputString.append(URLEncoder.encode("Parameter2", "UTF-8"));
						outputString.append("=");
						outputString.append(URLEncoder.encode("", "UTF-8"));
						
						System.out.println("outputString: "+outputString);
						
						DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
						wr.writeBytes(outputString.toString());                                  // write query string into servlet doPost() method
						wr.flush();
						wr.close();
						
						if(urlCon.getResponseCode()==200){		
							in = new InputStreamReader(urlCon.getInputStream());
							final BufferedReader br = new BufferedReader(in);
							String strTemp = null;	

							String[] Str = null;								
							
							strTemp = br.readLine();
							Str = strTemp.split("coordinate")[0].split(",");
							
							P1_Name.setText(Str[1]);
							P1_Unit.setText(Str[2]);
							P2_Name.setText(Str[3]);
							P2_Unit.setText(Str[4]);
							x1_Name.setText(Str[7]);
							x1_Unit.setText(Str[8]);
							x2_Name.setText(Str[9]);
							x2_Unit.setText(Str[10]);
							x3_Name.setText(Str[11]);
							x3_Unit.setText(Str[12]);
							x4_Name.setText(Str[13]);
							x4_Unit.setText(Str[14]);
							x5_Name.setText(Str[15]);
							x5_Unit.setText(Str[16]);
							x6_Name.setText(Str[17]);
							x6_Unit.setText(Str[18]);
							
							/**create a file writer to generate the csv file to store the GIS coordinate for the polygon*/
							FileWriter fileWriter = null; 
							fileWriter = new FileWriter(PlantGIS);
							/**extract the x and y coordinate*/
							String[] GISx = null, GISy = null;
							GISx = strTemp.split("coordinate")[1].split(",");
							GISy = strTemp.split("coordinate")[2].split(",");
							fileWriter.append("x_coordinate" + ", "+ "y_coordinate");
							for(int i=0; i<GISx.length; i++){
								fileWriter.append(GISx[i]);
								fileWriter.append(",");
								fileWriter.append(GISy[i]);
								fileWriter.append("\n");     // start a new line
							}
							
							fileWriter.flush();
							fileWriter.close();
							
						}
						out.close();
					}
					
	    		} catch (IOException e) {
					e.printStackTrace();
				}
            }
        });
	    
	    /** add new button "Optimize" to Optimize the model*/		 
	    btnOptimize.setFont(new java.awt.Font("Tahoma", 1, 12)); // set font "Tahoma", bold, size 12
	    btnOptimize.setText("Optimize");
	    btnOptimize.addActionListener(new java.awt.event.ActionListener() {
	    	@Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
		    	HttpURLConnection urlCon;
				OutputStreamWriter out;
				InputStreamReader in;
				URL url;
				
				StringBuilder Flag = new StringBuilder();
				StringBuilder Parameter1 = new StringBuilder();
				StringBuilder Parameter2 = new StringBuilder();
				
				Flag.append("Optimize");
				Parameter1.append(P1_Value.getText());
				Parameter2.append(P2_Value.getText());
									
	    		try {
	    			url = new URL("http://172.25.182.41/OptServlet/");  // URL of servlet
					urlCon = (HttpURLConnection) url.openConnection();
					urlCon.setRequestMethod("POST");
					urlCon.setDoOutput(true);				
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					
					if(Parameter1.equals("")){
						System.out.println("Please type in the user specified parameter.");
						
					}else{
						StringBuilder outputString = new StringBuilder();
						outputString.append(URLEncoder.encode("OptT", "UTF-8"));
						outputString.append("=");				
						outputString.append(URLEncoder.encode("", "UTF-8"));
						outputString.append("&");
						outputString.append(URLEncoder.encode("Flag", "UTF-8"));
						outputString.append("=");				
						outputString.append(URLEncoder.encode(Flag.toString(), "UTF-8"));
						outputString.append("&");
						outputString.append(URLEncoder.encode("Parameter1", "UTF-8"));
						outputString.append("=");
						outputString.append(URLEncoder.encode(Parameter1.toString(), "UTF-8"));
						outputString.append("&");
						outputString.append(URLEncoder.encode("Parameter2", "UTF-8"));
						outputString.append("=");
						outputString.append(URLEncoder.encode(Parameter2.toString(), "UTF-8"));
						
						System.out.println("outputString: "+outputString);
						
						DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
						wr.writeBytes(outputString.toString());                                  // write query string into servlet doPost() method
						wr.flush();
						wr.close();
						
						if(urlCon.getResponseCode()==200){
							System.out.println("OooooHoooooOooo");						
							in = new InputStreamReader(urlCon.getInputStream());
							final BufferedReader br = new BufferedReader(in);
							String strTemp = null;	

							String[] Str = null;								
							
							strTemp = br.readLine();
							Str = strTemp.split(",");
							
							x1_Value.setText(Str[0]);
							x2_Value.setText(Str[1]);
							x3_Value.setText(Str[2]);
							x4_Value.setText(Str[3]);
							x5_Value.setText(Str[4]);
							x6_Value.setText(Str[5]);
							
						}
						out.close();
					}
					
	    		} catch (IOException e) {
					e.printStackTrace();
				}
            }
        });
	    
	    /** add new button "Clear" to Clear the current task*/		 
	    btnClear.setFont(new java.awt.Font("Tahoma", 1, 12)); // set font "Tahoma", bold, size 12
	    btnClear.setText("Clear");
	    btnClear.addActionListener(new java.awt.event.ActionListener() {
			 @Override
	            public void actionPerformed(java.awt.event.ActionEvent evt) {
				 PlantName.setText("");
				 P1_Value.setText("");
				 P2_Value.setText("");
				 x1_Value.setText("");
				 x2_Value.setText("");
				 x3_Value.setText("");
				 x4_Value.setText("");
				 x5_Value.setText("");
				 x6_Value.setText("");
			 }
		 });
	    
	    /** add new button "Show Location" to the new query window*/		 
	    btnShowPlantLocation.setFont(new java.awt.Font("Tahoma", 1, 12)); // set font "Tahoma", bold, size 12
		btnShowPlantLocation.setText("Show plant location on the map");
	    
	    javax.swing.GroupLayout jPanelLayout = new javax.swing.GroupLayout(jPanel);
		jPanel.setLayout(jPanelLayout);
	    
	    /** The visual design of the new query JFrame*/		
	    jPanelLayout.setHorizontalGroup(
	    		jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
	    		.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addComponent(jLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 90, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
	    						.addComponent(PlantName, javax.swing.GroupLayout.PREFERRED_SIZE, 200, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 2, 15)
	    						.addComponent(btnLoadModel)
	    						.addGap(0, 2, 15)
	    						.addComponent(btnOptimize)
	    						.addGap(0, 2, 15)
	    						.addComponent(btnClear))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addComponent(jLabel_UserSpecify, javax.swing.GroupLayout.PREFERRED_SIZE, 230, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 60, 100)
	    						.addComponent(btnShowPlantLocation, javax.swing.GroupLayout.PREFERRED_SIZE, 250, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 50, 151)
	    						.addComponent(jLabel_P_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
	    						.addComponent(jLabel_P_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
	    						.addComponent(jLabel_P_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_P1, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P1_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(P1_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(P1_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						)
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_P2, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P2_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(P2_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(P2_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))	    				
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addComponent(jLabel_OperationalParameters, javax.swing.GroupLayout.PREFERRED_SIZE, 230, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_x1, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x1_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(x1_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(x1_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_x2, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x2_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(x2_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(x2_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_x3, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x3_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(x3_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(x3_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_x4, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x4_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(x4_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(x4_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_x5, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x5_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(x5_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(x5_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createSequentialGroup()
	    						.addGap(0, 2, 15)
	    						.addComponent(jLabel_x6, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x6_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 30, 30)
	    						.addComponent(x6_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 15, 15)
	    						.addComponent(x6_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				
	    				
	    				)
	    		
	    		
	    );
	    
	    jPanelLayout.setVerticalGroup(
	    		jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)	
	    		.addGroup(jPanelLayout.createSequentialGroup()
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
								 .addComponent(jLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
		                         .addComponent(PlantName, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
		                         .addComponent(btnLoadModel)
		                         .addComponent(btnOptimize)
		                         .addComponent(btnClear))
	    				.addGap(1, 10, 10)
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_UserSpecify, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(btnShowPlantLocation, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGap(1, 10, 10)
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_P_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 5, 15)
	    						.addComponent(jLabel_P_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addGap(0, 5, 15)
	    						.addComponent(jLabel_P_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGap(1, 10, 100)
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_P1, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P1_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P1_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P1_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGap(1, 10, 100)
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_P2, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P2_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P2_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(P2_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_OperationalParameters, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_x1, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x1_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x1_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x1_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_x2, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x2_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x2_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x2_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_x3, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x3_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x3_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x3_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_x4, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x4_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x4_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x4_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_x5, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x5_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x5_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x5_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
	    				.addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
	    						.addComponent(jLabel_x6, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x6_Name, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x6_Value, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
	    						.addComponent(x6_Unit, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
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
	
	public static void main(String args[]) {
		 
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(OptimizationGUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(OptimizationGUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(OptimizationGUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(OptimizationGUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>

        
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new OptimizationGUI().setVisible(true);	                
            }
        });

    }
	
}
