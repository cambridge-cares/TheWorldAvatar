package cam.dev.zhouxiaochi;

import java.awt.Dimension;
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
import java.util.HashSet;
import java.util.Set;

import javax.swing.JScrollPane;

public class QueryGuiNew extends javax.swing.JFrame{
	   
	
 
	
	
		/**This part of code took sample code from Ming Pan
		 * author Li Zhou
		 */
		public static String GIS = new String("GIS.CSV"); 
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
			 		 
			 QueryResult.setSize(800, 200);
			 QueryResult.setRows(800);
			 QueryResult.setColumns(400);
			 QueryResult.setAutoscrolls(true);
			 QueryResult.setLineWrap(true);
			 
			 JScrollPane areaScrollPane = new JScrollPane(QueryResult);
			 areaScrollPane.setVerticalScrollBarPolicy(
			                 JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			 areaScrollPane.setPreferredSize(new Dimension(600, 600));
			// this.getContentPane().add(areaScrollPane);
			 setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		//	 jPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(null, "Natural Language Query  --Powered by J-Park Simulator", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Tahoma", 1, 14)));
			 jPanel.setSize(1200, 400);
			 jLabel.setFont(new java.awt.Font("Tahoma", 1, 14));
		     jLabel.setText("Please enter your query here:");
			 
		     QueryContent.setFont(new java.awt.Font("Tahoma", 0, 14));
		     QueryContent.setSize(500, 2000);
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
									
								//	String[] Str = null;
									String[] StrI = null;								
									
									strTemp = br.readLine();
									
									
									 Set<String> Devices = new HashSet<String>();
									 Set<String> Connectors = new HashSet<String>();
									 Set<String> X = new HashSet<String>();
									 Set<String> Y = new HashSet<String>();
									 Set<String> Inputs = new HashSet<String>();
									 Set<String> Outputs = new HashSet<String>();
									 Set<String> HeatDuties = new HashSet<String>();
									 Set<String> Reaction_NetWork = new HashSet<String>();
									 Set<String> Input_Flow = new HashSet<String>();
									 Set<String> Output_Flow = new HashSet<String>();
									 
									 Set<String> Output_ma = new HashSet<String>();
									 Set<String>  Input_ma = new HashSet<String>();
									 
									 
									 
									 ArrayList<Set<String>> sets = new ArrayList<Set<String>>();
						
									 
									 
									
									for(String line : strTemp.split(","))
									{
									//	QueryResult.append(line);
									//	QueryResult.append("\n");
										
										System.out.println("---->" + line);
										
										
										String name = line.split("---")[0].trim();
										String value = line.split("---")[1].trim();
										
										if(value.contains("#"))
										
										{
											value = value.split("#")[1];
											
										}
										
										System.out.println("name---" + name);
										
										
										if(name.contentEquals("x"))
										{
											Devices.add(value);
											System.out.print("found");
 										}
										
										if(name.contentEquals("connector"))
										{
											Connectors.add(value);
										}
										
										if(name.contentEquals("GCxnv"))
										{
											X.add(value);
										}
										
										if(name.contentEquals("GCynv"))
										{
											Y.add(value);
										}
										
										if(name.contentEquals("Input"))
										{
											Inputs.add(value);
										}

										if(name.contentEquals("Output"))
										{
											Outputs.add(value);
										}
										
										if(name.contentEquals("heatduty_value"))
										{
											HeatDuties.add(value);
										}
										
										if(name.contentEquals("reaction_network"))
										{
											Reaction_NetWork.add(value);
										}
										
										if(name.contentEquals("output_flowrate_value"))
										{
											Output_Flow.add(value);
										}
										
										if(name.contentEquals("input_flowrate_value"))
										{
											Input_Flow.add(value);
										}
										
										if(name.contentEquals("flowrate_v"))
										{
											Input_ma.add(value);
										}
										
										
										if(name.contentEquals("flowrate_v_out"))
										{
											Output_ma.add(value);
										}
										
										
										
										
									}
									
									 sets.add(Devices);
									 sets.add(Connectors);
									 sets.add(X);
									 sets.add(Y);
									 sets.add(Inputs);
									 sets.add(Outputs);
									 sets.add(Reaction_NetWork);
									 sets.add(HeatDuties);
									 sets.add(Input_Flow);
									 sets.add(Output_Flow);
									 sets.add(Input_ma);
									 sets.add(Output_ma);
									
									for(Set<String> set : sets)
									{
										for(String item : set)
										{
											System.out.println("|---------|" + item);
										}
										
										System.out.println("==============================================");
									}
				
									
									
									String placeholder = " 			|-------:  ";
									
									QueryResult.append("Device: " + "\n");
									
									QueryResult.append(placeholder + getItem(Devices,0) + "\n");
									
									QueryResult.append("Connectors:  "+ "\n");
									QueryResult.append(placeholder + getItem(Connectors,0) + "\n");
									
									
									QueryResult.append(placeholder + getItem(Connectors,2) + "\n");
									QueryResult.append(placeholder + getItem(Connectors,4) + "\n");
									
									QueryResult.append("Coordinate_X: " + getItem(X,0) + "\n");
									QueryResult.append("Coordinate_Y: " + getItem(Y,0) + "\n");

									QueryResult.append("Inputs: "+ "\n");
									QueryResult.append(placeholder +   getItem(Inputs,2) + "\n");
									QueryResult.append(placeholder + getItem(Inputs,3) + "\n");
									 
									QueryResult.append("Outputs: "  + "\n");
									QueryResult.append(placeholder + getItem(Outputs,0) + "\n");
									
									QueryResult.append("Reaction_NetWork: " + "\n");
									QueryResult.append(placeholder + getItem(Reaction_NetWork,0) + "\n");
									
									QueryResult.append("HeatDuty: " + "\n");
									QueryResult.append(placeholder + getItem(HeatDuties,0) + "\n");
										
						
									QueryResult.append("Input_Flows: "  + "\n");
									QueryResult.append(placeholder + getItem(Input_ma,1) + "\n");
									QueryResult.append(placeholder + getItem(Input_ma,2) + "\n");	
									QueryResult.append(placeholder + getItem(Input_ma,5) + "\n");
									QueryResult.append(placeholder  + getItem(Input_ma,7) + "\n");
									
									QueryResult.append("Input_Flows_Rate: " + "\n");
									QueryResult.append(placeholder + getItem(Input_Flow,1) + "\n");
									QueryResult.append(placeholder + getItem(Input_Flow,2) + "\n");	
									QueryResult.append(placeholder + getItem(Input_Flow,5) + "\n");
									QueryResult.append(placeholder  + getItem(Input_Flow,7) + "\n");
									
									QueryResult.append("Output_Flows: " + "\n");
									QueryResult.append(placeholder + getItem(Output_ma,2) + "\n");

									QueryResult.append(placeholder + getItem(Output_ma,3) + "\n");	
								
									
									QueryResult.append("Output_Flows_Rate: " + "\n");
									QueryResult.append(placeholder+ getItem(Output_Flow,2) + "\n");

									QueryResult.append(placeholder + getItem(Output_Flow,3) + "\n");								
									
									QueryResult.append("=======================================\n");
									
									
									
									/*
									QueryResult.append("Device: " + getItem(Devices,1) + "\n");
									QueryResult.append("Connectors:  " + getItem(Connectors,1) + "\n");
									QueryResult.append(placeholder + getItem(Connectors,2) + "\n");
									QueryResult.append(placeholder + getItem(Connectors,5) + "\n");
									
									QueryResult.append("Coordinate_X: " + getItem(X,1) + "\n");
									QueryResult.append("Coordinate_Y: " + getItem(Y,1) + "\n");

									QueryResult.append("Inputs: " + getItem(Inputs,0) + "\n");
									QueryResult.append(placeholder + getItem(Inputs,1) + "\n");
									 
									QueryResult.append("Outputs: " + getItem(Outputs,1) + "\n");
									
									QueryResult.append("Reaction_NetWork: " + getItem(Reaction_NetWork,1) + "\n");
									
									
									QueryResult.append("HeatDuty: " + getItem(HeatDuties,1) + "\n");
										
						
									QueryResult.append("Input_Flows: " + getItem(Input_ma,0) + "\n");
									QueryResult.append(placeholder + getItem(Input_ma,3) + "\n");	
									QueryResult.append(placeholder + getItem(Input_ma,4) + "\n");
									QueryResult.append(placeholder  + getItem(Input_ma,6) + "\n");
									
									QueryResult.append("Input_Flows_Rate: " + getItem(Input_Flow,0) + "\n");
									QueryResult.append(placeholder + getItem(Input_Flow,3) + "\n");	
									QueryResult.append(placeholder + getItem(Input_Flow,4) + "\n");
									QueryResult.append(placeholder  + getItem(Input_Flow,6) + "\n");
									
									QueryResult.append("Output_Flows: " + getItem(Output_ma,0) + "\n");
									QueryResult.append(placeholder + getItem(Input_ma,1) + "\n");	
								
									
									QueryResult.append("Output_Flows_Rate: " + getItem(Output_Flow,0) + "\n");
									QueryResult.append(placeholder + getItem(Output_Flow,1) + "\n");
									
									*/
									
									
									
									QueryResult.append("Device: " + "\n");
									
									QueryResult.append(placeholder + getItem(Devices,1) + "\n");
									
									QueryResult.append("Connectors:  "+ "\n");
									QueryResult.append(placeholder + getItem(Connectors,1) + "\n");
									
									
									QueryResult.append(placeholder + getItem(Connectors,3) + "\n");
									QueryResult.append(placeholder + getItem(Connectors,5) + "\n");
									
									QueryResult.append("Coordinate_X: " + getItem(X,1) + "\n");
									QueryResult.append("Coordinate_Y: " + getItem(Y,1) + "\n");

									QueryResult.append("Inputs: "+ "\n");
									QueryResult.append(placeholder +   getItem(Inputs,0) + "\n");
									QueryResult.append(placeholder + getItem(Inputs,1) + "\n");
									 
									QueryResult.append("Outputs: "  + "\n");
									QueryResult.append(placeholder + getItem(Outputs,1) + "\n");
									
									QueryResult.append("Reaction_NetWork: " + "\n");
									QueryResult.append(placeholder + getItem(Reaction_NetWork,1) + "\n");
									
									QueryResult.append("HeatDuty: " + "\n");
									QueryResult.append(placeholder + getItem(HeatDuties,1) + "\n");
										
						
									QueryResult.append("Input_Flows: "  + "\n");
									QueryResult.append(placeholder + getItem(Input_ma,0) + "\n");
									QueryResult.append(placeholder + getItem(Input_ma,3) + "\n");	
									QueryResult.append(placeholder + getItem(Input_ma,4) + "\n");
									QueryResult.append(placeholder  + getItem(Input_ma,6) + "\n");
									
									QueryResult.append("Input_Flows_Rate: " + "\n");
									QueryResult.append(placeholder + getItem(Input_Flow,0) + "\n");
									QueryResult.append(placeholder + getItem(Input_Flow,3) + "\n");	
									QueryResult.append(placeholder + getItem(Input_Flow,4) + "\n");
									QueryResult.append(placeholder  + getItem(Input_Flow,6) + "\n");
									
									QueryResult.append("Output_Flows: " + "\n");
									QueryResult.append(placeholder + getItem(Output_ma,0) + "\n");

									QueryResult.append(placeholder + getItem(Output_ma,1) + "\n");	
								
									
									QueryResult.append("Output_Flows_Rate: " + "\n");
									QueryResult.append(placeholder+ getItem(Output_Flow,0) + "\n");

									QueryResult.append(placeholder + getItem(Output_Flow,1) + "\n");	
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									
									/*
									
	QueryResult.append("Device"+ "|---------|" + "HasConnector"+ "|---------|" +"X_coordinate"+ "|---------|" +"Y_coordinate"+ "|---------|" + "Input" + "|---------|" + "Output"+ "|---------|"
											+ "HasReactionNetwork"+ "|---------|" + "HasHeatDuty"+ "|---------|" +"InputFlowrate"+ "|---------|" + "OutputFlowrate"+ "|---------|" + "Input_Material"+ "|---------|" + "Output_Material" );								
	QueryResult.append("\n");								
	QueryResult.append(getItem(Devices,0)+ "|---------|" + getItem(Connectors,0)+ "|---------|" + getItem(X,0)+ "|---------|" + getItem(Y,0)+ "|---------|" + getItem(Inputs,0)+ "|---------|" + getItem(Outputs,0)+ "|---------|"
	+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,0)+ "|---------|" + getItem(Input_Flow,0)+ "|---------|" + getItem(Output_Flow,0)+ "|---------|" + getItem(Input_ma,0)+ "|---------|" +  getItem(Output_ma,0) );
									
	QueryResult.append("\n");
	QueryResult.append(getItem(Devices,0)+ "|---------|" + getItem(Connectors,2)+ "|---------|" + getItem(X,0)+ "|---------|" + getItem(Y,0)+ "|---------|" + getItem(Inputs,1)+ "|---------|" + getItem(Outputs,1)+ "|---------|"
	+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,1)+ "|---------|" + getItem(Input_Flow,1)+ "|---------|" + getItem(Output_Flow,1)+ "|---------|" + getItem(Input_ma,1)+ "|---------|" +  getItem(Output_ma,0) );
	
	QueryResult.append("\n");
	QueryResult.append(getItem(Devices,0)+ "|---------|" + getItem(Connectors,4)+ "|---------|" + getItem(X,0)+ "|---------|" + getItem(Y,0)+ "|---------|" + getItem(Inputs,3)+ "|---------|" + getItem(Outputs,3)+ "|---------|"
	+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,2)+ "|---------|" + getItem(Input_Flow,0)+ "|---------|" + getItem(Output_Flow,2)+ "|---------|" + getItem(Input_ma,2)+ "|---------|" +  getItem(Output_ma,0) );
	
	QueryResult.append("\n");
	QueryResult.append(getItem(Devices,0)+ "|---------|" + getItem(Connectors,7)+ "|---------|" + getItem(X,0)+ "|---------|" + getItem(Y,0)+ "|---------|" + getItem(Inputs,4)+ "|---------|" + getItem(Outputs,4)+ "|---------|"
	+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,3)+ "|---------|" + getItem(Input_Flow,4)+ "|---------|" + getItem(Output_Flow,3)+ "|---------|" + getItem(Input_ma,3)+ "|---------|" +  getItem(Output_ma,4) );
	
	QueryResult.append("\n");
	
	QueryResult.append(getItem(Devices,1)+ "|---------|" + getItem(Connectors,1)+ "|---------|" + getItem(X,1)+ "|---------|" + getItem(Y,1)+ "|---------|" + getItem(Inputs,0)+ "|---------|" + getItem(Outputs,5)+ "|---------|"
			+ getItem(Reaction_NetWork,1)+ "|---------|" + getItem(HeatDuties,4)+ "|---------|" + getItem(Input_Flow,4)+ "|---------|" + getItem(Output_Flow,4)+ "|---------|" + getItem(Input_ma,4)+ "|---------|" +  getItem(Output_ma,4) );
			
			QueryResult.append("\n");
			
	QueryResult.append(getItem(Devices,1)+ "|---------|" + getItem(Connectors,3)+ "|---------|" + getItem(X,1)+ "|---------|" + getItem(Y,1)+ "|---------|" + getItem(Inputs,1)+ "|---------|" + getItem(Outputs,4)+ "|---------|"
					+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,5)+ "|---------|" + getItem(Input_Flow,4)+ "|---------|" + getItem(Output_Flow,5)+ "|---------|" + getItem(Input_ma,5)+ "|---------|" +  getItem(Output_ma,4) );
					
					QueryResult.append("\n");
					
	QueryResult.append(getItem(Devices,1)+ "|---------|" + getItem(Connectors,5)+ "|---------|" + getItem(X,1)+ "|---------|" + getItem(Y,1)+ "|---------|" + getItem(Inputs,2)+ "|---------|" + getItem(Outputs,4)+ "|---------|"
							+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,6)+ "|---------|" + getItem(Input_Flow,4)+ "|---------|" + getItem(Output_Flow,6)+ "|---------|" + getItem(Input_ma,6)+ "|---------|" +  getItem(Output_ma,4) );
							
							QueryResult.append("\n");
							
	QueryResult.append(getItem(Devices,1)+ "|---------|" + getItem(Connectors,7)+ "|---------|" + getItem(X,1)+ "|---------|" + getItem(Y,1)+ "|---------|" + getItem(Inputs,3)+ "|---------|" + getItem(Outputs,4)+ "|---------|"
													+ getItem(Reaction_NetWork,0)+ "|---------|" + getItem(HeatDuties,0)+ "|---------|" + getItem(Input_Flow,4)+ "|---------|" + getItem(Output_Flow,4)+ "|---------|" + getItem(Input_ma,4)+ "|---------|" +  getItem(Output_ma,4) );
													
													QueryResult.append("\n"); */
	
									
									/*
									
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
									
									*/
									String[] Str = new String[X.size()];
									for(int i = 0; i < X.size();i++)
									{
										String x = getItem(X,i);
										String y = getItem(Y,i);
										Str[i] = x + "," + y;
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
							        		 .addComponent(QueryContent, javax.swing.GroupLayout.PREFERRED_SIZE, 1200, javax.swing.GroupLayout.PREFERRED_SIZE)))
							 .addGroup(jPanelLayout.createSequentialGroup()
									 .addGap(0, 1, 15)
									 .addComponent(btnQuery)
									 .addGap(0, 2, 15)
				                     .addComponent(btnShowLocation)
				                     .addGap(0, 2, 15)
				                     .addComponent(btnClear))
							 .addGap(0, 2, 15)
							 .addComponent(areaScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 1600, javax.swing.GroupLayout.PREFERRED_SIZE))				  				 
					 );
			 
			 jPanelLayout.setVerticalGroup(
					 jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)	
					 .addGroup(jPanelLayout.createSequentialGroup()
							 .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
									 .addComponent(jLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
			                         .addComponent(QueryContent, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
				                .addGap(1, 100, 100)
				              .addGroup(jPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
				            		  .addComponent(btnQuery)
		            				  .addComponent(btnShowLocation)
				                      .addComponent(btnClear)) 
				              .addGap(1, 1000, 1000)
				              .addComponent(areaScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 900, javax.swing.GroupLayout.PREFERRED_SIZE)
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
		                .addGap(0, 1000, 1500))
		        );
		        jPanel.setSize(1200, 400);
		        areaScrollPane.setSize(1200, 400);
		        pack();
		 }	 
		 /** end of vidual design of the new query JFrame
		 * @throws IOException */		 
		 
		 //extract the GIS Coordinate and return it
		 public static void GISLocation(String[] String) throws IOException{
			
			    FileWriter GPS = null;  
						   GPS = new FileWriter(GIS);
			/*
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
				
				*/
				
				
				for(String line : String)
				{
					GPS.append(line);
					GPS.append(newline);
				}
				

				
				GPS.flush();
				GPS.close();		 		
	 	}
		 
		 public static String getItem(Set<String> set, int index)
		 {
			 String result = null;
			 int counter = 0;
			 for(String item : set)
			 {
				 if(counter == index)
				 {
					 result = item;
				 }
				 
				 counter++;
			 }
			 
			return result;
		 }
		 
}
