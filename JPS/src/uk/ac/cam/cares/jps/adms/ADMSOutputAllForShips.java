package uk.ac.cam.cares.jps.adms;


import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.SystemUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

/**
 * Servlet implementation class ADMSOutput
 */
@WebServlet("/ADMSOutputAllForShips")
public class ADMSOutputAllForShips extends HttpServlet {
    private static final long serialVersionUID = 1L;
    private static Logger logger = LoggerFactory.getLogger(ADMSOutputAllForShips.class);
	String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
	Path pyrelpath = SystemUtils.IS_OS_LINUX ? Paths.get("bin","python") : Paths.get("Scripts","python.exe");

    /**
     * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
     * get all adms output in one go
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        JSONObject joforEN = AgentCaller.readJsonParameter(request);
        request.setCharacterEncoding("UTF-8");
        // String folder = null;
        String folderfilename = joforEN.getString("folder");

        // this is required because unix file paths do not appear as IRIs to the triple store
        // so we have to add file:/ in front of the path
        if (!CommandHelper.isWindows()) {
            folderfilename = folderfilename.split("file:/")[1];
        }
        // X.Zhou@2020.5.9 Implemented an extra mechanism to identify the extension of the target file and trigger different conversion script 
        // accordingly. I also suggest a future clean up/ restructure of the GST conversion script. I personally suspect the maintainability 
        // and extensibility of this script 
        File output = new File(folderfilename);
        String folder = output.getParent();
        File dir = new File(folder);
        
        File[] gst_files = dir.listFiles((dir1, name) -> name.contains("test.levels") && name.endsWith(".gst"));
        File[] dat_files = dir.listFiles((dir1, name) -> name.endsWith(".dat"));
        
        System.out.println("GST files : " + Arrays.toString(gst_files));
        System.out.println("DAT files : " + Arrays.toString(dat_files));

        int n_gst = gst_files.length;
        int n_dat = dat_files.length;
        if ((n_gst + n_dat) == 0) {
        	response.getWriter().write("Error: empty foder");
        }
        else
        {
        	if (n_gst > n_dat) {
        		// it is a gst folder 
                get_output_for_gst(request, response, folder);
        	}else {
        		// it is a dat folder, trigger python
        		ArrayList<String> args = new ArrayList<String>();
        		if (AgentLocator.isJPSRunningAtCMCL()) {
        			Path pyexe = Paths.get(KeyValueMap.getInstance().get(IKeys.SPEED_LOAD_MAP_VENV_DIR),pyrelpath.toString());
        			args.add(pyexe.toString());
        		} else {
        			args.add("python");
        		}
        		args.add("dat_reader.py"); 
        		args.add(dat_files[0].getAbsolutePath());
        		String result = CommandHelper.executeCommands(targetFolder, args);
        		response.setContentType("application/json");
        		response.getWriter().write(result);
        		
        	}
        	
        }
        
         
        
        
        
    }
    
    
    public void get_output_for_gst(HttpServletRequest request, HttpServletResponse response, String folder) throws IOException {
    	
        try {
           

            String outputFile = folder + "/test.levels.gst";
			// get what file is stored in the folder 
			// DAT / GST
            String csv = new QueryBroker().readFileLocal(outputFile);
            List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
            int startcontentindex = 7;

            int heightamount = (simulationResult.get(0).length - startcontentindex) / getAllPollutants(simulationResult, startcontentindex).size();//height variation level amount (e.g:0m,10m,20m,30m) currently 4
            logger.info("number of height= " + heightamount);


            int numpol = findUniquePol(simulationResult, startcontentindex).size();  //number of polluttant (e.g:CO2,CO,NO2,..etc) with ozone and so2
            logger.info("number of pollutant= " + numpol);


//		TODO IS IT NEEDED IN THE FUTURE TO CHANGE THE GST PHYSICALLY????
            ArrayList<String[]> copier = new ArrayList<String[]>();
            int totalline = simulationResult.size();

            //hardcoded =  (numpol=10 then merged to 9), pollostmerging=1

            int pollostmerging = 10 - numpol;
            int newarrsize = simulationResult.get(0).length - (heightamount * pollostmerging);// 43


            //the header of a modified file
            String[] newheader = new String[newarrsize];
            //column index 0-6 is the same between old and modified file
            for (int line = 0; line < startcontentindex; line++) {
                newheader[line] = simulationResult.get(0)[line];
            }

            int counter1 = 0;
            int index = startcontentindex;
            for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level
                boolean flag10 = false;
                boolean flag25 = false;
                int line1 = startcontentindex;
                while (Double.valueOf(simulationResult.get(0)[line1].split("Z=")[1].split("m")[0])
                        - Double.valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {

                    if (simulationResult.get(0)[line1 + numpol * heightindex + counter1].contains("PM2.5")) {
                        if (flag25 == false) {
                            String headernamepm25 = simulationResult.get(0)[line1 + numpol * heightindex];
                            newheader[index] = headernamepm25.replace(headernamepm25.split("\\|")[2], "PM2.5");
                            flag25 = true;
                            index++;
                        }

                    } else if (simulationResult.get(0)[line1 + numpol * heightindex + counter1].contains("PM10")) {
                        if (flag10 == false) {
                            String headernamepm10 = simulationResult.get(0)[line1 + numpol * heightindex];
                            newheader[index] = headernamepm10.replace(headernamepm10.split("\\|")[2], "PM10");
                            flag10 = true;
                            index++;
                        }

                    } else {
                        newheader[index] = simulationResult.get(0)[line1 + numpol * heightindex + counter1];
                        index++;
                    }

                    line1++;

                }
                counter1 += pollostmerging; // how many pol lost after merging
                if (flag10 == false && flag25 == true) {
                    String headernamepm10 = simulationResult.get(0)[startcontentindex + 10 * heightindex];
                    //System.out.println(headernamepm10);
                    newheader[index] = headernamepm10.replace(headernamepm10.split("\\|")[2], "PM10");
                    flag10 = true;
                    //System.out.println(newheader[index]);
                    index++;
                }


            }
            copier.add(newheader);


            //the content of a modified file
            for (int t = 1; t < totalline; t++) { // row
                String[] newcontent = new String[newarrsize];
                for (int line = 0; line < startcontentindex; line++) {
                    newcontent[line] = simulationResult.get(t)[line];
                }
                int counter = 0;
                int indexcontent = startcontentindex;

                for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level
                    boolean flag10 = false;
                    boolean flag25 = false;
                    int line1 = startcontentindex;
                    int lockindexpm25 = 0;
                    double pm25 = 0;
                    int lockindexpm10 = 0;
                    double pm10 = 0;

                    while (Double.valueOf(simulationResult.get(0)[line1].split("Z=")[1].split("m")[0]) - Double
                            .valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {

                        if (simulationResult.get(0)[line1 + numpol * heightindex + counter].contains("PM2.5")) {
                            pm25 = pm25 + Double.valueOf(simulationResult.get(t)[line1 + numpol * heightindex + counter]);
                            if (flag25 == false) {

                                lockindexpm25 = indexcontent;
                                flag25 = true;
                                indexcontent++;
                            }

                        } else if (simulationResult.get(0)[line1 + numpol * heightindex + counter].contains("PM10")) {
                            pm10 = pm10 + Double.valueOf(simulationResult.get(t)[line1 + numpol * heightindex + counter]);
                            if (flag10 == false) {

                                lockindexpm10 = indexcontent;
                                flag10 = true;
                                indexcontent++;
                            }
                        } else {
                            newcontent[indexcontent] = simulationResult.get(t)[line1 + numpol * heightindex + counter];
                            // System.out.println("indexc= " + index);
                            indexcontent++;
                        }

                        line1++;
                    }
                    if (flag25 == true) {

                        newcontent[lockindexpm25] = "" + pm25;
                        if (flag10 == false) {
                            newcontent[lockindexpm25 + 1] = "" + (pm10 + pm25);
                            indexcontent++;
                        }
                        // System.out.println("index25= "+lockindexpm25);

                    }
                    if (flag10 == true) {
                        newcontent[lockindexpm10] = "" + (pm10 + pm25);
                        // System.out.println("index10= "+lockindexpm10);
                    }

                    counter += pollostmerging; // how many pol lost after merging.

                }
                copier.add(newcontent);
            }

            //make the json array to replace the functionality of gstreader.py
            JSONObject ans = new JSONObject();
            JSONArray a = new JSONArray();
            JSONArray xcoord = new JSONArray();
            JSONArray ycoord = new JSONArray();
            //to get the x and y coordinates in their native form, get [4,5]
            for (int i = 1; i< simulationResult.size(); i++) {
            	xcoord.put(Float.parseFloat(simulationResult.get(i)[4]));
            	ycoord.put(Float.parseFloat(simulationResult.get(i)[5]));
            }
            for (int z = 0; z < heightamount; z++) {
                JSONArray h = new JSONArray();
                for (int y = startcontentindex; y < startcontentindex + findUniquePol(copier, startcontentindex).size(); y++) { // index0-index 9 for 1 pollutant
                    JSONArray pol = new JSONArray();
                    for (int x = 1; x < copier.size(); x++) { // 1-the last line
                        pol.put(copier.get(x)[y + findUniquePol(copier, startcontentindex).size() * z]);
                    }
                    h.put(pol);
                }
                a.put(h);
            }
            ans.put("grid", a);
            ans.put("numpol", numpol);
            ans.put("listofpol", findUniquePol(copier, startcontentindex));
            ans.put("numheight", heightamount);
            ans.put("numinterval", 10);
            ans.put("initialheight", 0);
            ans.put("x_coord", xcoord);
            ans.put("y_coord", ycoord);
            


            new QueryBroker().putLocal(folder + "/testmod.levels.gst", MatrixConverter.fromArraytoCsv(copier));

            //logger.debug("=== Result === :" + result);
            response.setContentType("application/json");
            //response.getWriter().write(result);
            response.getWriter().write(ans.toString());
        } catch (JSONException e) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
        }
    }
    

    public List<String> findUniquePol(List<String[]> simulationResult, int startcontentindex) { //only the unique
        List<String> newList = getAllPollutants(simulationResult, startcontentindex).stream().distinct().collect(Collectors.toList()); //remove duplication

        if (newList.contains("PM2.5") && !newList.contains("PM10")) {
            newList.add("PM10");
        }
        //int sizeofpol=newList.size();
        return newList;
    }

    public ArrayList<String> getAllPollutants(List<String[]> simulationResult, int startcontentindex) {

        int sizeofpol2 = startcontentindex;
        ArrayList<String> listofpol = new ArrayList<String>();

        while (Double.valueOf(simulationResult.get(0)[sizeofpol2].split("Z=")[1].split("m")[0])
                - Double.valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {

            String name = simulationResult.get(0)[sizeofpol2].split("\\|")[2];
            //System.out.println(name);
            sizeofpol2++;
            if (name.contains("PM10")) {
                listofpol.add("PM10");
            } else if (name.contains("PM2.5")) {
                listofpol.add("PM2.5");
            } else {
                listofpol.add(name);
            }
        }


        return listofpol;
    }
}
