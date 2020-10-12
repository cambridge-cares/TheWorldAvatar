package uk.ac.cam.cares.jps.agent.gPROMS;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.json.JSONObject;

import au.com.bytecode.opencsv.CSVWriter;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class output_demo {

	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		   JSONObject jo = new JSONObject();
	        System.out.println("Starting Matlab Agnet");
	        URI uri = AgentCaller.createURIWithURLandJSON("ElChemoAgent/test", jo.toString());
	        System.out.println(uri);
	       // String resultStart = AgentCaller.executeGetWithJsonParameter("ElChemoAgent/test", jo.toString());
	        //System.out.println("resultStart");
}
	}

