package uk.ac.ceb.como.molhub.action;


import java.io.IOException;


public class PythonExample {

	public static void main(String[] args) throws IOException, InterruptedException {
		
		String[] cmd = {"python", "C:/Users/nk510/git/c4e-dln22-TDC/Source/thermoDriver.py", "-j", "C:/Users/nk510/git/c4e-dln22-TDC/Source/TiCl4.json", };
		
		Runtime.getRuntime().exec(cmd);
	}
}