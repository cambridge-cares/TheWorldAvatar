package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

public class Utils {
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openSourceFile(String filePathPlusName)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				filePathPlusName), "UTF-8"));
	}
}
