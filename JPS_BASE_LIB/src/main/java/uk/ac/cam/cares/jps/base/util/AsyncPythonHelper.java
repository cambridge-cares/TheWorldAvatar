package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;

/**
 * Outstream and Errstream of exe might block each other. Use async to avoid
 * this issue.
 * 
 * @author morta
 *
 */
public class AsyncPythonHelper {
	private static Logger logger = LoggerFactory.getLogger(PythonHelper.class);

	public static String[] callPython(String pythonScriptName, String[] parameters, Class thisObject)
			throws IOException {
		String path = AgentLocator.getNewPathToPythonScript(pythonScriptName, thisObject);
		logger.info(path, parameters);
		System.out.println(path);
		String[] head = { "python", path };
		String[] cmd = (String[]) ArrayUtils.addAll(head, parameters);
		for (String c : cmd) {
			System.out.println(c);
		}
		String[] result = null;
		try {
			result = processCommand(cmd);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;
	}

	public static String callCommandFrom(String[] cmds, String dir) throws IOException {
		List<String> cmdList = new ArrayList<String>(Arrays.asList(cmds));
		ProcessBuilder pb = new ProcessBuilder(cmdList);
		pb.directory(new File(dir));
		Process pr = pb.start();
		BufferedReader br = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String str = null;
		while ((str = br.readLine()) != null) {
			System.out.println(str);
		}

		return str;
	}

	public static String[] processCommand(String[] cmd) throws IOException, InterruptedException, ExecutionException {
		ExecutorService executor = Executors.newFixedThreadPool(2);// excecutor of async reading of output streams
		Process p = Runtime.getRuntime().exec(cmd);// process to execute

		Future<String> promiseErrOutput = executor.submit(getStreamResult(p.getErrorStream()));
		Future<String> promiseInputOutput = executor.submit(getStreamResult(p.getInputStream()));
		String[] results = new String[2];
		results[0] = promiseInputOutput.get();
		results[1] = promiseErrOutput.get();

		return results;
	};

	private static Callable<String> getStreamResult(InputStream inputStream) {
		Callable<String> callableTask = () -> {
			String line = "";
			try {
				InputStreamReader isr = new InputStreamReader(inputStream);
				BufferedReader br = new BufferedReader(isr);
				String newline;
				while ((newline = br.readLine()) != null) {
					line += newline + "\r\n";
				}

			} catch (Exception e) {
				logger.error("", e.getMessage(), e);
			}
			return line;
		};
		return callableTask;
	}
}
