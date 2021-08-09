package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class CommandHelper {
    /* Author ZHOU XIAOCHI 2018.5.17*/

    private static Logger LOGGER = LogManager.getLogger(CommandHelper.class);


    //Since the command line commands are dependent on the OS, its imp to identify the OS.
    private static String OS = System.getProperty("os.name").toLowerCase();

    public static boolean isWindows() {

        return (OS.contains("win"));

    }

    public static boolean isMac() {

        return (OS.contains("mac"));

    }


    public static String executeSingleCommand(String targetFolder, String command) {
        LOGGER.info("In folder: " + targetFolder + " Excuted: " + command);

        return getCommandResultString(getCommandProcess(targetFolder, command));
    }

    public static String executeCommands(String targetFolder, ArrayList<String> commands) {
        LOGGER.info("In folder: " + targetFolder + " Excuted: " + commands);

        String[] command = commands.toArray(new String[0]);
        String resultString = getCommandResultString(getCommandProcess(targetFolder, command));


        int min = Math.min(resultString.length(), 200);
        LOGGER.info("=== Result (only the first 200 characters) === :" + resultString.substring(0, min));
        return resultString;
    }

	public static String executeAsyncSingleCommand(String targetFolder, String command) {

		LOGGER.info("In folder: " + targetFolder + " Excuted: " + command);

        return getCommandResultString(getAsyncCommandProcess(targetFolder, command));
	}

    public static String getCommandResultString(Process pr) {

        BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
        String line;
        StringBuilder resultString = new StringBuilder();
        try {
            while ((line = bfr.readLine()) != null) {
                resultString.append(line);
            }
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        return resultString.toString();
    }


    private static Process getCommandProcess(String targetFolder, String command) {
        Process pr;

        try {
            pr = Runtime.getRuntime().exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        return pr;
    }

    private static Process getCommandProcess(String targetFolder, String[] command) {
        Process pr;

        try {
            pr = Runtime.getRuntime().exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        return pr;
    }

	private static Process getAsyncCommandProcess(String targetFolder, String command) {
		Process pr = null;

		try {

			if (isWindows()) {
				pr = Runtime.getRuntime().exec("cmd /c start " + command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
			} else if (isMac()) {
				pr = Runtime.getRuntime().exec("open " + command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
			}


		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		return pr;
	}

}