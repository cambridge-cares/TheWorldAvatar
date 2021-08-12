package uk.ac.cam.cares.jps.editor.file;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * This class has been created to edit any type of text file by replacing</br>
 * an old or existing list of strings with a new list of strings.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class FileEditor implements IFileEditor{
	Logger log = Logger.getLogger(FileEditor.class.getName());
	
	/**
	 * The type of file, e.g. .owl, .rdf, .txt, .csv, etc.
	 */
	private String fileType;
	
	/**
	 * The default constructor.
	 */
	public FileEditor(){}
	
	public static void main(String[] args) {
		String directory = "C:\\Users\\msff2\\Documents\\c4eWorkInProgress\\kb";
		List<String> fileExtensions = Arrays.asList(".owl");
		List<String> oldStrings = Arrays.asList("https://como.ceb.cam.ac.uk/");
		List<String> newStrings = Arrays.asList("https://www.como.ceb.cam.ac.uk/");
		new FileEditor().replace(directory, fileExtensions, oldStrings, newStrings);
	}
	
	/**
	 * Replaces an old list of strings in a set of files stored in a directory</br>
	 * with a new list of strings. The size of both lists should be equal and the</br>
	 * first string of the old list corresponds to the first string of the new list.</br>
	 * Similarly, the last string of the old list corresponds to the last string of</br>
	 * the new list. The edited files are kept under the path formed by appending a folder</br>
	 * called edited at the end of the directory provided.
	 * 
	 * @param oldDirectory the directory or path where the files which are subject to be edited reside.
	 * @param fileExtensions the type of files which will be edited. For example, .owl, .rdf or .txt.
	 * @param oldStrings strings to be replaced
	 * @param newStrings strings that will replace the old or existing strings
	 */
	public void replace(String directory, List<String> fileExtensions, List<String> oldStrings, List<String> newStrings){
		if(oldStrings==null || newStrings==null){
			return;
		}
		if(oldStrings.size()!=newStrings.size()){
			log.info("The number of strings in the oldStrings list is not the same as the number of strings in the newStrings list");
			System.out.println("The number of strings in the oldStrings list is not the same as the number of strings in the newStrings list");
			return;
		}
		File directoryPath = new File(directory);
		if(directoryPath.isDirectory()){
			FileUtil fileUtil = new FileUtil();
			for(File file:fileUtil.getDirectoryFiles(new File(directory), fileExtensions)){
				replaceStrings(directory, file, oldStrings, newStrings);				
			}
		}else{
			log.info("The following directory does not exist:"+directory);
			System.out.println("The following directory does not exist:"+directory);
		}
	}

	/**
	 * Replaces a list of old or existing strings with a list of new or proposed strings.  
	 * 
	 * @param directory user provided directory where the files that have to be edited are located.
	 * @param file the file that will be updated by replacing a list of old strings</br>
	 * with a list of new strings.
	 * @param oldStrings the list of old strings 
	 * @param newStrings the list of new strings
	 */
	private void replaceStrings(String directory, File file, List<String> oldStrings, List<String> newStrings){
		try {
			BufferedReader br = FileUtil.openSourceFile(file.getAbsolutePath());
			// If the user provided path to the files is C:/file/path, the edited
			// files will be put under a new path called C:/file/path-edited 
			String newDirectoryPath = file.getAbsolutePath().replace(directory, directory.concat("-edited"));
			File newDirectory = new File(newDirectoryPath.replace(file.getName(), ""));
			newDirectory.mkdirs();
			BufferedWriter bw = FileUtil.openBufferedWriter(newDirectoryPath);
			String line = "";
			while((line=br.readLine())!=null){
				int stringIndex = 0;
				for(String oldString:oldStrings){
					if(line.contains(oldString)){
						line = line.replace(oldString, newStrings.get(stringIndex));
					}
					stringIndex++;
				}
				bw.write(line+"\n");
			}
			br.close();
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
