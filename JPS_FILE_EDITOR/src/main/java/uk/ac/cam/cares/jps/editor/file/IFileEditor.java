package uk.ac.cam.cares.jps.editor.file;

import java.util.List;

/**
 * An interface to the implementation that enables editing any type of text</br>
 * file by replacing an old list of strings with a new list of strings.</br>
 * As each item in a list has a fixed position or index, the first item</br>
 * in the old list corresponds to the first item in the new list, the second</br>
 * item in the old list corresponds to the second item in the new list, and so on</br>
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IFileEditor {
	/**
	 * Replaces an old list of strings in a set of files stored in a directory</br>
	 * with a new list of strings. The size of both lists should be equal and the</br>
	 * first string of the old list corresponds to the first string of the new list.</br>
	 * Similarly, the last string of the old list corresponds to the last string of</br>
	 * the new list. The edited files are kept under the path formed by appending a folder</br>
	 * called edited at the end of the directory provided.
	 * 
	 * @param directory the directory or path where the files which are subject to be edited reside.
	 * @param fileExtensions the type of files which will be edited. For example, .owl, .rdf or .txt.
	 * @param oldStrings strings to be replaced
	 * @param newStrings strings that will replace the old or existing strings
	 */
	public void replace(String directory, List<String> fileExtensions, List<String> oldStrings, List<String> newStrings);
}
