package uk.ac.cam.cares.jps.editor.file;

import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * A test created for assessing the correctness of the file editing tool.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class FileEditorTest {
	/**
	 * Verifies if the FileEditor can edit a list of strings in a target file.
	 * 
	 * @throws SQLException
	 */
	@Test
	public void editStringsTest() throws SQLException{
		File dataDir = new File("data/owl");
		if (dataDir == null) {
		      System.out.println("Folder not found.");
		  } else {
			  IFileEditor iFileEditor = new FileEditor();
				List<String> fileExtensions = Arrays.asList(".owl");
				List<String> oldStrings = Arrays.asList("http://theworldavatar.com/");
				List<String> newStrings = Arrays.asList("http://www.theworldavatar.com/");
				iFileEditor.replace(dataDir.getAbsolutePath(), fileExtensions, oldStrings, newStrings);
				File expectedOutputFile = new File("data/output/0cee09a7-235b-4d73-adaf-09aeda8f50cc_34.owl");
				File editedFile = new File("data/owl-edited/0cee09a7-235b-4d73-adaf-09aeda8f50cc_34.owl");
				try {
					assertNotNull(editedFile);
					boolean isTwoEqual = FileUtils.contentEqualsIgnoreEOL(expectedOutputFile, editedFile, null);
					assertTrue(isTwoEqual);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		  }
	}

}
