package uk.ac.cam.cares.jps.editor.file;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

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
	 * @throws IOException
	 */
	@Test
	public void editStringsTest() throws SQLException, IOException {
		File dataDir = new File("data/owl");

		assertNotNull("Folder not found.", dataDir);

		IFileEditor iFileEditor = new FileEditor();
		List<String> fileExtensions = Arrays.asList(".owl");
		List<String> oldStrings = Arrays.asList("http://theworldavatar.com/");
		List<String> newStrings = Arrays.asList("http://www.theworldavatar.com/");
		iFileEditor.replace(dataDir.getAbsolutePath(), fileExtensions, oldStrings, newStrings);
		File expectedOutputFile = new File("data/output/0cee09a7-235b-4d73-adaf-09aeda8f50cc_34.owl");
		File editedFile = new File("data/owl-edited/0cee09a7-235b-4d73-adaf-09aeda8f50cc_34.owl");

		// This method will output any difference between the two files rather than just 'true' or 'false'
		assertEquals(Files.readAllLines(expectedOutputFile.toPath()), Files.readAllLines(editedFile.toPath()));
	}

}
