package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class FileUtilTest {

	// temporary folder for testing
	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	@Test
	public void testReadFileLocally() throws URISyntaxException {
		
		// read file from test resources (in target/test-classes/)
		String path = Paths.get(this.getClass().getResource("/FileUtilTestInput.txt").toURI()).toFile().getPath();
		String result = FileUtil.readFileLocally(path);
		
		assertEquals("This is a test file.\r\nTest input.", result);
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testReadFileLocallyException() {
		
		String path = "nonexistent/path";
		String result = FileUtil.readFileLocally(path);
	}
	
	@Test
	public void testWriteFileLocally() throws IOException {
		
		// write a test file to temporary folder
		String content = "This is a test file.\r\nTest write to file.";		
		String folderPath = tempFolder.getRoot().toString();
		String filePath = folderPath + "/FileUtilTestOutput.txt";
		
		FileUtil.writeFileLocally(filePath, content); 
		
		// assert new file exists
		File file = new File(filePath);
		assertTrue(file.exists());
		
		// check contents
		String result = new String(Files.readAllBytes(Paths.get(filePath)));
		assertEquals(content, result);
	}
	
	@Test
	public void testInputStreamToString() throws IOException {
		
		String expectedString = "This is a test string.\r\nConvert to inputstream.";
		
		InputStream inputStream = new ByteArrayInputStream(expectedString.getBytes(Charset.forName("UTF-8")));
		String result = FileUtil.inputStreamToString(inputStream);
		
		assertEquals(expectedString, result);
	}

	@Test
	public void testStringToInputStream() throws IOException {
		
		String expectedString = "This is a test string.\r\nConvert to inputstream.";
		
		// Test the method by converting the inputstream returned by the method back to a string and compare the contents
		try (ByteArrayOutputStream out = new ByteArrayOutputStream()) { // this takes care of closing the stream

			InputStream result = FileUtil.stringToInputStream(expectedString);
			
			IOUtils.copy(result, out);
			
			String resultString = new String(out.toByteArray());
			assertEquals(expectedString, resultString);
		} 
	}
	

	@Test
	public void testOpenSourceFile() throws URISyntaxException, IOException {
			
		// read file from test resources (in target/test-classes/)
		String filePath = Paths.get(this.getClass().getResource("/FileUtilTestInput.txt").toURI()).toFile().getPath();
		
		BufferedReader result = null;
		try {
			result = FileUtil.openSourceFile(filePath);
			
			// read to array
			String strCurrentLine;
			ArrayList<String> resultText = new ArrayList<String>();
			while ((strCurrentLine = result.readLine()) != null) {
	
			    resultText.add(strCurrentLine);
			}
			
			// assert size and contents
			assertEquals(2, resultText.size());
			assertEquals("This is a test file.", resultText.get(0)); 
			assertEquals("Test input.", resultText.get(1)); 
			
		}
		finally {
			if (result != null) {
				result.close();
			}
		}		
	}
	
	@Test
	public void testOpenBufferedWriter() throws IOException {
		
		// create file in temporary folder
		String folderPath = tempFolder.getRoot().toString();
		String filePath = folderPath + "/Test.txt";

		BufferedWriter result = null;
		try{
			
			result = FileUtil.openBufferedWriter(filePath);
			
			// write something
			result.write("Test");
			result.flush();
			
			// assert file exists
			File file = new File(filePath);
			assertTrue(file.exists());
			
		} finally {
			if (result != null) {
				result.close();
			}
		}
	}
	
	@Test
	public void testUnzip() throws URISyntaxException {
		
		// unzip file from test resources (in target/test-classes/)
		String sourcePath = Paths.get(this.getClass().getResource("/FileUtilTestZip.zip").toURI()).toFile().getPath();
		String targetFolderPath = tempFolder.getRoot().toString();
		String filePath = targetFolderPath + "/FileUtilTestZip.txt";
		
		FileUtil.unzip(sourcePath, targetFolderPath);
		
		// assert unzipped file exists
		File file = new File(filePath);
		assertTrue(file.exists());	 		 
	}
}