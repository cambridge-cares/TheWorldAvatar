package uk.ac.cam.cares.jps.base.util;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import static org.mockito.Mockito.*;

class testFileUtil {
	FileUtil fileUtil;
	@BeforeEach
	public void init(){
		fileUtil = new FileUtil();
		}

	@Test
	void testreadFileLocally() {
		String actual = fileUtil.readFileLocally(System.getProperty("user.dir")+"/test_sample_dir/test.txt");
		String expected= "Dfds";
		assertEquals(expected,actual,"The command was not executed correctly");
		assertThrows(JPSRuntimeException.class, () -> fileUtil.readFileLocally("dfsd"));
	}
	
	@Test
	void testwriteFileLocally() {
		fileUtil.writeFileLocally(System.getProperty("user.dir")+"/test_sample_dir/test2/test.txt","testwrite");
		String actual = fileUtil.readFileLocally(System.getProperty("user.dir")+"/test_sample_dir/test2/test.txt");
		String expected= "testwrite";
		assertEquals(expected,actual,"The command was not executed correctly");
		//assertThrows(JPSRuntimeException.class, () -> fileUtil.writeFileLocally("dfsd","dsds"));
	}
	



}
