package uk.ac.cam.cares.jps.base.slurm.job.test;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.slurm.job.Property;

public class PropertyTest extends TestCase{

	// test getPropertyName
	public void testgetPropertyName() {
		String userhome = System.getProperty("user.home");
		
		assertEquals(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(), userhome);
		assertEquals(Property.CHK_POINT_FILE_EXTENSION.getPropertyName(),".chk");
		assertEquals(Property.STATUS_FILE_NAME.getPropertyName(),"status.txt");
		assertEquals(Property.JSON_INPUT_FILE_NAME.getPropertyName(),"input.json");
		assertEquals(Property.SLURM_SCRIPT_FILE_NAME.getPropertyName(),"Slurm.sh");
	}
	
	// test getValue
	public void testgetValue() {
		assertEquals(Property.JOB_WORKSPACE_PARENT_DIR.getValue(), 0);
		assertEquals(Property.CHK_POINT_FILE_EXTENSION.getValue(), 0);
		assertEquals(Property.STATUS_FILE_NAME.getValue(),0);
		assertEquals(Property.JSON_INPUT_FILE_NAME.getValue(), 0);
		assertEquals(Property.SLURM_SCRIPT_FILE_NAME.getValue(), 0);
	}
	
}