package uk.ac.cam.cares.jps.ship.coordination;

import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.coordination.GetExtraInfo;

import javax.ws.rs.BadRequestException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;

public class GetExtraInfoTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testValidateInput(){
        GetExtraInfo getExtraInfo= new GetExtraInfo();
        JSONObject jsonObject = new JSONObject();
        //Empty JSONObject
        try{
            getExtraInfo.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam is empty.",e.getMessage());
        }
        //key:filepath is not present
        jsonObject.put("key","value");
        try{
            getExtraInfo.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam does not contain the key:filepath or key:filepath is null.",e.getMessage());
        }
        //key:filepath is null
        jsonObject.put("filepath",JSONObject.NULL);
        try{
            getExtraInfo.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("RequestParam does not contain the key:filepath or key:filepath is null.",e.getMessage());
        }
        //key:filepath is empty
        jsonObject.put("filepath","");
        try{
            getExtraInfo.validateInput(jsonObject);
            Assert.fail();
        }catch (BadRequestException e){
            Assert.assertEquals("The key:filepath is empty.",e.getMessage());
        }
        //valid key:value pair
        jsonObject.put("filepath","testFilePath");
        boolean validate= getExtraInfo.validateInput(jsonObject);
        Assert.assertTrue(validate);
    }

    @Test
    //case when isWindows() is true
    public void testProcessRequestParameter1() throws NoSuchFieldException, IllegalAccessException, IOException {
        GetExtraInfo getExtraInfo= new GetExtraInfo();
        JSONObject jsonObject = new JSONObject();

        JSONObject expected1= new JSONObject();
        expected1.put("key1","value1");
        expected1.put("key2","value2");
        CommandHelper commandHelper= new CommandHelper();
        Field os= commandHelper.getClass().getDeclaredField("OS");
        os.setAccessible(true);
        os.set(commandHelper,"win");

        File f=folder.newFolder("basepath");
        File tempFile = folder.newFile("basepath/extra_info.json");

        //case where filepath contains a .gst file
        jsonObject.put("filepath",f.getPath().replace("\\","/")+"/JPS_ADMS/testFilePath.gst");
        try(MockedStatic<FileUtil>fileutil= Mockito.mockStatic(FileUtil.class)) {
            fileutil.when(() -> FileUtil.readFileLocally(tempFile.getPath().replace("\\","/"))).thenReturn(expected1.toString());
            JSONObject actual = getExtraInfo.processRequestParameters(jsonObject);
            Assert.assertEquals(expected1.toString(), actual.toString());
        }
        //case where filepath contains a .dat file
        jsonObject.put("filepath",f.getPath().replace("\\","/")+"/output/testFilePath.dat");
        try(MockedStatic<FileUtil>fileutil= Mockito.mockStatic(FileUtil.class)) {
            fileutil.when(() -> FileUtil.readFileLocally(tempFile.getPath().replace("\\","/"))).thenReturn(expected1.toString());
            JSONObject actual = getExtraInfo.processRequestParameters(jsonObject);
            Assert.assertEquals(expected1.toString(), actual.toString());
        }
    }

    @Test
    //case when isWindows is false
    public void testProcessRequestParameter2() throws NoSuchFieldException, IllegalAccessException, IOException {
        GetExtraInfo getExtraInfo = new GetExtraInfo();
        JSONObject jsonObject = new JSONObject();

        JSONObject expected1 = new JSONObject();
        expected1.put("key1", "value1");
        expected1.put("key2", "value2");
        CommandHelper commandHelper = new CommandHelper();
        Field os = commandHelper.getClass().getDeclaredField("OS");
        os.setAccessible(true);
        os.set(commandHelper, "mac");

        File f = folder.newFolder("basepath");
        File tempFile = folder.newFile("basepath/extra_info.json");

        //case where filepath contains a .gst file
        jsonObject.put("filepath", "home/file:/"+f.getPath().replace("\\", "/") + "/JPS_ADMS/testFilePath.gst");
        try (MockedStatic<FileUtil> fileutil = Mockito.mockStatic(FileUtil.class)) {
            fileutil.when(() -> FileUtil.readFileLocally(tempFile.getPath().replace("\\", "/"))).thenReturn(expected1.toString());
            JSONObject actual = getExtraInfo.processRequestParameters(jsonObject);
            Assert.assertEquals(expected1.toString(), actual.toString());
        }
        //case where filepath contains a .dat file
        jsonObject.put("filepath", "home/file:/"+f.getPath().replace("\\", "/") + "/output/testFilePath.dat");
        try (MockedStatic<FileUtil> fileutil = Mockito.mockStatic(FileUtil.class)) {
            fileutil.when(() -> FileUtil.readFileLocally(tempFile.getPath().replace("\\", "/"))).thenReturn(expected1.toString());
            JSONObject actual = getExtraInfo.processRequestParameters(jsonObject);
            Assert.assertEquals(expected1.toString(), actual.toString());
        }
    }
}
