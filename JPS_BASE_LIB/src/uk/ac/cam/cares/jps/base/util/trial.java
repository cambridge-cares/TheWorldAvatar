package uk.ac.cam.cares.jps.base.util;


import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.cts.IllegalCoordinateException;
import org.cts.crs.CRSException;
import org.cts.op.CoordinateOperationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class trial {

	public static void main(String[] args) {
		double[] coord= {10,0,0,0,0};
		CRSTransformer cRSTransformer= new CRSTransformer();
		FileUtil fileUtil=new FileUtil();
		String actual = fileUtil.readFileLocally(System.getProperty("user.dir")+"/test_sample_dir/test.txt");
		double[] expected = null;
		try {
			expected = cRSTransformer.transformInternal("EPSG:3414", "EPSG:2326", coord);
		} catch (IllegalCoordinateException e) {
			// TODO Auto-generated catch block
			System.out.println("illegal");
		} catch (CoordinateOperationException e) {
			// TODO Auto-generated catch block
			System.out.println("op");
		} catch (CRSException e) {
			// TODO Auto-generated catch block
			System.out.println("crs");
		}

		System.out.println(actual);

		

}
}