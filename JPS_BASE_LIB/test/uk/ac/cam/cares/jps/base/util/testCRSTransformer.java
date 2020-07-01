package uk.ac.cam.cares.jps.base.util;

import static org.junit.jupiter.api.Assertions.*;

import org.cts.IllegalCoordinateException;
import org.cts.crs.CRSException;
import org.cts.op.CoordinateOperationException;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

class testCRStransformer {

	@Test
	void testtransformInternal() {
		CRSTransformer cRSTransformer=new CRSTransformer();
		double[] expected1 = {-349791.02841347083,-1534788.7205039992};
		double[] coord1= {23.2,21.22};
		double[] coord2= {-13,-15};
		double[] actual1 = null;
		try {
			actual1 = cRSTransformer.transformInternal("EPSG:3414", "EPSG:2326",coord1);
		} catch (IllegalCoordinateException | CoordinateOperationException | CRSException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	   //assertThrows(CRSException.class, () ->cRSTransformer.transformInternal("EPSG:3414", "EPSG:2326", coord2),"Unknown exception thrown");
		assertArrayEquals(expected1,actual1,"The command was not executed correctly");
	}

}
