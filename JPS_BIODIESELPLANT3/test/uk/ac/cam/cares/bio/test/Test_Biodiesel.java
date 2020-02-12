package uk.ac.cam.cares.bio.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.bio.DoSimulation;

public class Test_Biodiesel extends TestCase{
	protected int value1, value2;
	   
	   // assigning the values
	   protected void setUp(){
	      value1 = 3;
	      value2 = 3;
	   }

	   // test method to add two values
	   public void testAdd(){
	      double result = value1 + value2;
	      assertTrue(result == 6);
	   }
	   public void testSimulation() throws IOException{
		   Double[] inputs_num = new Double[]{33.0,30.0,180.0,30.0,233.135,4.0};
		   ArrayList<String[]>result=new DoSimulation().doSimulation(null,inputs_num);
	   System.out.println("answer= "+Arrays.toString(result.get(0)));
	   }
}
