package uk.ac.cam.cares.jps.composition.test;

import static org.junit.Assert.*;

import org.junit.Test;

import uk.ac.cam.cares.jps.composition.Vocabulary.*;

public class VocabularyTest {

	@Test
	public void test() {
		System.out.println(MSM.Service.IRI());
		System.out.println(MSM.Operation.IRI());
		System.out.println(MSM.MessageContent.IRI());
		System.out.println(MSM.MessagePart.IRI());

	}

}
