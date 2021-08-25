package uk.ac.cam.cares.jps.base.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.cts.CRSFactory;
import org.cts.IllegalCoordinateException;
import org.cts.crs.CRSException;
import org.cts.crs.CompoundCRS;
import org.cts.crs.CoordinateReferenceSystem;
import org.cts.crs.GeodeticCRS;
import org.cts.datum.GeodeticDatum;
import org.cts.op.CoordinateOperation;
import org.cts.op.CoordinateOperationException;
import org.cts.op.CoordinateOperationFactory;
import org.cts.op.transformation.FrenchGeocentricNTF2RGF;
import org.cts.op.transformation.GridBasedTransformation;
import org.cts.op.transformation.NTv2GridShiftTransformation;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class CRSTransformer {
	
	/**
	 * used by google, geojson, gis systems
	 */
	public static final String EPSG_4326 = "EPSG:4326";
	public static final String EPSG_3857 = "EPSG:3857";
	/**
	 * used by Berlin city gml data
	 */
	public static final String EPSG_25833 = "EPSG:25833";	
	/**
	 * used by The Hague city gml data
	 */
	public static final String EPSG_28992 = "EPSG:28992";
	/**
	 * used by Singapore apl input file
	 */
	public static final String EPSG_3414 = "EPSG:3414";
	/**
	 * used by Hong Kong apl input file
	 */
	public static final String EPSG_2326 = "EPSG:2326";
	/**
	 * used by Singapore Episode
	 */
	public static final String EPSG_32648 = "EPSG:32648";
	/**
	 * used by Hong Kong Episode
	 */
	public static final String EPSG_32650 = "EPSG:32650";

	private static Logger logger;
	private static CRSFactory cRSFactory;
	private static Map<String, CoordinateReferenceSystem> crsMap = new HashMap<String, CoordinateReferenceSystem>();
	private static Map<String, Set<CoordinateOperation>> coordOpMap = new HashMap<String, Set<CoordinateOperation>>();

	private static synchronized CRSFactory getFactory() {
		if (cRSFactory == null) {
			cRSFactory = new CRSFactory();
			RegistryManager registryManager = cRSFactory.getRegistryManager();
			// registryManager.addRegistry(new IGNFRegistry());
			registryManager.addRegistry(new EPSGRegistry());
			// registryManager.addRegistry(new ESRIRegistry());
			// registryManager.addRegistry(new Nad27Registry());
			// registryManager.addRegistry(new Nad83Registry());
			// registryManager.addRegistry(new WorldRegistry());
			
			logger = LogManager.getLogger(CRSTransformer.class);
		}

		return cRSFactory;
	}
	
	/**
	 * Transform a point from a CRS to another CRS
	 *
	 * The source code for this method was initially copied from
	 * https://github.com/orbisgis/cts/blob/master/src/test/java/org/cts/op/BaseCoordinateTransformTest.java
	 * and then adapted.
	 *
	 * @param sourceCRSName
	 * @param targetCRSName
	 * @param inputPoint
	 * @return
	 * @throws IllegalCoordinateException
	 * @throws CoordinateOperationException
	 * @throws CRSException 
	 */
	    static double[] transformInternal(String sourceCRSName, String targetCRSName, double[] sourcePoint)
		         throws IllegalCoordinateException, CoordinateOperationException, CRSException {
		   
		GeodeticCRS sourceCRS = (GeodeticCRS) getCRS(sourceCRSName);
		GeodeticCRS targetCRS = (GeodeticCRS) getCRS(targetCRSName);
		
		Set<CoordinateOperation> ops = getCoordinateOperations(sourceCRS, targetCRS);
		//int tot = ops.size();
		// for (CoordinateOperation op : ops) System.out.println(" " + op.getName());
		if (sourceCRS.getDatum() == GeodeticDatum.WGS84 || targetCRS.getDatum() == GeodeticDatum.WGS84) {
			ops = CoordinateOperationFactory.excludeFilter(ops, FrenchGeocentricNTF2RGF.class);
			ops = CoordinateOperationFactory.excludeFilter(ops, NTv2GridShiftTransformation.class);
		}
		// If source CRS comes from the EPSG registry and is not a CompoundCRS,
		// we use BursaWolf or translation rather than GridBasedTransformation,
		// even if a GridBasef Transformation is available (precise transformation
		// may be available because we also read IGNF registry and precise
		// transformations have been stored in GeodeticDatum objects.
		else if (sourceCRS.getIdentifier().getAuthorityName().equals("EPSG") && !(sourceCRS instanceof CompoundCRS)
				&& !(targetCRS instanceof CompoundCRS)) {
			ops = CoordinateOperationFactory.excludeFilter(ops, GridBasedTransformation.class);
		}
		//int subtot = ops.size();
		if (!ops.isEmpty()) {
			CoordinateOperation op = CoordinateOperationFactory.getMostPrecise(ops);
			
//			if (logger.isDebugEnabled()) {
//				logger.debug("Source " + sourceCRS);
//				logger.debug("Target " + targetCRS);
//				logger.debug(tot + " transformations found, " + subtot + " retained");
//				logger.debug("Used transformation (" + op.getPrecision() + ") : " + op);
//
//				if (ops.size() > 1) {
//					for (CoordinateOperation oop : ops) {
//						logger.debug("   other transformation : precision = " + oop.getPrecision());
//					}
//				}
//			}
			
			double[] source = new double[sourcePoint.length];
			System.arraycopy(sourcePoint, 0, source, 0, sourcePoint.length);
			return op.transform(source);
			
		} else {
			String message = "No CRS transformation found from " + sourceCRS + " to " + targetCRS;
			logger.error(message);
			throw new RuntimeException(message);
		}
	}

	public static double[] transform(String sourceCRSName, String targetCRSName, double... sourcePoints) {
		
		double[] result = new double[sourcePoints.length];
		
		try {		
			for (int i=0; i<sourcePoints.length; i=i+2) {
				double[] pointPoint = new double[] {sourcePoints[i], sourcePoints[i+1]};
				double[] targetPoint = transformInternal(sourceCRSName, targetCRSName, pointPoint);
				result[i] = targetPoint[0];
				result[i+1] = targetPoint[1];
			} 
		} catch (IllegalCoordinateException | CRSException | CoordinateOperationException exc) {
			throw new JPSRuntimeException(exc.getMessage(), exc);
		}
		
		return result;
	}
	
	private static synchronized CoordinateReferenceSystem getCRS(String crsName) throws CRSException {
		if (crsMap.containsKey(crsName)) {
			return crsMap.get(crsName);
		}
		
		CoordinateReferenceSystem crs = getFactory().getCRS(crsName);
		crsMap.put(crsName,  crs);
		return crs;
	}
	
	private static Set<CoordinateOperation> getCoordinateOperations(GeodeticCRS sourceCRS, GeodeticCRS targetCRS) throws CoordinateOperationException {
		String key = sourceCRS.getName() + "_" + targetCRS.getName();
		if (coordOpMap.containsKey(key)) {
			return coordOpMap.get(key);
		}
		
		Set<CoordinateOperation>  ops = CoordinateOperationFactory.createCoordinateOperations(sourceCRS, targetCRS);
		coordOpMap.put(key,  ops);
		return ops;
	}
}
