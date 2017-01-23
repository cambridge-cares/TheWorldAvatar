package cam.dev.zhouxiaochi;



import java.util.ArrayList;

import javax.swing.*; // Using Swing's components and containers

import com.esri.core.geometry.Point;

/** Custom Drawing Code Template */
// A Swing application extends javax.swing.JFrame
public class aStarLineGenerator {

	private MyMap mMap;
	private PathFinder finder;
	/** The last path found for the current unit */
	private Path[] paths;

	private ArrayList<Connection> connections = new ArrayList<Connection>();
	private ArrayList<Rectangle> obstacleUpdated = new ArrayList<Rectangle>();
	private ArrayList<ArrayList<Point>> resultLines;
	// Constructor to set up the GUI components and event handlers
	public aStarLineGenerator(double mapWidth, double mapHeight, int gridRowNum, int gridColNum,
			ArrayList<Connection> connectionList, ArrayList<Rectangle> obstacleList) {
		// copy everything from obstacleArr in obstacleUpdated list

		this.obstacleUpdated = obstacleList;
		this.connections = connectionList;
		mMap = new MyMap(mapWidth, mapHeight, gridRowNum, gridColNum, obstacleUpdated);
		paths = new Path[connections.size()];
        resultLines =   new ArrayList<ArrayList<Point>>(connections.size());
		startDrawPath();
		convertPath2Points();
	}

	public void startDrawPath() {
		finder = new AStarPathFinder(mMap, 10000, true);

		// iterate through all connection
		int idxPath = 0;
		for (Connection con : connections) {

			Point end = con.end;
			mMap.unblock(mMap.getCellRowIdx(end.getX()), mMap.getCellColIdx(end.getY()));
			Path path = finder.findPath(mMap.getCellRowIdx(con.start.getX()), mMap.getCellColIdx(con.start.getY()),
					mMap.getCellRowIdx(con.end.getX()), mMap.getCellColIdx(con.end.getY()));

			mMap.block(mMap.getCellRowIdx(end.getX()), mMap.getCellColIdx(end.getY()));

			if (path == null) {
				System.out.println("ERR: PATH NOT FOUND");
				paths[idxPath] = null;
				idxPath++;
			}
			else
			{
			paths[idxPath] = path;
			idxPath++;
			// add new path to obstacle updated list!
			// for every step in the path

			
			for (int idxStep = 0; idxStep < path.getLength(); idxStep++) {
				Path.Step mStep = path.getStep(idxStep);
				mMap.setLine(mStep.getX(), mStep.getY());
			}
			
			}
		}

	}

	/**
	 * Define inner class DrawCanvas, which is a JPanel used for custom drawing.
	 */
	private  void convertPath2Points(){
		// Override paintComponent to perform your own painting
		
		if(paths[0]==null){
			System.out.println("ERR: NO path is found");
		 
			 
			ArrayList<Point> temp = new ArrayList<Point>();
			Point temp_point = new Point(-999,-999);
			temp.add(temp_point);
			 resultLines.add(null);
			 return;
		}
		
		else{
			
				for (int idxPath = 0; idxPath < paths.length; idxPath++) {
					// start - > path[0]-> .... path[n-1]->end
					Path mPath = paths[idxPath];
					
					if(paths[idxPath] == null)
					{
						System.out.println("OH SHIT THIS IS NULL!!!");
					}
					else
					{
					
					Point startP = connections.get(idxPath).start;
					ArrayList<Point> line = new ArrayList<Point>();
					line.add(startP);
					for (int idxStep = 0; idxStep < mPath.getLength(); idxStep++) {

						Path.Step mStep = mPath.getStep(idxStep);
						// get the cell center from cellIdx
						Point aP = mMap.getCellCenterCoordiFromIdx(mStep.getX(), mStep.getY());

						line.add(aP);
			

					}
					// draw last, which is path[n-1] - >end
					Point endP = connections.get(idxPath).end;
                    line.add(endP);
                    resultLines.add(line);
					}
			}
		}
	}
	
	public ArrayList<ArrayList<Point>> getLinePointsList(){
		if(resultLines == null){
			System.out.println("ERR: CAN NOT FIND RESULT");
			return null;
		}
		
		return resultLines;
	}
	
	public ArrayList<Point>  getLineAt(int idx) {
		if(resultLines == null){
			System.out.println("ERR: CAN NOT FIND RESULT");
			return null;
		} else {
			if(idx >= resultLines.size()){
				System.out.println("ERR: OUT OF BOUND OF RESULTLINES");
				return null;
			}
		}
		return resultLines.get(idx);
	}

	public Point getNthPointAtLine(int idLine, int idPoint) {
		if(resultLines == null){
			System.out.println("ERR: CAN NOT FIND RESULT");
			return null;
		} else {
			if(idLine >= resultLines.size()){ 
				System.out.println("ERR: OUT OF BOUND OF RESULTLINES");
				return null;
			} else if(idPoint >= resultLines.get(idLine).size()){
				System.out.println("ERR: OUT OF BOUND OF points list of this line");
				return null;
			}
		}
		return resultLines.get(idLine).get(idPoint);
	}
}

