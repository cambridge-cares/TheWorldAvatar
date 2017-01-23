

package cam.dev.zhouxiaochi;


/**
 * A heuristic that uses the tile that is closest to the target
 * as the next best tile.
 * 
 * @author Kevin Glass
 */
public class ClosestHeuristic implements AStarHeuristic {
	private static final float LINE_COST_BIAS = 10000.0f;

	/**
	 * @see AStarHeuristic#getCost(TileBasedMap, Mover, int, int, int, int)
	 */
	public float getCost(TileBasedMap map,  int x, int y, int tx, int ty) {		
		float dx = tx - x;
		float dy = ty - y;
		
		float result = (float) (Math.sqrt((dx*dx)+(dy*dy)));
		
		return result;
	}

	//biased ,favor for straight line(same x or y as current)
	@Override
	public float getCost(TileBasedMap map, int curX, int curY, int x, int y, int tx, int ty) {
		float dx = tx - x;
		float dy = ty - y;
		
		float result = (float) (Math.sqrt((dx*dx)+(dy*dy)));
		
		///favor straight line
	 
		if(curX == x || curY == y){
		result*=1.1f; ///small overheat to speed up search
		} else{
			result*=100.0f;
		}
 
		if(map.isLine(x,y)){
			result*=LINE_COST_BIAS;
		}
		
			return result ;
	
	}

}