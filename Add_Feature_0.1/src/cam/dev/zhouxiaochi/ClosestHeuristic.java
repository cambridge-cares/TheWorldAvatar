

package cam.dev.zhouxiaochi;


/**A* for line Arrangement : heuristic implements. Base is closest heuristic, with custom weight favours straight line
 *  && greatly favours no crossing with other lines.  
 * Note that closest heuristic itself favours straight[vertical|horizontal] steps to diagonal step with a weight of square root 2.
 * [A diagonal step will take a cost of square root two instead of 1]
 * 
 */
public class ClosestHeuristic implements AStarHeuristic {
	private static final float LINE_COST_BIAS = 10000.0f;

	/**
	 * @see AStarHeuristic#getCost(TileBasedMap, Mover, int, int, int, int)
	 */
	public float getCost(TileBasedMap map,  int x, int y, int tx, int ty) {		
		float dx = tx - x;
		float dy = ty - y;
		
		float result = (float) (Math.sqrt((dx*dx)+(dy*dy)));//calculate distance
		
		return result;
	}

	//biased ,favor for straight line(same x or y as current)
	@Override
	public float getCost(TileBasedMap map, int curX, int curY, int x, int y, int tx, int ty) {
		float dx = tx - x;
		float dy = ty - y;
		
		float result = (float) (Math.sqrt((dx*dx)+(dy*dy)));
		
		///favor straight line
	 
		if(curX == x || curY == y){////Next step forms a Straight line with current step?
			//=>yes! 
		result*=1.1f; ///add small overheat to speed up search(Note search is faster if heuristic cost > actual cost)
		} else{//NO
			//add 100 times cost weight(would go as far as 100 steps all straight[Vertical|Horizontal] to avoid one diagonal step)
			result*=100.0f;
		}
 
		if(map.isLine(x,y)){////Next step is crossing with other line?
			result*=LINE_COST_BIAS;//times line cost bias
		}
		
			return result ;
	
	}

}