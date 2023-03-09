package cam.dev.zhouxiaochi;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


	/***
	 * Tree structure for entities. Has inner class TreeNode.
	 * @author Shaocong
	 *
	 */
public class EntityTree{
	
	/***
	 * Self-define interface for function with 2 input parameter.
	 * @author Shaocong
	 *
	 * @param <A> parameter A type
	 * @param <B> parameter B type
	 * @param <R> return type
	 */
	@FunctionalInterface
	interface Function2 <A, B, R>{
		public R apply(A a, B b);
	}
	
	  final static Logger logger = LoggerFactory.getLogger(OWLReader.class);

	private TreeNode root;
	
	/**
	 * Get root node of Tree
	 * @return
	 */
	public TreeNode getRoot() {
		return root;
	}


/**
 * Constructor
 * @param rootEInfo
 */
	public EntityTree (EntityInfo rootEInfo){
		root = new TreeNode(rootEInfo);
	}
	
	

	


	
	
	/**
	 * expand on each node and call a function which takes in a treenode and return a entityInfo object. recursive
	 */
	List<EntityInfo> results = new ArrayList<EntityInfo>();
	private List<EntityInfo> expandEachGetSth(TreeNode mNode, Function<TreeNode, EntityInfo> getSth ){
		EntityInfo result = getSth.apply(mNode);
		if(result!=null){
		results.add(result);
		}
		List<TreeNode> children = mNode.getChildren();
		for(TreeNode child : children){
			expandEachGetSth(child, getSth);
		}
		return results;
	}

	/***
	 * expand on tree to search for a node with a name string, a recursive function
	 * @param mNode
	 * @param name
	 * @return
	 */
	private TreeNode expandGetNodeVName(TreeNode mNode, String name ){
		EntityInfo result = mNode.getEntityInfo();
		if(result.getName().equals(name)){
		return   mNode;
		}
		TreeNode targetNode = null;
		List<TreeNode> children = mNode.getChildren();
		for(TreeNode child : children){
			TreeNode tmp = null;
			tmp = expandGetNodeVName(child, name);
		 if(tmp != null){
			 targetNode = tmp; 
			 break;
		 }
		}
		return targetNode;
	}
	
	

	private  ArrayList<EntityInfo>  leafDatas = null;

	/***
	 * A function takes in a treenode and return its entityInfo if it is a leaf node, called by expandEachGetSth
	 * @param mNode
	 * @return
	 */
	private static  EntityInfo  getLeafNodeData(TreeNode mNode){
		if(mNode.children.size() == 0){//is this node a leaf?
			//=>YES! Then add its data to leafData list
			return mNode.entityInfo;
			
		}
		return null;
	}
	

	/***
	 * Search through node with a name string. Call recursive function expandGetNodeVName
	 * @param name
	 * @return
	 */
	public TreeNode getNodeVName(String name){
		TreeNode target = expandGetNodeVName(root, name);
		return target;
	}
	
	/***
	 * Tranverse through tree to get all EntityInfo of leaf nodes. Call recursive function expandEachGetSth with function getLeafNodeData
	 * @return
	 */
	public List<EntityInfo>  getAllLeafNodeData(){
		if(leafDatas == null){///lazy initiation
			leafDatas =  (ArrayList<EntityInfo>) expandEachGetSth(root, EntityTree::getLeafNodeData);
			
		}
		return leafDatas;
		
	} 
	
	

	
	/***
	 * Tree node has data: entityInfo, a list of children and a parent node.
	 * @author Shaocong
	 *
	 */
	public  class TreeNode{
		

		private EntityInfo entityInfo;
		public EntityInfo getEntityInfo() {
			return entityInfo;
		}


		
		public TreeNode(EntityInfo entityInfo) {
			this.entityInfo = entityInfo;
			this.children =  new ArrayList<TreeNode>();

		}





		private List<TreeNode> children;
		public List<TreeNode> getChildren() {
			return children;
		}


		private TreeNode parent;

		public TreeNode getParent() {
			return parent;
		}
		public void setParent(TreeNode parent) {
			this.parent = parent;
		}
		
		
		public TreeNode addChild( EntityInfo entityInfo){
			
			TreeNode childNode =new TreeNode(entityInfo);
			childNode.setParent(this);
			this.children.add(childNode);
			return childNode;
			
	}
		
		public void deleteAllChildren(){
			this.children.clear();
		}
	
	}
}

