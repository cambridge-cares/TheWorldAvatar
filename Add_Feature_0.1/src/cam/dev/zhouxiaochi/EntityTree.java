package cam.dev.zhouxiaochi;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


	
public class EntityTree{
	
	@FunctionalInterface
	interface Function2 <A, B, R>{
		public R apply(A a, B b);
	}
	
	  final static Logger logger = LoggerFactory.getLogger(OWLReader.class);

	private TreeNode root;
	
	public TreeNode getRoot() {
		return root;
	}



	public EntityTree (EntityInfo rootEInfo){
		root = new TreeNode(rootEInfo);
	}
	
	

	


	
	private void expandEachDoSth(TreeNode mNode, Consumer<TreeNode> doSth ){
		doSth.accept(mNode);
		List<TreeNode> children = mNode.getChildren();
		for(TreeNode child : children){
			expandEachDoSth(child, doSth);
		}
		
	}
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

	private static  EntityInfo  getLeafNodeData(TreeNode mNode){
		if(mNode.children.size() == 0){//is this node a leaf?
			//=>YES! Then add its data to leafData list
			return mNode.entityInfo;
			
		}
		return null;
	}
	
	private static EntityInfo  getNodeData(TreeNode mNode){
		return mNode.entityInfo;
	}
	public TreeNode getNodeVName(String name){
		TreeNode target = expandGetNodeVName(root, name);
		return target;
	}
	
	
	public List<EntityInfo>  getAllLeafNodeData(){
		if(leafDatas == null){///lazy initiation
			leafDatas =  (ArrayList<EntityInfo>) expandEachGetSth(root, EntityTree::getLeafNodeData);
			
		}
		return leafDatas;
		
	} 
	
	

	
	
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

