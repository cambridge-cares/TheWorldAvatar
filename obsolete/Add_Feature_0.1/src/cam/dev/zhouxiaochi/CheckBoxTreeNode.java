package cam.dev.zhouxiaochi;
import java.util.function.Function;

import javax.swing.tree.DefaultMutableTreeNode;  
public class CheckBoxTreeNode extends DefaultMutableTreeNode  
{  
    protected boolean isSelected;  

    private Function<String, Boolean> selectCallBack = JParkSim::selectCallBack;
    private Function<String, Boolean> unselectCallBack = JParkSim::unselectCallBack;

   //  private Function<String, Boolean> selectCallBack = TestSideMenu::selectCallBack;
   //  private Function<String, Boolean> unselectCallBack = TestSideMenu::unselectCallBack;
     private String name; 
    public CheckBoxTreeNode()  
    {  
        this(null);  
    }  
      
    public CheckBoxTreeNode(Object userObject)  
    {  
        this(userObject, true, false);  
    }  
      
    public CheckBoxTreeNode(Object userObject, boolean allowsChildren, boolean isSelected)  
    {  
        super(userObject, allowsChildren);  
        this.isSelected = isSelected;  
        this.name = (String)userObject;
    }  
  
    public boolean isSelected()  
    {  
        return isSelected;  
    }  
      

    
    public void setSelected(boolean _isSelected)  
    {  
        this.isSelected = _isSelected;  
          
        if(_isSelected)   //set to be true?
        {  //Following behaviors should be implemented:
        	//1: If this node is selected, then all chidren of this node should be automatically selected
        	//2: If all children is selected of a parent, then this parent should be automatically selected
        	selectCallBack.apply(name);
        	
        	
            if(children != null)// this node has children?  
            {  
                for(Object obj : children)  //all this children is automatically selected
                {  
                    CheckBoxTreeNode node = (CheckBoxTreeNode)obj;  
                    if(_isSelected != node.isSelected())  
                        node.setSelected(_isSelected);  
                }  
            }  
            // check upwards: if all children are selected for a parent, then parent node is automatically selectedc 
            CheckBoxTreeNode pNode = (CheckBoxTreeNode)parent;  
            //this node has parent?
            if(pNode != null)  
            {  //=>yes! 
                int index = 0;  
                for(; index < pNode.children.size(); ++ index)  
                {  
                    CheckBoxTreeNode pChildNode = (CheckBoxTreeNode)pNode.children.get(index);  
                    if(!pChildNode.isSelected())//any of children is not selected?  
                        break;  //no need to select parent, exit
                }  
                
                if(index == pNode.children.size())//indeed all children are selected?  
                {  
                    if(!pNode.isSelected()) //select parent node 
                        pNode.setSelected(true);  
                }  
            }  
        }  
        else   //the node is unselected?
        {   //Following behaviors should be implemented:
            //1: unselect this node will NOT automatically unselect all chidren UNLESS ALL CHILDREN ARE CURRENTLY SELECTED
        	//2: unselect one of the children will automatially unselect the parent node
        	
        	unselectCallBack.apply(name);
            if(children != null)  //the node has children?
            {  //=> Yes
                int index = 0;  
                for(; index < children.size(); ++ index)  //loop all children
                {  
                    CheckBoxTreeNode childNode = (CheckBoxTreeNode)children.get(index);  
                    if(!childNode.isSelected())  //any child is unselected?
                        break;  //not all children are selected, should not do behavior 1, exit
                }  
               
                if(index == children.size())//all children are currently selected?  
                {  
                    for(int i = 0; i < children.size(); ++ i)  
                    {  
                        CheckBoxTreeNode node = (CheckBoxTreeNode)children.get(i);  
                        if(node.isSelected())  //any node is not unselected?
                            node.setSelected(false);  // unselect it
                    }  
                }  
            }  

            CheckBoxTreeNode pNode = (CheckBoxTreeNode)parent;  
            if(pNode != null && pNode.isSelected())//since this node is unselected, then parent should be unselected  
                pNode.setSelected(false);  
        }  
    }  
}  
