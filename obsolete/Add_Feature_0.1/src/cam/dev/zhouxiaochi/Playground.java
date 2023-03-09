package cam.dev.zhouxiaochi;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.EventObject;
import java.util.Vector;

import javax.swing.AbstractCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;

import org.json.JSONException;

public class Playground {
  public static void main(String args[]) throws Exception {
	  PointObjectsGenerator.layer_factory(0,"Load",null,"Load_Point",true); // Load Points
      PointObjectsGenerator.layer_factory(0,"Coupler",null,"Bus_Coupler",false); // Load Points
      PointObjectsGenerator.layer_factory(0,"Transformer","^.*EHT.*$","EHT_Station",true);
      PointObjectsGenerator.layer_factory(0,"Transformer","^.*UHT.*$","UHT_Station",true);
      PointObjectsGenerator.layer_factory(0,"Transformer","^.*HT.*$","HT_Station",true);
      PointObjectsGenerator.layer_factory(0,"Transformer","^.*LT.*$","LT_Station",true);  
      System.out.println("Finished");
  }
}
