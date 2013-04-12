/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend.swing

import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.JTree
import javax.swing.JScrollPane
import javax.swing.event.TreeSelectionListener
import javax.swing.event.TreeSelectionEvent
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreePath
import scala.swing.Component
import javax.swing.event.TreeExpansionListener
import javax.swing.event.TreeExpansionEvent
import org.greatcactus.xs.frontend._

/**
 * A Swing implementation of the TreePane (to the left of the SwingDetailsPane)
 */
class SwingTreePane(edit:XSEdit) extends XSEditListener {
  
  var currentlyEditing = edit.currentlyEditing
  var lookup : Map[XSTreeNode,DefaultMutableTreeNode] = Map.empty
  
  private def build(node:XSTreeNode) : DefaultMutableTreeNode = {
      val gui = new DefaultMutableTreeNode(node)
      lookup+=node->gui
      for (c<-node.treeChildren) gui.add(build(c))
      gui
  }
  
  val treeModel = new DefaultTreeModel(build(edit.treeRoot))
  val jtree = new JTree(treeModel)
  
    jtree.addTreeSelectionListener(new TreeSelectionListener {
      override def valueChanged(e:TreeSelectionEvent) {
        jtree.getLastSelectedPathComponent match {
          case n:DefaultMutableTreeNode =>
            val node = n.getUserObject.asInstanceOf[XSTreeNode]
            currentlyEditing=node
            edit.changeCurrentlyEditing(node)
          case _ =>
        }
      }
    })
  
  jtree.addTreeExpansionListener(new TreeExpansionListener {
    override def treeExpanded(e:TreeExpansionEvent) { state(e,true) }
    override def treeCollapsed(e:TreeExpansionEvent) { state(e,false) }
    
    def state(e:TreeExpansionEvent,isOpen:Boolean) {
      e.getPath().getLastPathComponent() match {
        case n:DefaultMutableTreeNode => 
          val node = n.getUserObject.asInstanceOf[XSTreeNode]
          //println("Expanded "+node+" state="+isOpen)
          edit.setOpen(node,isOpen)
        case _ => println("Strange thing expanded")
      }
    }
  })
    
  val treeView = new JScrollPane(jtree)
  val wrapped = Component.wrap(treeView)
  
  
  def removeFromLookup(n:DefaultMutableTreeNode) {
    lookup-=n.getUserObject.asInstanceOf[XSTreeNode]
    import collection.JavaConversions._
    for (c<-n.children().toSeq) removeFromLookup(c.asInstanceOf[DefaultMutableTreeNode])
  }
  
  def change(c:TreeNodeChange) {
    val parent = lookup(c.parent)
    if (c.changedStructure) {
      for (d<-c.removedChildren;dd<-lookup.get(d)) treeModel.removeNodeFromParent(dd)
      for (n<-c.addedChildren.sortBy{c.children.indexOf(_)}) treeModel.insertNodeInto(build(n),parent,c.children.indexOf(n))
      treeModel.nodeStructureChanged(parent)
    }
    val parentpath = getPath(parent)
    val currentlyExpanded = jtree.isExpanded(parentpath)
    if (currentlyExpanded!=c.parent.isOpen) {
      if (c.parent.isOpen) jtree.expandPath(parentpath)
      else jtree.collapsePath(parentpath)
    }
    treeModel.nodeChanged(parent)
    for (sub<-c.sub) change(sub)
  }
  override def apply(changes:TreeChange) {
    for (e<-changes.elements) change(e)
  }
  override def setCurrentlyEditing(node:Option[XSTreeNode]) {
    for (n<-node;ln<-lookup.get(n)) if (currentlyEditing!=n) {
      currentlyEditing=n
      val path = getPath(ln)
      jtree.setSelectionPath(path) 
    }
  }


  def getPath(node:DefaultMutableTreeNode) = new TreePath(node.getPath().asInstanceOf[Array[Object]])
}