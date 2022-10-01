package limbo.leetcodeSolution

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object RandomOnes {
  def isSymmetric(root: TreeNode): Boolean = {
    def isMirror(r1: TreeNode,r2:TreeNode):Boolean = {
      if(r1 == null && r2 == null)
        true
      else if(r1 == null || r2 == null)
        false
      else{
        (r1.value == r2.value) &&
        isMirror(r1.left,r2.right) && isMirror(r1.right,r2.left)
      }
    }

    if(root == null)
      return true
    else
      isMirror(root.left,root.right)
  }


}
