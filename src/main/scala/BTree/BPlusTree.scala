package BTree

class KeyIndex(val x: Node, val i: Int)

class Node(var minDegree: Int, var isLeaf: Boolean, var isRoot: Boolean) {
	var numKey: Int = 0
	var degree: Int = 0
	var keys: Array[Int] = new Array[Int](minDegree * 2 - 1)
	var children: Array[Node] = new Array[Node](minDegree * 2)
	var minNodeDegree: Int =
		if(isLeaf) minDegree
		else if(isRoot) 2
		else minDegree
	var maxNodeDegree: Int = minDegree * 2
}

class BPlusTree(val minDegree: Int) {
	var numNode: Int = 1
	var root: Node = new Node(100, true, true)
	private def diskRead(x: Node): Unit = {

	}
	private def diskWrite(x: Node): Unit = {

	}
	private def splitChild(x: Node, i: Int): Unit = {
		// x.children(i) is full so we need to split x.children(i)
		val z = new Node(minDegree, x.children(i).isLeaf, false)
		z.numKey = minDegree - 1
		for(j <- 0 until minDegree - 1)
			z.keys(j) = x.children(i).keys(j + minDegree)
		if (!x.children(i).isLeaf)
			for(j <- 0 until minDegree)
				z.children(j) = x.children(i).children(j + minDegree)
		x.children(i).numKey = minDegree - 1
		for(j <- 0 until x.degree - i - 1)
			x.children(x.degree - j) = x.children(x.degree - j - 1)
		x.children(i + 1) = z
		for(j <- 0 until x.degree - i - 1)
			x.keys(x.degree - j - 1) = x.keys(x.degree - j - 2)
		x.keys(i) = x.children(i).keys(minDegree - 1)
		x.numKey = x.numKey + 1
		diskWrite(x.children(i))
		diskWrite(z)
		diskWrite(x)
	}
	private def insertNoFull(x: Node, key: Int): Unit = {
		// Here x is not full, i.e. x.numKey <= minDegree * 2 - 2
		var i = x.numKey
		if (x.isLeaf) {
			while(i > -1 && key < x.keys(i - 1)) {
				x.keys(i) = x.keys(i - 1)
				i = i - 1
			}
			x.keys(i + 1) = key
			x.degree = x.degree + 1
			x.numKey = x.numKey + 1
			diskWrite(x)
		}
		else {
			while(i > -1 && key < x.keys(i - 1)) i = i - 1
			i = i + 1
			diskRead(x.children(i))
			if(x.children(i).numKey == 2 * minDegree - 1) {
				splitChild(x, i)
				if (key > x.keys(i)) i = i + 1
			}
			insertNoFull(x.children(i), key)
		}
	}
	def empty(k: Int) = new BPlusTree(k)
	def search(x: Node, k: Int): KeyIndex = {
		var i = 0
		while(i < x.numKey && k > x.keys(i))
			i = i + 1
		if(i < x.numKey && k == x.keys(i))
			new KeyIndex(x, i)
		else if (x.isLeaf)
			new KeyIndex(x, -1)
		else {
			diskRead(x.children(i))
			search(x.children(i), k)
		}
	}
	def insert(key: Int): Unit = {
		val r = root
		if(r.numKey == 2 * minDegree - 1) {
			root = new Node(minDegree, false, true)
			root.children(0) = r
			root.degree = root.degree + 1
			splitChild(root, 0)
			insertNoFull(root, key)
		}
	}
}
