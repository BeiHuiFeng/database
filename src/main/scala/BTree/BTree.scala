package BTree

import java.io.FileWriter
import java.nio.file.{Files, Paths}

import scala.io.Source


class KeyIndex(val x: Node, val i: Int)

class Node {
	var minDegree: Int = -1
	var id: Int = -1
	var isLeaf: Boolean = false
	var isRoot: Boolean = false

	var numKey: Int = -1			// number of keys in this node
	var degree: Int = -1			// number of children
	var keys: Array[Int] = _
	// val children: Array[Node] = new Array[Node](minDegree * 2)
	var childrenID: Array[Int] = _

	var minNodeNumKey: Int = -1
	var maxNodeNumKey: Int = -1
	var minNodeDegree: Int = -1
	var maxNodeDegree: Int = -1

	def this(minDegree: Int, id: Int, isLeaf: Boolean, isRoot: Boolean) {
		this()
		init(minDegree, id, isLeaf, isRoot)
	}

	def init(minDegree: Int, id: Int, isLeaf: Boolean, isRoot: Boolean): Unit= {

		this.minDegree = minDegree
		this.id = id
		this.isLeaf = isLeaf
		this.isRoot = isRoot

		this.numKey = 0
		this.degree = 0

		this.keys = new Array[Int](minDegree * 2 - 1)
		this.childrenID = new Array[Int](minDegree * 2)

		/* Every node except root: minDegree - 1 <= numKey <= 2 * minDegree - 1
		 * If root is a leaf node: 0 <= numKey <= 2 * minDegree - 1
	 	* If root is not a leaf node: 1 <= numKey <= 2 * minDegree - 1
	 	*/
		this.minNodeNumKey =
			if(!isRoot) minDegree - 1
			else if(isLeaf) 0
			else 1
		this.maxNodeNumKey = 2 * minDegree - 1

		/* Every node except root and leaf: minDegree <= degree <= 2 * minDegree
		 * Leaf node: 0 <= degree <= 0
		 * If root is not a leaf node: 2 <= degree <= 2 * minDegree
		 */
		this.minNodeDegree =
			if(isLeaf) 0
			else if(isRoot) 2
			else minDegree
		this.maxNodeDegree =
			if(isLeaf) 0
			else 2 * minDegree
	}
}

object BTree {
	def instance(minDegree: Int, dataPath: String): BTree = {
		val bt = new BTree(minDegree, dataPath)
		if(Files.exists(Paths.get(dataPath + "root.dat")))
			bt.load()
		else
			bt.init()
		bt
	}
}

class BTree(val minDegree: Int, val dataPath: String) {
	/* memoryCapacity limits the number of node that can be loaded in to memory
	 * nodeQueue maintains the ids of the node staying in memory
	 * When a new node is loaded into memory, then an old node must be written
	 * 		into the disk.
	 */
	val mem: virtualMemory = virtualMemory.empty(20, dataPath)

	var numNode: Int = 0		// the total number of nodes in this tree
	var height: Int = 0			// the height of the tree
	var rootID: Int = -1
	var root: Node = _

	def init(): Unit = {
		root = mem.allocate(minDegree, 0, true, true)
		numNode += 1
		height += 1
		rootID = root.id
	}

	private def diskRead(id: Int): Node = {
		mem.read(id)
	}

	private def diskWrite(): Unit = {
		mem.write()
	}

	private def splitChild(xID: Int, i: Int): Unit = {
		/* x.children(i) is full so we need to split x.children(i)
		 * x is the parent of y and z
		 * y is the left child of x while z is the right child of x
		 * Here left child means that y is left to z
		 */
		val x = diskRead(xID)
		val y = diskRead(x.childrenID(i))
		val z = mem.allocate(minDegree = minDegree, id = numNode, y.isLeaf, false)
		numNode += 1
		z.numKey = minDegree - 1
		z.degree = if(z.isLeaf) 0 else minDegree
		for(j <- 0 until minDegree - 1)
			z.keys(j) = y.keys(j + minDegree)
		if (!y.isLeaf)
			for(j <- 0 until minDegree)
				z.childrenID(j) = y.childrenID(j + minDegree)
		y.numKey = minDegree - 1
		y.degree = if(y.isLeaf) 0 else minDegree
		for(j <- 0 until x.degree - i - 1)
			x.childrenID(x.degree - j) = x.childrenID(x.degree - j - 1)
		x.childrenID(i + 1) = z.id
		for(j <- 0 until x.degree - i - 1)
			x.keys(x.degree - j - 1) = x.keys(x.degree - j - 2)
		x.keys(i) = y.keys(minDegree - 1)
		x.numKey = x.numKey + 1
		x.degree = x.degree + 1
	}

	private def insertNoFull(xID: Int, key: Int): Unit = {
		// Here x is not full, i.e. x.numKey <= minDegree * 2 - 2
		val x: Node = diskRead(xID)
		var i = x.numKey - 1
		if (x.isLeaf) {
			while(i > -1 && key < x.keys(i)) {
				x.keys(i + 1) = x.keys(i)
				i = i - 1
			}
			x.keys(i + 1) = key
			x.numKey = x.numKey + 1
		}
		else {
			while(i > -1 && key < x.keys(i)) i = i - 1
			i = i + 1
			val y = diskRead(x.childrenID(i))
			if(y.numKey == 2 * minDegree - 1) {
				splitChild(x.id, i)
				if (key > x.keys(i)) i = i + 1
			}
			insertNoFull(y.id, key)
		}
	}
	private def recursiveSearch(id: Int, key: Int): KeyIndex = {
		val x: Node = diskRead(id)
		var i = 0
		while(i < x.numKey && key > x.keys(i))
			i = i + 1
		if(i < x.numKey && key == x.keys(i))
			new KeyIndex(x, i)
		else if (x.isLeaf)
			new KeyIndex(x, -1)			// No result found
		else
			recursiveSearch(x.childrenID(i), key)
	}

	def search(key: Int): KeyIndex = {
		recursiveSearch(rootID, key)
	}

	def insert(key: Int): Unit = {
		root = diskRead(rootID)
		if(root.numKey == 2 * minDegree - 1) {
			// if root is full
			root.isRoot = false
			root.minNodeNumKey = minDegree - 1
			val newRoot = mem.allocate(minDegree, numNode, false, true)
			numNode += 1
			height += 1
			rootID = newRoot.id

			newRoot.childrenID(0) = root.id
			newRoot.degree = newRoot.degree + 1
			splitChild(newRoot.id, 0)
			insertNoFull(newRoot.id, key)
		}
		else insertNoFull(rootID, key)

	}

	def load(): Unit = {
		val file = Source.fromFile(dataPath + "root.dat")
		val iter = file.getLines()
		rootID = iter.next.split("\t")(1).toInt
		file.close()
		root = diskRead(rootID)
	}

	def save(): Unit = {
		val writer = new FileWriter(dataPath + "root.dat")
		writer.write("rootID:\t" + rootID)
		writer.close()
		diskWrite()
	}
}
