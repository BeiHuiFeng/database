package BTree

import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}

import MyExceptions.{AnywayThisIsAnException}

import scala.io.Source


class ItemType(val key: Int, val value: ValueType) {
	def this() {
		this(-1, new ValueType())
	}
	def isEmpty: Boolean = key == -1
}

class ValueType {
	var name: String = ""
	var age: Int = -1
	var school: String = ""
	def init(that: ValueType): Unit = {
		this.name = that.name
		this.age = that.age
		this.school = that.school
	}
	def init(name: String, age: Int, school: String): Unit = {
		this.name = name
		this.age = age
		this.school = school
	}
	def this(name: String, age: Int, school: String) {
		this()
		init(name, age, school)
	}
	def isEmpty: Boolean = age == -1
}

protected class Node {
	var minDegree: Int = -1
	var id: Int = -1
	var isLeaf: Boolean = false
	var isRoot: Boolean = false

	var isDestroyed: Boolean = false

	var numKey: Int = -1			// number of keys in this node
	var degree: Int = -1			// number of children
	var keys: Array[Int] = _
	var values: Array[ValueType] = _
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
		this.values = new Array[ValueType](minDegree * 2 - 1)
		for(i <- this.values.indices)
			this.values(i) = new ValueType
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
		else {
			bt.init()
			bt.cleanData()
		}
		bt
	}
}

class BTree(val minDegree: Int, val dataPath: String) {
	/* memoryCapacity limits the number of node that can be loaded in to memory
	 * nodeQueue maintains the ids of the node staying in memory
	 * When a new node is loaded into memory, then an old node must be written
	 * 		into the disk.
	 */
	private val mem: virtualMemory = virtualMemory.empty(10, dataPath)

	private var totalNumKey: Int = -1
	private var numNode: Int = 0		// the total number of nodes in this tree
	private var height: Int = 0			// the height of the tree
	private var rootID: Int = -1
	private var root: Node = _

	def init(): Unit = {
		totalNumKey = 0
		root = mem.allocate(minDegree, 0, true, true)
		numNode += 1
		height += 1
		rootID = root.id
	}

	def getTotalNumKey: Int = totalNumKey


	private def delDir(pname: String): Unit = {
		val path: File = new File(pname)
		if (path.exists && path.isDirectory)
			for (f <- path.listFiles())
				if (f.isFile) f.delete()
	}

	private def min(a: Int, b: Int): Int = if(a < b) a else b

	private def persist(xId: Int): Unit = mem.persist(xId)

	private def unpersist(xId: Int): Unit = mem.unpersist(xId)

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
		persist(xID)
		val y = diskRead(x.childrenID(i))
		persist(y.id)
		val z = mem.allocate(minDegree = minDegree, id = numNode, y.isLeaf, false)
		persist(z.id)
		numNode += 1
		z.numKey = minDegree - 1
		z.degree = if(z.isLeaf) 0 else minDegree
		for(j <- 0 until minDegree - 1) {
			z.keys(j) = y.keys(j + minDegree)
			z.values(j).init(y.values(j + minDegree))
		}
		if (!y.isLeaf)
			for(j <- 0 until minDegree)
				z.childrenID(j) = y.childrenID(j + minDegree)
		y.numKey = minDegree - 1
		y.degree = if(y.isLeaf) 0 else minDegree
		for(j <- 0 until x.degree - i - 1)
			x.childrenID(x.degree - j) = x.childrenID(x.degree - j - 1)
		x.childrenID(i + 1) = z.id
		for(j <- 0 until x.degree - i - 1) {
			x.keys(x.degree - j - 1) = x.keys(x.degree - j - 2)
			x.values(x.degree - j - 1).init(x.values(x.degree - j - 2))
		}
		x.keys(i) = y.keys(minDegree - 1)
		x.values(i).init(y.values(minDegree - 1))
		x.numKey = x.numKey + 1
		x.degree = x.degree + 1
		unpersist(x.id)
		unpersist(y.id)
		unpersist(z.id)
	}

	private def insertNoFull(xID: Int, key: Int, value: ValueType): Unit = {
		// Here x is not full, i.e. x.numKey <= minDegree * 2 - 2
		val x: Node = diskRead(xID)
		persist(x.id)
		var i = x.numKey - 1
		if (x.isLeaf) {
			while(i > -1 && key < x.keys(i)) {
				x.keys(i + 1) = x.keys(i)
				x.values(i + 1).init(x.values(i))
				i = i - 1
			}
			totalNumKey += 1
			x.keys(i + 1) = key
			x.values(i + 1).init(value)
			x.numKey = x.numKey + 1
		}
		else {
			while(i > -1 && key < x.keys(i)) i = i - 1
			i = i + 1
			var y = diskRead(x.childrenID(i))
			persist(y.id)
			if(y.numKey == 2 * minDegree - 1) {
				splitChild(x.id, i)
				if (key > x.keys(i)) {
					unpersist(y.id)
					y = diskRead(x.childrenID(i + 1))
					persist(y.id)
				}
			}
			insertNoFull(y.id, key, value)
			unpersist(y.id)
		}
		unpersist(x.id)
	}


	def insert(key: Int, value: ValueType): Boolean = {
		if(key < 0) return false
		root = diskRead(rootID)
		persist(root.id)
		if(root.numKey == 2 * minDegree - 1) {
			// if root is full
			root.isRoot = false
			root.minNodeNumKey = minDegree - 1
			if(!root.isLeaf) root.minNodeDegree = minDegree
			val newRoot = mem.allocate(minDegree, numNode, false, true)
			numNode += 1
			height += 1
			rootID = newRoot.id

			newRoot.childrenID(0) = root.id
			newRoot.degree = newRoot.degree + 1
			unpersist(root.id)
			persist(newRoot.id)
			splitChild(newRoot.id, 0)
			insertNoFull(newRoot.id, key, value)
			unpersist(newRoot.id)
		}
		else {
			insertNoFull(rootID, key, value)
			unpersist(rootID)
		}
		true
	}

	private def recursiveSearch(id: Int, key: Int): ItemType = {
		val x: Node = diskRead(id)
		persist(x.id)
		var i = 0
		while(i < x.numKey && key > x.keys(i))
			i = i + 1
		var result = new ItemType(key, x.values(i))
		if(i < x.numKey && key == x.keys(i))
			result = new ItemType(key, x.values(i))
		else if (x.isLeaf)
			result = new ItemType(-1, new ValueType)			// No result found
		else
			result = recursiveSearch(x.childrenID(i), key)
		unpersist(x.id)
		result
	}

	def search(key: Int): ItemType = {
		recursiveSearch(rootID, key)
	}

	private def recursiveShow(totalNum: Int, foundNum: Int, xId: Int, container: Array[ItemType]): Int = {
		/* Return the total number of items that have been found after this function */
		val x: Node =diskRead(xId)
		persist(x.id)
		var i = 0
		var currFoundNum = foundNum
		if(currFoundNum == totalNum) {
			unpersist(x.id)
			return currFoundNum
		}
		if(x.isLeaf) {
			val bound: Int = min(currFoundNum + x.numKey, totalNum)
			for (j <- currFoundNum until bound)
				container(j) = new ItemType(x.keys(j - currFoundNum), x.values(j - currFoundNum))
			unpersist(x.id)
			return bound
		}
		while(i < x.degree) {
			currFoundNum = recursiveShow(totalNum, currFoundNum, x.childrenID(i), container)
			if(currFoundNum >= totalNum) {
				unpersist(x.id)
				return totalNum
			}
			if(i < x.numKey) {
				container(currFoundNum) = new ItemType(x.keys(i), x.values(i))
				currFoundNum += 1
			}
			i += 1
		}
		unpersist(x.id)
		currFoundNum
	}

	def show(totalNum: Int): Array[ItemType] = {
		val container: Array[ItemType] = new Array[ItemType](totalNum)
		for(i <- container.indices) container(i) = new ItemType
		recursiveShow(totalNum, 0, rootID, container)
		container
	}

	private def justMerge(xId: Int, leftRank: Int): Unit = {
		val x: Node = diskRead(xId)
		persist(x.id)
		val leftId = x.childrenID(leftRank)
		val rightId = x.childrenID(leftRank + 1)

		val left: Node = diskRead(leftId)
		persist(leftId)
		val right: Node = diskRead(rightId)
		persist(rightId)

		left.keys(left.numKey) = x.keys(leftRank)
		left.values(left.numKey).init(x.values(leftRank))
		for(i <- 0 until right.numKey) {
			left.keys(i + left.numKey + 1) = right.keys(i)
			left.values(i + left.numKey + 1).init(right.values(i))
			left.childrenID(i + left.numKey + 1) = right.childrenID(i)
		}
		left.childrenID(left.numKey + right.numKey + 1) =
			right.childrenID(right.numKey)
		left.numKey += right.numKey + 1
		left.degree += right.degree

		for(i <- leftRank until x.numKey - 1) {
			x.keys(i) = x.keys(i + 1)
			x.values(i).init(x.values(i + 1))
			x.childrenID(i + 1) = x.childrenID(i + 2)
		}
		x.degree -= 1
		x.numKey -= 1

		right.isDestroyed = true
		unpersist(x.id)
		unpersist(left.id)
		unpersist(right.id)
	}

	private def mergeChild(xId: Int, childId: Int, childRank: Int): Unit = {
		val x: Node = diskRead(xId)
		persist(x.id)
		val child: Node = diskRead(childId)
		persist(child.id)
		var needMerge = true
		if(childRank < x.degree - 1) {
			val rightSibling: Node = diskRead(x.childrenID(childRank + 1))
			if(rightSibling.numKey > rightSibling.minNodeNumKey) {
				/* corresponding to case 3a, page 288, Introduction to Algorithms */
				needMerge = false

				// descend a keyword from x into child
				child.keys(child.numKey) = x.keys(childRank)
				child.values(child.numKey).init(x.values(childRank))
				child.childrenID(child.degree) = rightSibling.childrenID(0)
				child.degree += 1
				child.numKey += 1

				// rise the first key of rightSibling to x
				x.keys(childRank) = rightSibling.keys(0)
				x.values(childRank).init(rightSibling.values(0))

				// delete the first key of rightSibling
				for(i <- 0 until rightSibling.numKey - 1) {
					rightSibling.keys(i) = rightSibling.keys(i + 1)
					rightSibling.values(i).init(rightSibling.values(i + 1))
					rightSibling.childrenID(i) = rightSibling.childrenID(i + 1)
				}
				rightSibling.childrenID(rightSibling.numKey - 1) = rightSibling.childrenID(rightSibling.numKey)
				rightSibling.numKey -= 1
				rightSibling.degree -= 1
			}
		}
		if(needMerge && childRank > 0) {
			val leftSibling: Node = diskRead(x.childrenID(childRank - 1))
			if(leftSibling.numKey > leftSibling.minNodeNumKey) {
				/* corresponding case 3a */
				needMerge = false

				// descend the keyword from x into child
				for(i <- 1 until child.numKey + 1) {
				}
				var i = child.numKey
				while(i > 0) {
					child.keys(i) = child.keys(i - 1)
					child.values(i).init(child.values(i - 1))
					child.childrenID(i + 1) = child.childrenID(i)
					i -= 1
				}
				child.childrenID(1) = child.childrenID(0)
				child.keys(0) = x.keys(childRank - 1)
				child.values(0).init(x.values(childRank - 1))
				child.childrenID(0) = leftSibling.childrenID(leftSibling.degree - 1)
				child.degree += 1
				child.numKey += 1

				// rise the last keyword of leftSibling to x
				x.keys(childRank - 1) = leftSibling.keys(leftSibling.numKey - 1)
				x.values(childRank - 1).init(leftSibling.values(leftSibling.numKey - 1))

				// delete the last keyword from leftSibling
				leftSibling.numKey -= 1
				leftSibling.degree -= 1
			}
		}
		if(needMerge) {
			/* merge child with rightSibling */
			/* corresponding case 3b, page 288, Introduction to Algorithms */
			if(childRank < x.degree - 1)
				justMerge(x.id, childRank)
			/* merge child with leftSibling */
			/* corresponding case 3b, page 288, Introduction to Algorithms */
			else if(childRank > 0)
				justMerge(x.id, childRank - 1)
			else {
				println("x.id = ", x.id)
				throw AnywayThisIsAnException()
			}
		}
		unpersist(x.id)
		unpersist(child.id)
	}

	private def delPrecursorOrSuccessor(xId: Int, isPrecursor: Boolean): ItemType = {
		/* xId.numKey >= xId.minNumKey + 1 */
		val x: Node = diskRead(xId)
		persist(x.id)
		try {
			assert(x.isRoot || x.numKey > x.minNodeNumKey)
		}
		catch {
			case e: Throwable =>
				println("x.isRoot = " + x.isRoot)
				println("x.numKey = " + x.numKey)
				println("x.minNodeNumKey = " + x.minNodeNumKey)
				println("x.Id = " + x.id)
				throw e
		}
		if (x.isLeaf) {
			if(isPrecursor) {
				x.numKey -= 1
				x.degree -= 1
				totalNumKey -= 1
				unpersist(x.id)
				return new ItemType(x.keys(x.numKey), x.values(x.numKey))
			}
			else {
				val res = new ItemType(x.keys(0), x.values(0))
				for(i <- 0 until x.numKey - 1) {
					x.keys(i) = x.keys(i + 1)
					x.values(i).init(x.values(i + 1))
				}
				x.numKey -= 1
				x.degree -= 1
				totalNumKey -= 1
				unpersist(x.id)
				return res
			}
		}
		/* else this is a interior node */
		/* corresponding to case 3, page 286, Introduction to Algorithms */
		var childRank = if(isPrecursor) x.degree - 1 else 0
		val child: Node = diskRead(x.childrenID(childRank))
		if (child.numKey == child.minNodeNumKey)
			/* correspongding to case 3a or 3b */
			mergeChild(x.id, child.id, childRank)
		// x can't be root
		if(x.isRoot) {
			println("x.id = ", x.id)
			println("x.isRoot = ", x.isRoot)
			throw AnywayThisIsAnException()
		}
		/* x.degree may change */
		childRank = if(isPrecursor) x.degree - 1 else 0
		val res = delPrecursorOrSuccessor(x.childrenID(childRank), isPrecursor)
		unpersist(x.id)
		res
	}

	private def safeDel(key: Int, xId: Int): Boolean = {
		/* x.numKey >= x.minNodeNumKey + 1 */
		val x: Node = diskRead(xId)
		persist(xId)
		try{
			assert(x.isRoot || x.numKey > x.minNodeNumKey)
		}
		catch {
			case e: Throwable =>
				println("x.isRoot = " + x.isRoot)
				println("x.numKey = " + x.numKey)
				println("x.minNodeNumKey = " + x.minNodeNumKey)
				println("x.Id = " + x.id)
				throw e
		}
		var i = 0
		while(i < x.numKey && key > x.keys(i)) i+= 1
		if(x.isLeaf) {
			/* corresponding case 1, page 286, Introduction to Algorithms */
			/* key is in a current node and current node is a leaf node */

			/* key not found */
			if(i >= x.numKey) { unpersist(x.id); return false }
			if(key < x.keys(i)) { unpersist(x.id); return false }

			/* key is found; delete the key */
			val res = new ItemType(x.keys(i), x.values(i))
			for(j <- i until x.numKey - 1) {
				x.keys(j) = x.keys(j + 1)
				x.values(j).init(x.values(j + 1))
			}
			totalNumKey -= 1
			x.numKey -= 1
			unpersist(x.id)
			return true
		}
		/* is not leaf */
		if(i < x.numKey && key == x.keys(i)) {
			/* corresponding to case 2, page 298, Introduction to Algorithms */
			/* key is in this interior node */
			val lId: Int = x.childrenID(i)
			val rId: Int = x.childrenID(i + 1)

			val left: Node = diskRead(lId)
			val right: Node = diskRead(rId)
			persist(lId)
			persist(rId)
			if(left.numKey >= left.minNodeNumKey + 1) {
				/* corresponding to case 2a */
				/* find the precursor in left node */
				val precursor = delPrecursorOrSuccessor(left.id, isPrecursor = true)
				/* replace the keyword (that is to be deleted) with its precursor */
				x.keys(i) = precursor.key
				x.values(i).init(precursor.value)
				unpersist(lId)
				unpersist(rId)
				unpersist(xId)
				true
			}
			else if(right.numKey >= right.minNodeNumKey + 1) {
				/* corresponding to case 2b */
				/* find the successor in right node */
				val successor = delPrecursorOrSuccessor(right.id, isPrecursor = false)
				/* replace the keyword (that is to be delted) with its successor */
				x.keys(i) = successor.key
				x.values(i).init(successor.value)
				unpersist(lId)
				unpersist(rId)
				unpersist(xId)
				true
			}
			else {
				/* corresponding to case 2c */
				justMerge(x.id, i)
				val res = safeDel(key, x.childrenID(i))
				unpersist(lId)
				unpersist(rId)
				unpersist(xId)
				res
			}
		}
		else {
			/* corresponding to case 3, page 286, Introduction to Algorithms */
			/* key is not in this interior node */
			val isOutofBound = if(i >= x.numKey) true else false
			var childId = x.childrenID(i)
			val child: Node = diskRead(childId)
			/* corresponding to case 3a or 3b */
			if(child.numKey == child.minNodeNumKey) {
				val oldDegree = x.degree
				mergeChild(x.id, child.id, i)
				if(i > 0 && !isOutofBound && key < x.keys(i - 1)) childId = x.childrenID(i - 1)
				else if(isOutofBound && x.degree < oldDegree) childId = x.childrenID(i - 1)
				if(x.isRoot && x.numKey == 0) {
					x.isDestroyed = true
					rootID = x.childrenID(0)
					height -= 1
					val newRoot = diskRead(rootID)
					newRoot.minDegree = x.minDegree
					newRoot.isRoot = x.isRoot
					newRoot.minNodeNumKey = if(newRoot.isLeaf) 0 else 1
					newRoot.minNodeDegree = if(newRoot.isLeaf) 0 else 2
					newRoot.maxNodeDegree = if(newRoot.isLeaf) 0 else 2 * newRoot.minDegree
				}
			}
			val res = safeDel(key, childId)
			unpersist(x.id)
			res
		}
	}

	def delete(key: Int): Boolean = {
		safeDel(key, rootID)
	}

	def cleanData(): Unit = {
		/* clean data file */
		delDir(dataPath)
	}



	def load(): Unit = {
		val file = Source.fromFile(dataPath + "root.dat")
		val iter = file.getLines()
		rootID = iter.next.split("\t")(1).toInt
		numNode = iter.next.split("\t")(1).toInt
		height = iter.next.split("\t")(1).toInt
		totalNumKey = iter.next.split("\t")(1).toInt
		file.close()
		root = diskRead(rootID)
	}

	def save(): Unit = {
		val writer = new FileWriter(dataPath + "root.dat")
		writer.write("rootID:\t" + rootID + "\n")
		writer.write("numNode:\t" + numNode + "\n")
		writer.write("height:\t" + height + "\n")
		writer.write("totalNumKey:\t" + totalNumKey + "\n")
		writer.close()
		diskWrite()
	}
}
