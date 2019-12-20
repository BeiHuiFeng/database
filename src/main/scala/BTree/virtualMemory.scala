package BTree

import java.io.{File, FileWriter}

import MyExceptions.Segmentfault

import scala.collection.mutable
import scala.io.Source

object virtualMemory {
	def empty(memCapacity: Int, dataPath: String) = new virtualMemory(memCapacity, dataPath)
}

class virtualMemory(var memCapacity: Int, var dataPath: String) {
	private var head: Int = 0
	private var tail: Int = 0
	private val idSet: mutable.BitSet = mutable.BitSet.empty
	// private val persistSet: mutable.BitSet = mutable.BitSet.empty
	private val persistCount: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
	private val idMapNode: mutable.Map[Int, Node] = mutable.Map[Int, Node]()
	private val memory: Array[Node] = new Array[Node](memCapacity)
	for(i <- 0 until memCapacity)
		memory(i) = new Node

	private def delDir(pname: String): Unit = {
		val path: File = new File(pname)
		if (path.exists && path.isDirectory)
			for (f <- path.listFiles())
				if (f.isFile) f.delete()
	}

	private def delFile(fname: String): Unit = {
		val file: File = new File(fname)
		if(file.exists && file.isFile)
			file.delete()
	}

	private def isInMem(id: Int): Boolean = idSet.contains(id)
	private def isFull: Boolean = idSet.size == memCapacity
	private def free(): Unit = {
		// free the last node and write it to file
		assert(idSet.nonEmpty)
		assert(persistCount.size <= memCapacity - 1)
		while(persistCount.contains(memory(tail).id) && persistCount(memory(tail).id) > 0) {
			tail = (tail + 1) % memCapacity
			head = (head + 1) % memCapacity
		}
		val n = memory(tail)
		tail = (tail + 1) % memCapacity
		idSet -= n.id
		idMapNode -= n.id
		val filename = dataPath + n.id + ".dat"

		if(n.isDestroyed) {
			delFile(filename)
			return
		}

		val writer = new FileWriter(filename)
		writer.write("minDegree:\t" + n.minDegree + "\n")
		writer.write("id:\t" + n.id + "\n")
		writer.write("isLeaf:\t" + n.isLeaf + "\n")
		writer.write("isRoot:\t" + n.isRoot + "\n")
		writer.write("numKey:\t" + n.numKey + "\n")
		writer.write("degree:\t" + n.degree + "\n")
		writer.write("keys:\n")
		for(i <- 0 until n.numKey)
			writer.write(i + "\t" + n.keys(i) + "\n")
		writer.write("values:\n")
		for(i <- 0 until n.numKey)
			writer.write(i + "\tname:\t" + n.values(i).name +
				"\tage:\t" + n.values(i).age + "\tschool\t" + n.values(i).school + "\n")
		writer.write("childrenID:\n")
		for(i <- 0 until n.degree)
			writer.write(i + "\t" + n.childrenID(i) + "\n")
		writer.write("minNodeNumKey\t" + n.minNodeNumKey + "\n")
		writer.write("maxNodeNumKey\t" + n.maxNodeNumKey + "\n")
		writer.write("minNodeDegree\t" + n.minNodeDegree + "\n")
		writer.write("maxNodeDegree\t" + n.maxNodeDegree + "\n")
		writer.close()

	}


	def allocate(minDegree: Int, id: Int, isLeaf: Boolean, isRoot: Boolean): Node = {
		if(isFull) free()
		val n = memory(head)
		n.init(minDegree, id, isLeaf, isRoot)
		head = (head + 1) % memCapacity
		idSet += n.id
		idMapNode += n.id -> n
		n
	}

	def read(id: Int): Node = {
		/* if x is in memory, then this is an empty operation
		 * otherwise we need to load the node from disk
		 */
		if(isInMem(id)) return idMapNode(id)
		if(isFull) free()
		val x = memory(head)
		head = (head + 1) % memCapacity
		val filename = dataPath + id + ".dat"
		val file = Source.fromFile(filename)
		val iter = file.getLines()
		x.init(
			iter.next.split("\t")(1).toInt,
			iter.next.split("\t")(1).toInt,
			iter.next.split("\t")(1).toBoolean,
			iter.next.split("\t")(1).toBoolean
		)
		try {
			x.numKey = iter.next.split("\t")(1).toInt
			x.degree = iter.next.split("\t")(1).toInt
			/* Neglect the line "keys:" and read the keys one by one */
			iter.next()
			for (i <- 0 until x.numKey)
				x.keys(i) = iter.next.split("\t")(1).toInt
			/* Neglect the line "values: " and read the values one by one */
			iter.next()
			for (i <- 0 until x.numKey) {
				val value = iter.next.split("\t")
				x.values(i).init(value(2), value(4).toInt, value(6))
			}
			iter.next()
			for (i <- 0 until x.degree)
				x.childrenID(i) = iter.next.split("\t")(1).toInt
			assert(x.minNodeNumKey == iter.next.split("\t")(1).toInt)
			assert(x.maxNodeNumKey == iter.next.split("\t")(1).toInt)
			assert(x.minNodeDegree == iter.next.split("\t")(1).toInt)
			assert(x.maxNodeDegree == iter.next.split("\t")(1).toInt)
		} catch {
			case e: Throwable=>
				println("x.id = " + x.id)
				println("x.isLeaf = " + x.isLeaf)
				println("x.isRoot = " + x.isRoot)
				throw e
		}
		file.close()
		idSet += x.id
		idMapNode += x.id -> x
		x
	}

	def write(): Unit = {
		while(idSet.nonEmpty) free()
	}

	def persist(xId: Int): Unit = {
		if(persistCount.size >= memCapacity - 1) throw Segmentfault()
		if(persistCount.contains(xId)) persistCount(xId) += 1
		else persistCount += xId -> 1
	}

	def unpersist(xId: Int): Unit = {
		if(persistCount.contains(xId)) {
			persistCount(xId) -= 1
			if(persistCount(xId) <= 0)
				persistCount -= xId
		}
	}

}
