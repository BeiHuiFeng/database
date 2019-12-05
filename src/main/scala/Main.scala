
import java.io.FileWriter

import BTree.{BTree, Node}

object Main {
	def main(args: Array[String]): Unit = {
		val btree: BTree = BTree.instance(4, "./src/data/")
		for(i <- 0 until 20)
			btree.insert(i)
		btree.save()
	}
}
