
import java.awt.Dimension

import BTree.{BTree, ValueType, ItemType}
import MyExceptions.NegativeKeyException

import scala.swing._
import java.io.File


import scala.swing.Dialog.Message
import scala.swing.GridBagPanel.Fill
import scala.swing.event.ButtonClicked

object MainWindow extends SimpleSwingApplication{
	val dataPath = "./src/data/"
	var btree: BTree = BTree.instance(4, dataPath)

	def top: MainFrame = new MainFrame {
		// delDir("./src/data/")
		title = "Database"

		//  buttons
		val insertBtn: Button = new Button("Insert")
		val insert100Btn: Button = new Button("Insert 100")
		val searchBtn: Button = new Button("Search")
		val delBtn: Button = new Button("Delete")
		val showBtn: Button = new Button("Show")
		val newBtn: Button = new Button("New")
		val saveBtn: Button = new Button("Save")

		// table and text area
		val rowData = new Array[Array[Any]](200)
		for(i <- rowData.indices)
			rowData(i) = new Array[Any](5)
		var items: Table = new Table(rowData, Array("itemID", "Key", "Age", "Name", "School")) {
			preferredViewportSize = new Dimension(500, 200)
		}
		def tableClear(): Unit = {
			for(i <- rowData.indices)
				for(j <- rowData(i).indices)
					items.update(i, j, null)
		}
		def tableUpdate(row: Int, res: ItemType) : Unit = {
			items.update(row, 0, row)
			items.update(row, 1, res.key)
			items.update(row, 2, res.value.age)
			items.update(row, 3, res.value.name)
			items.update(row, 4, res.value.school)
		}
		def itemsNum: Int = rowData.length
		val output: TextArea = new TextArea(10, 40) {editable = false}

		val display: BoxPanel = new BoxPanel(Orientation.Vertical) {
			contents += new ScrollPane(items)
			contents += new ScrollPane(output)
		}

		val buttons: BoxPanel = new BoxPanel(Orientation.Vertical) {
			contents += insertBtn
			contents += insert100Btn
			contents += searchBtn
			contents += delBtn
			contents += showBtn
			contents += newBtn
			contents += saveBtn

			listenTo(insertBtn, insert100Btn, searchBtn, delBtn, showBtn, newBtn, saveBtn)

			reactions += {
				case ButtonClicked(`insertBtn`) =>
					val insertDialog = new Dialog
					insertDialog.open()
					insertDialog.centerOnScreen()
					insertDialog.contents = new GridBagPanel {
						val prompt = new Label("Input data:")

						val keyLabel: Label = new Label("Key:")
						val nameLabel: Label = new Label("Name:")
						val ageLabel: Label = new Label("Age:")
						val schoolLabel: Label = new Label("School")

						val keyTxt: TextField = new TextField(15) {editable=true}
						val nameTxt: TextField = new TextField(15) {editable=true}
						val ageTxt: TextField = new TextField(15) {editable=true}
						val schoolTxt: TextField = new TextField(15) {editable=true}

						val randomBtn: Button = Button("Random") {
							keyTxt.text = scala.util.Random.nextInt(10000).toString
							nameTxt.text = "ToolManNo" + keyTxt.text
							ageTxt.text = (scala.util.Random.nextInt(20) + 20).toString
							schoolTxt.text = "ToolSchollNo" + keyTxt.text
						}
						val cancelBtn: Button = Button("Cancel") {insertDialog.close()}
						val OKBtn: Button = Button("OK!"){
							def printItem = {
								output.append("You just input:\n")
								output.append("Key = " + keyTxt.text + "\n")
								output.append("Name = " + nameTxt.text + "\n")
								output.append("age = " + ageTxt.text + "\n")
								output.append("school = " + schoolTxt.text + "\n\n")
							}
							if(	keyTxt.text.length > 0 &&
								nameTxt.text.length > 0 &&
								ageTxt.text.length > 0 &&
								schoolTxt.text.length > 0) {
								try {
									val value = new ValueType(nameTxt.text, ageTxt.text.toInt, schoolTxt.text)
									if(!btree.insert(keyTxt.text.toInt, value)) throw NegativeKeyException()
									output.append("Inserted successful!\n")
									printItem
								} catch {
									case e: NumberFormatException =>
										output.append("Invaid data format!\n")
										output.append("Please input an integer for key and age!\n")
										printItem
										insertDialog.close()
									case e: NegativeKeyException =>
										output.append("Key can't be negative!\n")
										printItem
										insertDialog.close()
									case _: Throwable =>
										output.append("Some unexpected exception occurred.\n\n")
										insertDialog.close()
								}
							}
							else if(keyTxt.text.length <= 0)
								output.append("Please input key!\n\n")
							else if(nameTxt.text.length <= 0)
								output.append("Please input name!\n\n")
							else if(ageTxt.text.length <= 0)
								output.append("Please input age!\n\n")
							else if(schoolTxt.text.length <= 0)
								output.append("Please input school!\n\n")
							insertDialog.close()
						}
						val c: Constraints = new Constraints()
						c.fill = Fill.Horizontal
						c.grid = (1, 0)
						c.gridwidth = 1
						c.weighty = 2
						c.weightx = 1
						layout(prompt) = c
						c.weighty = 1
						c.gridwidth = 1
						c.grid = (0, 1)
						layout(keyLabel) = c
						c.grid = (0, 2)
						layout(nameLabel) = c
						c.grid = (0, 3)
						layout(ageLabel) = c
						c.grid = (0, 4)
						layout(schoolLabel) = c

						c.grid = (1, 1)
						layout(keyTxt) = c
						c.grid = (1, 2)
						layout(nameTxt) = c
						c.grid = (1, 3)
						layout(ageTxt) = c
						c.grid = (1, 4)
						layout(schoolTxt) = c

						c.grid = (0, 5)
						layout(randomBtn) = c
						c.grid = (1, 5)
						layout(OKBtn) = c
						c.grid = (2, 5)
						layout(cancelBtn) = c
					}
				case ButtonClicked(`insert100Btn`) =>
					for(i <- 0 until 100) {
						val key = i
						val name = "ToolMan " + key
						val age = 20 + scala.util.Random.nextInt(20)
						val school = "ToolSchool " + key
						btree.insert(key, new ValueType(name, age, school))
					}
					output.append("Insert 100 items!\n\n")
				case ButtonClicked(`searchBtn`) =>
					val s = Dialog.showInput(buttons,
						"Input the key \n you want to search",
						"Search",
						Message.Plain,
						Swing.EmptyIcon,
						Nil, "")
					try {
						if(s.isDefined && s.get.length > 0) {
							if(s.get.toInt < 0) throw NegativeKeyException()
							val result = btree.search(s.get.toInt)
							if(result.key == -1)
								output.append("Key " + s.get + " was not found!\n\n")
							else {
								tableClear()
								tableUpdate(0, result)
								output.append("Query Successful!\n\n")
							}
						}
						else output.append("Invalid input!\n\n")
					} catch {
						case e: NumberFormatException =>
							output.append("Please input an integer!\n\n")
						case e: NegativeKeyException =>
							output.append("Key can't be negative!\n\n")
						case _: Throwable =>
							output.append("Some unexpected exceptions occurred!\n\n")
					}
				case ButtonClicked(`delBtn`) =>
					val s = Dialog.showInput(buttons,
						"Input the key \n you want to delete",
						"Delete",
						Message.Plain,
						Swing.EmptyIcon,
						Nil, "")
					try {
						if(s.isDefined && s.get.length > 0) {
							if(s.get.toInt < 0) throw NegativeKeyException()
							val result = btree.delete(s.get.toInt)
							if(!result)
								output.append("Key " + s.get + " was not in the database!\n\n")
							else output.append("Deleted Successful!\n\n")
						}
						else output.append("Invalid input!\n\n")
					} catch {
						case e: NumberFormatException =>
							output.append("Please input an integer!\n\n")
						case e: NegativeKeyException =>
							output.append("Key can't be negative!\n\n")
						case _: Throwable =>
							output.append("Some unexpected exceptions occurred!\n\n")
					}

				case ButtonClicked(`showBtn`) =>
					val results: Array[ItemType] = btree.show(itemsNum)
					tableClear()
					var foundNum = 0
					while(foundNum < results.length && results(foundNum).key >= 0)
						foundNum += 1
					for(i <- 0 until foundNum)
						tableUpdate(i, results(i))
					if(foundNum != 0) {
						output.append("The first " + foundNum + " items have been retrieved.\n")
						output.append("Total number of items in database: " + btree.getTotalNumKey + "\n\n")
					}
					else
						output.append("No item in the database yet!\n\n")
				case ButtonClicked(`newBtn`) =>
					val newDialog = new Dialog
					newDialog.open()
					newDialog.centerOnScreen()
					newDialog.contents = new GridBagPanel {
						val prompt: Label = new Label("Are you sure?")
						val okBtn: Button = Button("OK!") {
							btree.cleanData()
							btree = BTree.instance(4, dataPath)
							output.append("A new database has been created!\n\n")
							newDialog.close()
						}
						val cancelBtn: Button = Button("Cancel"){ newDialog.close() }
						val c: Constraints = new Constraints()
						c.fill = Fill.Horizontal
						c.grid = (0, 0)
						c.gridwidth = 2
						c.weightx = 1
						c.weighty = 3
						layout(prompt) = c
						c.gridwidth = 1
						c.grid = (0, 1)
						c.weightx = 1
						c.weighty = 1
						layout(cancelBtn) = c
						c.grid = (1, 1)
						c.weightx = 1
						layout(okBtn) = c
					}
				case ButtonClicked(`saveBtn`) =>

					btree.save()
					output.append("Saved!\n\n")
			}
		}

		contents = new SplitPane(Orientation.Vertical, buttons, display) {
			continuousLayout = true
		}
		centerOnScreen()
	}
}
