package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {
    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val maybeOperator = args(args.length - 2)
      val filename = args(args.length - 1)
      val content = createContent(args, args.length - 2)

      if (maybeOperator equals ">>") doEcho(state, content, filename, true)
      else if (maybeOperator equals ">") doEcho(state, content, filename, false)
      else state.setMessage(createContent(args, args.length))
    }
  }

  def doEcho(state: State, content: String, filename: String, appendMode: Boolean): State = {
    if (filename contains Directory.SEPARATOR) state.setMessage("Echo: Filename must not contain separators!")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, content, appendMode)
      if (newRoot == state.root) state.setMessage(filename + ": no such file!")
      else State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }

  }

  // topIndex is noninclusive!
  def createContent(input: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, accumulator: String): String = {
      if (currentIndex >= topIndex) accumulator
      else createContentHelper(currentIndex + 1, accumulator + " " + input(currentIndex))
    }

    createContentHelper(0, "")
  }

  def getRootAfterEcho(currentDirectory: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if (path.isEmpty) currentDirectory
    else if (path.tail.isEmpty) {
      val dirEntry: DirEntry = currentDirectory.findEntry(path.head)

      if (dirEntry == null) currentDirectory.addEntry(new File(currentDirectory.path, path.head, contents))
      else if (dirEntry.isDirectory) currentDirectory
      else if (append) currentDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
      else currentDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    }
    else {
      val nextDirectory = currentDirectory.findEntry(path.head).asDirectory
      val newNextDirectory = getRootAfterEcho(nextDirectory, path.tail, contents, append)

      if (newNextDirectory == nextDirectory) currentDirectory
      else currentDirectory.replaceEntry(path.head, newNextDirectory)
    }

  }
}
