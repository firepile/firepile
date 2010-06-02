package simplecl

import com.nativelibs4java.opencl._

class SCLEvent(evt: CLEvent) {
  val _CLEvent = evt

  def commandExecutionStatus: CLEvent.CommandExecutionStatus = _CLEvent.getCommandExecutionStatus
  def commandType: CLEvent.CommandType = _CLEvent.getCommandType
  def profilingCommandEnd: Long = _CLEvent.getProfilingCommandEnd
  def profilingCommandQueued: Long = _CLEvent.getProfilingCommandQueued
  def profilingCommandStart: Long = _CLEvent.getProfilingCommandStart
  def profilingCommandSubmit: Long = _CLEvent.getProfilingCommandSubmit
  def invokeUponCompletion(action: Runnable): Unit = _CLEvent.invokeUponCompletion(action)
  def release: Unit = _CLEvent.release
  override def toString: String = _CLEvent.toString
  def waitFor: Unit = _CLEvent.waitFor

}

