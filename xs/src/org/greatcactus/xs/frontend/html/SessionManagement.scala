/**
 * Copyright 2012-2014 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable.PriorityQueue
import scala.math.Ordered
import scala.collection.mutable.ListBuffer
import scala.concurrent._
//import ExecutionContext.Implicits.global
import scala.concurrent.Promise
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.Executors
import org.greatcactus.xs.frontend.XSClipBoard
import org.greatcactus.xs.frontend.XSClipboardRequest

/**
 * Keep track of who is still talking to us (and thus for whom we need to keep track of what is currently showing on their browser in order to be able to send diffs).
 *
 */


object SessionManagement {
  var sessionTimeout = 1000 * 60 * 60 // one hour
  
  private var sessions : Map[String,HTTPSession] = Map.empty
  def removeSession(session:HTTPSession) {
    synchronized {
      sessions-=session.id
    }
  }/*
  def addSession(session:HTTPSession) {
    synchronized {
      sessions+=session.id->session
    }
  }*/
  private var gcCount = 0;
  private var gcInverseFrequency = 10000
  val random = new java.util.Random
  def get(id:String) : Option[HTTPSession] = {
    var res = sessions.get(id)
    for (s<-res) s.keepAlive()
    synchronized {
      gcCount+=1
      if (gcCount==gcInverseFrequency) { gcCount=0; gc(); }  
    }
    for (s<-res if !s.disposed) yield s
  }
  def gc() {
    val now = System.currentTimeMillis()
    for (s<-sessions.values) if (s.shouldDie(now)) s.dispose()
  }
  /** Get a new ID for a session, and add the session. Done at this point (before the session object is fully created) to prevent exceedingly unlikely race condition. */
  def newID(session:HTTPSession) = {
    def rndID() = (random.nextLong()&0xffffffffffffL).toString
    var res = rndID()
    synchronized {
       while (sessions.contains(res)) res=rndID()
       sessions+=res->session
    }
    res
  } 
  
  /** Used for timeout in Session.cometCallFuture. If that method is not used, then this will never be created, which means there is no need to shut it down. */
  lazy val scheduler:ScheduledExecutorService = Executors.newScheduledThreadPool(1) 
}

/** Used for dependency injection. Not added by default as there could be multiple session prefixes in a given dependency injection tree. */
class HTTPSessionPrefix(val prefix:String)

class HTTPSession(val worker:HTTPSessionWorker) {
  val id:String = SessionManagement.newID(this)
  /** Gets appended before each action command */
  val jsSessionID = "S"+id
  val sessionPrefixNoTrailingPeriod = "xs."+jsSessionID 
  val sessionPrefix = sessionPrefixNoTrailingPeriod+"." 
  val createSessionHTML = 
    <script type="text/javascript">
      xs.S{id}=new xs.Session('{id}');
    </script>
  var lastTimeTalkedToClient = System.currentTimeMillis()
  def shouldDie(now:Long) = lastTimeTalkedToClient+SessionManagement.sessionTimeout < now
  def keepAlive() { lastTimeTalkedToClient = System.currentTimeMillis() }
  @volatile var disposed = false
  final def dispose() {
    synchronized {
      if (!disposed) {
        disposed=true
        SessionManagement.removeSession(this)
        clientDispose()
      }
    }
  }
  def clientDispose() { worker.dispose() }
  
  /** Messages to be sent to client. */
  private[this] var pendingSendToClient = new LinkedBlockingQueue[ClientMessage]
  /** A promise that can be completed by the next message that should be sent to the client */
  private[this] var pendingResponse : Option[Promise[Option[ClientMessage]]] = None
  /** A function that can consume messages that should be sent to the client */
  private[this] var directSendToClient : Option[ClientMessage => Unit] = None

  
  def setDirectSendToClient(callback:ClientMessage => Unit) {
    outgoingSyncObject.synchronized {
      for (message<-cometCallShouldReturnImmediately()) callback(message)
      directSendToClient=Some(callback)
    }
  }
  /** A better way to do comet calls - via futures. */
  def cometCallFuture : Future[Option[ClientMessage]] = {
    // println("Pre synchronized block in cometCallFuture")
    outgoingSyncObject.synchronized {
      println("Starting cometCallFuture for session "+id)
      pendingResponse match {
        case Some(p) => p.success(None); pendingResponse=None // may happen if the server goes to sleep for a long time.
        case None =>
      }
      // if (pendingResponse.isDefined) throw new IllegalArgumentException("Can't call cometCallFuture until previous call resolved")
      val immediate = cometCallShouldReturnImmediately()
      val promise = scala.concurrent.Promise[Option[ClientMessage]]
      if (immediate.isDefined) promise.success(immediate)
      else {
        pendingResponse=Some(promise)
        SessionManagement.scheduler.schedule(new Runnable() {override def run {timeoutPromise(promise)}},10,java.util.concurrent.TimeUnit.SECONDS)
      }
      promise.future
    }
    /* Simpler code that does the same thing, at the expense of using a thread to block 
    scala.concurrent.future {
      cometCallBlocking()
    }*/
  }
  
  /** force a promise to timeout if it is still the current one */
  private def timeoutPromise(promise:Promise[Option[ClientMessage]]) {
    outgoingSyncObject.synchronized {
      pendingResponse match {
        case Some(p) if p eq promise => pendingResponse=None; promise.success(None)
        case None =>
      }
    }
  }
  
  def cometCallShouldReturnImmediately() : Option[ClientMessage] = {
    keepAlive()
    val buffer = new ClientMessageBuffer
    var busyness : SimpleClientMessage = null // optimization - ignore all client busy messages other than the current one. This variable stores the most current one in thw queue so far.
    def hasAvailable() = {
      val head = pendingSendToClient.poll()
      head match {
        case null => false
        case m:SimpleClientMessage if m.command=="ServerStatus" => // optimization - ignore all client busy messages other than the current one.
          if (busyness==null || busyness.args(1).toLong < m.args(1).toLong) busyness=m
          true 
        case _ => buffer+=head; true
      }
    }
    while (hasAvailable()) {}
    if (busyness!=null) buffer+=busyness
    buffer.get
  }
  
  /** A comet call method that blocks for 10s if nothing is present. You are probably better off using cometCallFuture unless you are on a platform that doesn't support non-blocking responses.*/
  def cometCallBlocking() : Option[ClientMessage] = {
    keepAlive()
    Option(pendingSendToClient.poll(10,java.util.concurrent.TimeUnit.SECONDS))
  }
  
  def addMessage(message:ClientMessage) {
    outgoingSyncObject.synchronized {
      directSendToClient match {
        case Some(callback) => callback(message)
        case None => pendingResponse match {
          case None => pendingSendToClient.add(message)
          case Some(promise) =>
            pendingResponse=None
            assert (pendingSendToClient.isEmpty)
            promise.success(Some(message))
        }
      }
    }  
  }
  
  val incomingSyncObject = new Object
  val outgoingSyncObject = new Object
  var numProcessedFromClientMessage = 0L
  var largestFromClientMessage = -1L
  class SavedClientMessage(val count:Long,val message:ClientMessage) extends Ordered[SavedClientMessage] {
    def compare(that: SavedClientMessage): Int = if (count==that.count) 0 else if (count<that.count) 1 else -1 // want small count first.
  }
  var savedFromClientMessages = new PriorityQueue[SavedClientMessage]
  
  /**
   * When messages are sent via HTTP posts, it is possible (but unlikely) that they could get reordered.
   * This is a significant problem with editing, as more recent data may get clobbered by old data. For this
   * reason we have a manual orderer. It works as follows:
   * 
   * Each message the client sends has a count (starting from zero). After sending messages, the client saves them in case retransmission is needed.
   *  
   * If a message arrives in order, it is processed. If this makes it possible to process future saved messages, they are processed. 
   * If a message from the future arrives, it is saved for later use.
   * If a message with an already seen count is received, it is discarded.
   * In any case, an ACK is sent with the message received count and the expected next message. If this is less than the message received,
   * then it means that some earlier messages have possibly been lost, and they will be resent.
   */
  def receivedPossiblyUnorderedMessage(message:ClientMessage,messageCount:Long) : SimpleClientMessage = {
    incomingSyncObject.synchronized {
      if (messageCount==numProcessedFromClientMessage) {
        numProcessedFromClientMessage+=1
        receivedOrderedMessage(message)
        while ((!savedFromClientMessages.isEmpty)&&(savedFromClientMessages.head.count==numProcessedFromClientMessage)) {
          numProcessedFromClientMessage+=1
          receivedOrderedMessage(savedFromClientMessages.dequeue.message)
        }
      } else if (messageCount>numProcessedFromClientMessage && !savedFromClientMessages.exists(_.count==messageCount)) {
        savedFromClientMessages+=new SavedClientMessage(messageCount,message)
      } else None
      if (messageCount>largestFromClientMessage) largestFromClientMessage=messageCount
      ClientMessage.acknowledge(messageCount,numProcessedFromClientMessage,largestFromClientMessage)
    }
  }
  
  def receivedOrderedMessage(message:ClientMessage) {
   try {
     message match {
      case sm:SimpleClientMessage =>
        //println("Received message "+sm.command)
        if (sm.command=="CloseConnection") {
           println("closed connection for "+id)
           dispose()
        } else if (sm.command=="ReqWSAck" && sm.args.length==1) {
           addMessage(ClientMessage.websocketAckMessage(sm.args(0)))
        } else worker.receivedMessage(sm)
      case mm:MultipleClientMessage => for (sub<-mm.commands) receivedOrderedMessage(sub)
      case _ => throw new IllegalArgumentException(message.toString)
    }
   } catch { case e:Exception => e.printStackTrace(); }
  }
  
  //SessionManagement.addSession(this)
  
  private var localClipboard : Option[XSClipBoard] = None 
    
  def getClipboard(param:XSClipboardRequest,executionContext:ExecutionContext) : Future[XSClipBoard] = {
   Future ( 
  
    localClipboard match {
      case None => throw new IllegalArgumentException("Nothing on the clipboard")
      case Some(clip) =>
        if (clip.datatype==param) clip
        else throw new IllegalArgumentException("Clipboard is in the wrong format")
    }
   )(executionContext)
  }
  def setClipboard(data:XSClipBoard) {
    localClipboard=Some(data)
  }

}

abstract class HTTPSessionWorker {
  def dispose() {}
  def receivedMessage(message:SimpleClientMessage) {}
  def getDraggedElement(subid:String) : Option[XSClipBoard] 
}
