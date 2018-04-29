/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime.interpreter

import ca.uwaterloo.flix.api
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.InternalRuntimeException
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.locks

sealed trait Value

object Value {

  /**
    * The `Unit` value.
    */
  object Unit extends Value

  /**
    * The `True` value.
    */
  object True extends Value

  /**
    * The `False` value.
    */
  object False extends Value

  /**
    * A character value.
    */
  case class Char(lit: scala.Char) extends Value

  /**
    * A Float32 value.
    */
  case class Float32(lit: scala.Float) extends Value

  /**
    * A Float64 value.
    */
  case class Float64(lit: scala.Double) extends Value

  /**
    * An Int8 value.
    */
  case class Int8(lit: scala.Byte) extends Value

  /**
    * An Int16 value.
    */
  case class Int16(lit: scala.Short) extends Value

  /**
    * An Int32 value.
    */
  case class Int32(lit: scala.Int) extends Value

  /**
    * An Int64 value.
    */
  case class Int64(lit: scala.Long) extends Value

  /**
    * A BigInt value.
    */
  case class BigInt(lit: java.math.BigInteger) extends Value

  /**
    * A String value.
    */
  case class Str(lit: java.lang.String) extends Value

  /**
    * An Array value.
    */
  case class Arr(elms: Array[AnyRef], tpe: Type) {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Arr does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Arr does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Arr does not support `toString`.")
  }

  /**
    * A Boxed value.
    */
  class Box extends Value {
    /**
      * The internal value of the box.
      */
    private var value: AnyRef = _

    /**
      * Returns the value inside the box.
      */
    def getValue: AnyRef = value

    /**
      * Mutates the value inside the box.
      */
    def setValue(x: AnyRef): Unit = {
      value = x
    }

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Box does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Box does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Box does not support `toString`.")
  }

  /**
    * A Closure value.
    */
  case class Closure(sym: Symbol.DefnSym, bindings: Array[AnyRef]) extends Value {
    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Closure does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Closure does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Closure does not support `toString`.")
  }

  /**
    * Flix internal representation of tags.
    */
  case class Tag(enum: Symbol.EnumSym, tag: String, value: AnyRef) extends Value with api.Tag {
    def getTag: String = tag

    def getBoxedTagValue: AnyRef = value

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tag does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tag does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tag does not support `toString`.")
  }

  /**
    * A Tuple value.
    */
  case class Tuple(elms: List[AnyRef]) extends Value with api.Tuple {
    def getBoxedValue: Array[AnyRef] = elms.toArray

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Tuple does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Tuple does not support `hashCode`.")

    final override def toString: String = throw InternalRuntimeException(s"Value.Tuple does not support `toString`.")
  }

  case class Channel(len: Int, tpe: Type) extends  Value {
    private val contentType: Type = tpe

    private val capacity: Int = len

    private val content: AnyRef = new ConcurrentLinkedQueue[AnyRef]()

    private val waitingPutters: AnyRef = new ConcurrentLinkedQueue[Thread]()

    private val waitingGetters: AnyRef = new ConcurrentLinkedQueue[Thread]()

    def put(value: AnyRef): Channel = {
      this.synchronized {
        val c = content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wg = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wp = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]

        println(s"Thread: ${Thread.currentThread().getId()}  -  Put").asInstanceOf[AnyRef]

        if (c.size() < capacity) {  //If channel has room for more content
          println(s"Thread: ${Thread.currentThread().getId()}  -  Add to content").asInstanceOf[AnyRef]
          c.add(value)
          println(s"      c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
          
          wg.peek() match {           //Lookup if any waiting getters exists
            case null =>              //If no waiting getters exists DO NOTHING
            case _ =>                 //If some waiting getters exist NOTIFY IT
              val wgToNotify = wg.peek().asInstanceOf[Thread]
              println(s"Thread: ${Thread.currentThread().getId()}  -  Notifies Thread ${wgToNotify.getId()}").asInstanceOf[AnyRef]
              notifyAll()
          }
        }
        else {                      //If channel is full
          println(s"Thread: ${Thread.currentThread().getId()}  -  Add to Waiting Putters").asInstanceOf[AnyRef]
          wp.add(Thread.currentThread())
          println(s"      c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]

          wg.peek() match {           //Lookup waiting getters
            case null =>              //If no waiting getters exists GO TO SLEEP
              println(s"Thread: ${Thread.currentThread().getId()} goes to sleep!").asInstanceOf[AnyRef]
              wait()

              println(s"PUTTER ${Thread.currentThread().getId()} WOKEN").asInstanceOf[AnyRef]
              println(s"Thread: ${Thread.currentThread.getId()}  -  Remove self from Waiting Putters, put content").asInstanceOf[AnyRef]
              wp.poll()
              c.add(value)
              println(s"      c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
            case _ =>                 //If some waiting getters exist NOTIFY IT
              val wgToNotify = wg.peek().asInstanceOf[Thread]
              println(s"Thread: ${Thread.currentThread().getId()}  -  Notifies Thread ${wgToNotify.getId()}").asInstanceOf[AnyRef]
              notifyAll()
          }
        }

        this
      }
    }

    def get(): AnyRef = {
      this.synchronized {
        val c = content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wg = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wp = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]

        println(s"Thread: ${Thread.currentThread().getId()}  -  Get").asInstanceOf[AnyRef]

        c.peek() match {              //Lookup elements in content
          case null =>                //If no element exists ADD TO WAITING GETTERS AND PUT TO SLEEP
            println(s"Thread: ${Thread.currentThread().getId()}  -  Add to Waiting Getters and wait").asInstanceOf[AnyRef]
            wg.add(Thread.currentThread())
            println(s"      c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]

            wp.peek() match{            //Lookup waiting putters
              case null =>              //If no waiting putters exists NO NOTHING
              case _ =>                 //If some waiting putters exists NOTIFY IT
                val wpToNotify = wp.peek().asInstanceOf[Thread]
                println(s"Thread: ${Thread.currentThread().getId()}  -  Notifies Thread ${wpToNotify.getId()}").asInstanceOf[AnyRef]
                notifyAll()
            }

            println(s"Thread: ${Thread.currentThread().getId()} goes to sleep!").asInstanceOf[AnyRef]
            wait()
            get()
          /*
            println(s"GETTER Thread: ${Thread.currentThread().getId} WOKEN").asInstanceOf[AnyRef]
            c.peek() match {
              case null =>
                notifyAll()
                println("Wait:").asInstanceOf[AnyRef]
                wait()
                println("Unwait:").asInstanceOf[AnyRef]
              case _ =>
            }
                println(s"Thread: ${Thread.currentThread.getId}  -  Remove self from Waiting Getters, take and return content").asInstanceOf[AnyRef]
                wg.poll()
                val tmp = c.poll()
                println(s"      c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
                return tmp
          //}
          */

          case _ =>                   //If some element exists
            println(s"Thread: ${Thread.currentThread().getId}  -  Take and return content").asInstanceOf[AnyRef]
            val tmp = c.poll()
            println(s"      c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
            return tmp
        }

        AnyRef
      }
    }
    /*
    def put(value: AnyRef): Channel = {
      this.synchronized {
        val c = content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wg = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wp = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]

        println(s"Thread: ${Thread.currentThread()}  -  Put").asInstanceOf[AnyRef]

        if (c.size() < capacity) { //If channel has room for more content
          wg.peek() match {
            case null =>
              //synchronized {
                println(s"Thread: ${Thread.currentThread()}  -  Add to content").asInstanceOf[AnyRef]
                c.add(value)
                println(s"c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
              //}
            case _ => { //If there are any getters waiting
              //wg.peek().synchronized {
                println(s"Thread: ${Thread.currentThread()}  -  Add to content and notify").asInstanceOf[AnyRef]
                c.add(value)
                println(s"c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
              //}
                notifyGet()
            }
          }
        }
        else { //If channel is full
          wg.peek() match {
            case null => { //If there are no getters waiting
              //synchronized {
                println(s"Thread: ${Thread.currentThread()}  -  Add to Waiting Putters").asInstanceOf[AnyRef]
                wp.add(Thread.currentThread())
                println(s"c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
                println(s"Thread: ${Thread.currentThread()} goes to sleep!").asInstanceOf[AnyRef]
              //}
                Thread.currentThread().wait()
                println("PUTTER WOKEN")

            }
            case _ => { //If there are any getters waiting
              //wg.peek().synchronized {
                println(s"Thread: ${Thread.currentThread()}  -  Add to content and notify").asInstanceOf[AnyRef]
                c.add(value)
                println(s"c.size = ${c.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
              //}
                notifyGet()
            }
          }
        }
        this
      }
    }

    def get(): AnyRef = {
      this.synchronized {
        val cc = content.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wp = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        val wg = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[AnyRef]]
        //synchronized {
          println(s"Thread: ${Thread.currentThread()}  -  Get").asInstanceOf[AnyRef]
        //}
        cc.peek() match {
          case null => wp.peek() match {
            case null =>
              //synchronized {
                println(s"Thread: ${Thread.currentThread()}  -  Add to Waiting Getters and wait").asInstanceOf[AnyRef]
                wg.add(Thread.currentThread())
                println(s"c.size = ${cc.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
              //}
                println(s"Thread: ${Thread.currentThread()} goes to sleep!").asInstanceOf[AnyRef]
                Thread.currentThread().wait()
                println("GETTER WOKEN").asInstanceOf[AnyRef]
                //synchronized {
                  println(s"Thread: ${Thread.currentThread()}  -  Remove self from Waiting Getters, take and return content").asInstanceOf[AnyRef]
                  wg.poll()
                  val tmp = cc.poll()
                  println(s"c.size = ${cc.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
                  tmp
                //}


            case _ =>
              //wp.peek().synchronized {
                println(s"Thread: ${Thread.currentThread()}  -  Add to Waiting Getters and notify").asInstanceOf[AnyRef]
                wg.add(Thread.currentThread())
                println(s"c.size = ${cc.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
              //}
                notifyPut()

              throw InternalRuntimeException(s"Not implemented. Channel size: ${cc.size()}.")
          }
          case _ =>
            //synchronized {
              println(s"Thread: ${Thread.currentThread()}  -  Take and return content").asInstanceOf[AnyRef]
              val tmp = cc.poll()
              println(s"c.size = ${cc.size()}; wp.size = ${wp.size()}; wg.size = ${wg.size()}").asInstanceOf[AnyRef]
              tmp
            }
        //}
      }
    }
*/
    def notifyGet(): Unit = {
      this.synchronized{
        val g = waitingGetters.asInstanceOf[ConcurrentLinkedQueue[Thread]].peek()
        println(s"Notifies waiting getter ${g}")
        g.notifyAll()
      }
    }

    def notifyPut(): Unit = {
      this.synchronized {
        val p = waitingPutters.asInstanceOf[ConcurrentLinkedQueue[Thread]].peek()
        println(s"Notifies waiting putter ${p}")
        p.notifyAll()
      }
    }

    final override def equals(obj: scala.Any): Boolean = throw InternalRuntimeException(s"Value.Channel does not support `equals`.")

    final override def hashCode(): Int = throw InternalRuntimeException(s"Value.Channel does not support `hashCode`.")

    final override def toString: String = s"Channel[$tpe] $capacity"
  }
}
