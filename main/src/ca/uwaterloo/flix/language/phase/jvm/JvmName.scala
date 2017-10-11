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

package ca.uwaterloo.flix.language.phase.jvm

import java.nio.file.{Path, Paths}

/**
  * Companion object for the [[JvmName]] class.
  */
object JvmName {

  /**
    * The Flix Context class.
    */
  val Context: JvmName = JvmName(Nil, "Context")

  /**
    * The `java.math.BigInteger` name.
    */
  val BigInteger: JvmName = JvmName(List("java", "math"), "BigInteger")

  /**
    * The `java.lang.Boolean` name.
    */
  val Boolean: JvmName = JvmName(List("java", "lang"), "Boolean")

  /**
    * The `java.lang.Character` name.
    */
  val Character: JvmName = JvmName(List("java", "lang"), "Character")

  /**
    * The `java.lang.Byte` name.
    */
  val Byte: JvmName = JvmName(List("java", "lang"), "Byte")

  /**
    * The `java.lang.Short` name.
    */
  val Short: JvmName = JvmName(List("java", "lang"), "Short")

  /**
    * The `java.lang.Integer` name.
    */
  val Integer: JvmName = JvmName(List("java", "lang"), "Integer")

  /**
    * The `java.lang.Long` name.
    */
  val Long: JvmName = JvmName(List("java", "lang"), "Long")

  /**
    * The `java.lang.Float` name.
    */
  val Float: JvmName = JvmName(List("java", "lang"), "Float")

  /**
    * The `java.lang.Double` name.
    */
  val Double: JvmName = JvmName(List("java", "lang"), "Double")

  /**
    * The `java.lang.Object` name.
    */
  val Object: JvmName = JvmName(List("java", "lang"), "Object")

  /**
    * The `java.lang.String` name.
    */
  val String: JvmName = JvmName(List("java", "lang"), "String")

  /**
    * The `ca.uwaterloo.flix.api.Tuple` name
    */
  val Tuple: JvmName = JvmName(List("ca", "waterloo", "flix", "api"), "Tuple")

  /**
    * The `ca.uwaterloo.flix.api.Tag` name
    */
  val Tag: JvmName = JvmName(List("ca", "waterloo", "flix", "api"), "Tag")

  /**
    * The `ca.uwaterloo.flix.api.Unit` name
    */
  val Unit: JvmName = JvmName(List("ca", "waterloo", "flix", "api"), "Unit")

  /**
    * The `java.lang.Exception` name
    */
  val Exception: JvmName = JvmName(List("java", "lang"), "Exception")

  /**
    * The `java.lang.Exception` name
    */
  val UnsupportedOperationException: JvmName = JvmName(List("java", "lang"), "UnsupportedOperationException")
}

/**
  * Represents the name of a Java class or interface.
  *
  * @param pkg  the package name.
  * @param name the class or interface name.
  */
case class JvmName(pkg: List[String], name: String) {
  /**
    * Returns the type descriptor of `this` Java name.
    */
  def toDescriptor: String = "L" + pkg.mkString("/") + "/" + name + ";"

  /**
    * Returns the internal name of `this` Java name.
    */
  def toInternalName: String = if (pkg.isEmpty) name else pkg.mkString("/") + "/" + name

  /**
    * Returns the relative path of `this` Java name.
    */
  def toPath: Path = Paths.get(pkg.mkString("/"), name + ".class")
}