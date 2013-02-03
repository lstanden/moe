package org.moe.runtime

import scala.collection.mutable.{HashMap,Map}

/**
 * MoeClass!
 *
 * @param name MoeClass
 * @param version None
 * @param authority None
 * @param superclass None
 */
class MoeClass(
    private val name: String,
    private val version: Option[String] = None,
    private val authority: Option[String] = None,
    private var superclass: Option[MoeClass] = None
  ) extends MoeObject {

  private val methods: Map[String,MoeMethod] = new HashMap[String, MoeMethod]()
  private val attributes: Map[String,MoeAttribute] = new HashMap[String, MoeAttribute]()

  // Identity ...

  /**
   * Returns the name of this class
   */
  def getName: String = name

  /**
   * Returns the version of this class
   */
  def getVersion: Option[String] = version

  /**
   * Returns the authority of this class
   */
  def getAuthority: Option[String] = authority

  // Superclass ...

  /**
   * Returns this superclass of this class
   */
  def getSuperclass: Option[MoeClass] = superclass

  /**
   * Returns true if this class has a superclass
   */
  def hasSuperclass: Boolean  = superclass.isDefined

  /**
   * Sets the superclass of this class.
   */
  def setSuperclass(s: Option[MoeClass]) = superclass = s

  /**
   * Gets a list of classes in method resolution order for this class.
   */
  def getMRO: List[MoeClass] = superclass.map(
      s => this :: s.getMRO
    ).getOrElse(List(this))

  // Attributes

  /**
   * Adds an attribute to this class
   */
  def addAttribute(attribute: MoeAttribute): Unit = attributes += (attribute.getName -> attribute)

  /**
   * Removes an attribute from this class
   */
  def removeAttribute(name: String): Unit = attributes -= name

  /**
   * Returns this class' attribute with the specified name.
   *
   * @param name The name of the attribute to return
   */
  def getAttribute(name: String): Option[MoeAttribute] = attributes.get(name).orElse(
      superclass.flatMap({ sc => sc.getAttribute(name) })
    )

  /**
   * Returns true if this class (or any of it's superclasses) has an attribute
   * with the specified name.
   *
   * @param name The name of the attribute to check for
   */
  def hasAttribute(name: String): Boolean = getAttribute(name).isDefined

  // Instances

  /**
   * Creates a new instance of this class.
   */
  def newInstance: MoeObject = new MoeOpaque(Some(this))

  // Methods ...

  /**
   * Adds a method to this class.
   *
   * @param method The method to add to this class
   */
  def addMethod(method: MoeMethod): Unit = methods += (method.getName -> method)

  /**
   * Removes a method from this class
   */
  def removeMethod(name: String): Unit = methods -= name

  /**
   * Returns this class' method with the specified name.
   *
   * @param name The name of the method to return
   */
  def getMethod(name: String): Option[MoeMethod] = methods.get(name).orElse(
      superclass.flatMap({ sc => sc.getMethod(name) })
    )

  /**
   * Returns true if this class has a method with the specified name.
   *
   * @param name The name of the method to check for.
   */
  def hasMethod(name: String): Boolean = getMethod(name).isDefined

  // Utils ...

  /**
   * Returns a [[scala.collection.Map]] of names and attributes for this class
   * and all of it's superclasses.
   */
  private def collectAllAttributes: Map[String, MoeAttribute] = superclass.map(
      { s => s.collectAllAttributes ++ attributes }
    ).getOrElse(attributes.clone)

  /**
   * Returns a [[scala.collection.Map]] of names and methods for this class
   * and all of it's superclasses.
   */
  private def collectAllMethods: Map[String, MoeMethod] = superclass.map(
      { s => s.collectAllMethods ++ methods }
    ).getOrElse(methods.clone)

  /**
   * Returns a string representation of this class.
   */
  override def toString: String = {
    "{ " + name + "-" + version.getOrElse("") + "-" + authority.getOrElse("") + superclass.map({ s =>
      " #extends " + s.toString
    }).getOrElse("") + " }"
  }

}
