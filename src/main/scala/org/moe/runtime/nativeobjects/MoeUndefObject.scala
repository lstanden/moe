package org.moe.runtime.nativeobjects

import org.moe.runtime._

/**
 * NOTE:
 * this is the one outlyer, it doesn't need to be
 * a NativeObject subclass because it doesn't need 
 * to actually box any values, just the lack of a 
 * value
 */

class MoeUndefObject(
    t : Option[MoeType] = None
  ) extends MoeObject(t) {

  // runtime methods

  def equal_to (r: MoeRuntime, other: MoeObject): MoeBoolObject = r.NativeObjects.getBool(
    isUndef && other.isUndef
  )

  def not_equal_to (r: MoeRuntime, other: MoeObject): MoeBoolObject = r.NativeObjects.getBool(
    isUndef && !other.isUndef
  )

  // MoeNativeObject overrides

  def getNativeValue: AnyRef = null
  
  def copy = new MoeUndefObject(getAssociatedType)

  // MoeObject overrides

  override def isFalse: Boolean = true
  override def isUndef: Boolean = true
  override def toString: String = "undef"
}
