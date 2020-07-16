/*
 *  BuilderUtil.scala
 *  (in|filtration)
 *
 *  Copyright (c) 2019-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.infiltration

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.SoundProcesses

import scala.concurrent.Future
import scala.reflect.ClassTag

object BuilderUtil {
  def any2stringadd: Any = ()

  def mkFolder[S <: Sys[S]](parent: Folder[S], name: String)(implicit tx: S#Tx): Folder[S] =
    mkObj[S, Folder](parent, name, -1)(Folder[S]())

  def mkObj[S <: Sys[S], R[~ <: stm.Sys[~]] <: Obj[~]](parent: Folder[S], name: String, version: Int)
                                                      (create: => R[S])
                                                      (implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] = {
    val opt = parent.$[R](name)
    checkMkObj(opt, name, version)(create)(parent.remove)(parent.addLast)
  }

  private def checkMkObj[S <: Sys[S], R[~ <: Sys[~]] <: Obj[~]](opt: Option[R[S]], name: String, version: Int)
                                                               (create: => R[S])
                                                               (remove: Obj[S] => Unit)
                                                               (put: R[S] => Unit)
                                                               (implicit tx: S#Tx): R[S] = {

    def add(): R[S] = {
      val res = create
      res.name = name
      if (version >= 0) res.attr.put("version", IntObj.newConst(version))
      put(res)
      res
    }

    opt match {
      case Some(x) =>
        if (version < 0 || x.attr.$[IntObj]("version").exists(_.value >= version)) {
          x
        } else {
          remove(x)
          add()
        }

      case _ => add()
    }
  }

  def mkObjIn[S <: Sys[S], R[~ <: stm.Sys[~]] <: Obj[~]](parent: Obj[S], key: String, version: Int)(create: => R[S])
                                                        (implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] = {
    val a = parent.attr
    val opt = a.$[R](key)
    checkMkObj(opt, key, version)(create)(_ => ())(a.put(key, _))
  }

  def atomic[S <: Sys[S], A](body: S#Tx => A)(implicit cursor: stm.Cursor[S]): A =
    cursor.step(tx => body(tx))

  def flatMapTx[S <: Sys[S], A, B](fut: Future[A])(body: S#Tx => A => Future[B])
                                  (implicit cursor: stm.Cursor[S]): Future[B] = {
    import SoundProcesses.executionContext
    fut.flatMap { a =>
      atomic[S, Future[B]] { implicit tx => body(tx)(a) }
    }
  }

  def mapTx[S <: Sys[S], A, B](fut: Future[A])(body: S#Tx => A => B)
                              (implicit cursor: stm.Cursor[S]): Future[B] = {
    import SoundProcesses.executionContext
    fut.map { a =>
      atomic[S, B] { implicit tx => body(tx)(a) }
    }
  }
}
