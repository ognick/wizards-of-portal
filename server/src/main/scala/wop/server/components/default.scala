package wop.server.components

import korolev.{Shtml, VDom}

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
object default extends Shtml {

  def style(xs: (String, String)*): String = {
    xs map { case (k, v) => s"$k: $v" } mkString "; "
  }

  def icon(name: String, rest: VDom*) = 'i ('class /= s"icon ion-$name", rest)

}
