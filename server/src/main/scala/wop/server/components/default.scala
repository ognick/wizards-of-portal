package wop.server.components

import korolev._

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
object default {

  def style(xs: (String, String)*): String = {
    xs map { case (k, v) => s"$k: $v" } mkString "; "
  }

  def icon(name: String, rest: VDom*) = 'i ('class /= s"icon ion-$name", rest)

}
