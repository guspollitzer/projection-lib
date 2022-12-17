package wms.flow.planner
package workflow

import time.*

sealed trait DesiredBacklog;

case class Minimal(duration: Duration) extends DesiredBacklog
case object Maximal extends DesiredBacklog
