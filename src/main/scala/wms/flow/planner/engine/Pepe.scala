package wms.flow.planner
package engine

import scala.collection.immutable.SortedMap

import graph.Stage
import resource.Money
import time.Instant

trait Plan {
	def withIncreasedPowerAt(instant: Instant): PlanChange
	def withDecreasePowerAt(instant: Instant): PlanChange
}

case class Headcount(regular: Int, polyvalent: Int, hourly: Int)

case class Staff(headcountByStage: Map[Stage, Headcount])

case class PlanImpl(staffByIntervalStartingDate: Map[Instant, Staff]) extends Plan {
	def withIncreasedPowerAt(instant: Instant): PlanChange = ???
	def withDecreasePowerAt(instant: Instant): PlanChange = ???
}

trait PlanChange {
	def original: Plan
	def changed: Plan
}



//case class Backlog(queueByStage: GraphMap[Queue])

//case class Step(startingDate: Instant, startingBacklog: Backlog, endingBacklog: Backlog)

//case class BacklogTrajectory(stepsByEndingDate: Map[Instant, Step])


//trait TrajectoryCalculator {
//	def calcTrajectoryChange(originalTrayectory: BacklogTrajectory, planChange: PlanChange): BacklogTrajectory
//}


/////


trait StageProcessor {

}

trait WholeProcessor {
	def getProcesorAt(stage: Stage): StageProcessor
}