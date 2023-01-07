package wms.flow.planner
package global

/**
  * @param priority the consumption priority
  * @param bornPieceIndex the index of the piece when the elements associated to this [[Category]] where born.
  * @param path determines which path is taken by the elements associated to this [[Category]] in the forks of the graph.
  * @param channel the expedition channel to where the elements associated to this [[Category]] are destined to. */
case class Category(priority: Priority, bornPieceIndex: PieceIndex, path: Path, channel: Channel)
