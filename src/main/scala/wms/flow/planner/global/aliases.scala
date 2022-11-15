package wms.flow.planner
package global

import java.time.Duration

type Quantity = Float

/** Perhaps a better name would be deadline. Because the priority increases when the value decreases. */
type Priority = time.Instant

/** The channel is determined by the trip duration */
type Channel = Duration
