{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | A simple ''cron'' loop. Used for running commands according to a given schedule.
module HCron
	( module HCron.Schedule
	, cronLoop )
where
import HCron.Schedule
import Control.Concurrent
import Data.Time

-- | Given a schedule of commands, run them when their time is due.
--   Only one command is run at a time. If several commands could be started at a specific
--   moment, then we take the one with the earliest potential start time. If any command throws
--   an error then the whole loop does.
--
cronLoop :: Schedule (IO ()) -> IO ()
cronLoop schedule = do
	startTime <- getCurrentTime

	case earliestEventToStartAt startTime $ eventsOfSchedule schedule of
		Nothing -> do
			sleep 1
			cronLoop schedule

		Just event -> do
			let Just cmd = lookupCommandOfSchedule (eventName event) schedule
			cmd
			endTime <- getCurrentTime

			let event' = event
				{ eventLastStarted = Just startTime
				, eventLastEnded = Just endTime }

			let schedule' = adjustEventOfSchedule event' schedule

			cronLoop schedule'

-- | Sleep for a given number of seconds.
sleep :: Int -> IO ()
sleep secs
	= threadDelay $ secs * 1000000
