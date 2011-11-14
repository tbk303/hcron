{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | A schedule of commands that should be run at a certain time.
module HCron.Schedule
	-- * Time Periods
	( second, minute, hour, day

	-- * When
	, When (..)
	, WhenModifier (..)

	-- * Events
	, EventName
	, Event (..)
	, earliestEventToStartAt
	, eventCouldStartAt

	-- * Schedules
	, Schedule (..)
	, makeSchedule
	, lookupEventOfSchedule
	, lookupCommandOfSchedule
	, adjustEventOfSchedule
	, eventsOfSchedule)
where

import Data.Time
import Data.List
import Data.Function
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

instance Read NominalDiffTime where
	readsPrec n str = let [(secs :: Double, rest)] = readsPrec n str in
		case rest of
			's' : rest' -> [(fromRational $ toRational secs, rest')]
			_ -> []

second, minute, hour, day :: NominalDiffTime
second = 1
minute = 60
hour = 60 * minute
day = 24 * hour

-- When -------------------------------------------------------------------------------------------
-- | When to invoke some event.
data When
	-- | Just keep doing it.
	= Always

	-- | Don't do it, ever.
	| Never

	-- | Do it some time after we last started it.
	| Every NominalDiffTime
	
	-- | Do it some time after it last finished.
	| After NominalDiffTime
	
	-- | Do it each day at this time. The ''days'' are UTC days, not local ones.
	| Daily TimeOfDay
	deriving (Read, Show, Eq)


-- | Modifier to when.
data WhenModifier
	-- | If the event hasn't been invoked before then do it immediately
	--   when we start the cron process.
	= Immediate

	-- | Wait until after this time before doing it first.
	| WaitUntil UTCTime
	deriving (Read, Show, Eq)


-- Event ------------------------------------------------------------------------------------------
type EventName = String

-- | Records when an event should start, and when it last ran.
data Event
	= Event
	{ -- | A unique name for this event.
	  --   Used when writing the schedule to a file.
	  eventName :: EventName

	  -- | When to run the command.
	, eventWhen :: When

	  -- | Modifier to the previous.
	, eventWhenModifier :: Maybe WhenModifier

	  -- | When the event was last started, if any.
	, eventLastStarted :: Maybe UTCTime
		
	  -- | When the event last finished, if any.
	, eventLastEnded :: Maybe UTCTime }
	deriving (Read, Show, Eq)


-- | Given the current time and a list of events, determine which one should be started now.
--   If several events are avaliable then take the one with the earliest start time.
earliestEventToStartAt :: UTCTime -> [Event] -> Maybe Event
earliestEventToStartAt curTime events = let
	eventsStartable = filter (eventCouldStartAt curTime) events
	eventsSorted = sortBy (compare `on` eventLastStarted) eventsStartable
	in listToMaybe eventsSorted


-- | Given the current time, decide whether an event could be started.
--   If the `WhenModifier` is `Immediate` this always returns true.
--   The `SkipFirst` modifier is ignored, as this is handled separately.
eventCouldStartAt :: UTCTime -> Event -> Bool
eventCouldStartAt curTime event
	-- If the event has never started or ended, and is marked as immediate,
	-- then start it right away.
	| Nothing <- eventLastStarted  event
	, Nothing <- eventLastEnded    event
	, Just Immediate	<- eventWhenModifier event
	= True

	-- If the current end time is before the start time, then the most
	-- recent iteration is still running, so don't start it again.
	| Just lastStarted <- eventLastStarted event
	, Just lastEnded <- eventLastEnded   event
	, lastEnded < lastStarted
	= False

	-- Keep waiting if there's a seconday wait modifier.
	| Just (WaitUntil waitTime) <- eventWhenModifier event
	, curTime < waitTime
	= False

	-- Otherwise we have to look at the real schedule.
	| otherwise
	= case eventWhen event of
		Always -> True
		Never -> False

		Every diffTime
		 -> maybe True
			(\lastTime -> (curTime `diffUTCTime` lastTime ) > diffTime)
			(eventLastStarted event)

		After diffTime
		 -> maybe True
			(\lastTime -> (curTime `diffUTCTime` lastTime ) > diffTime)
			(eventLastEnded event)
	
		Daily timeOfDay
		 -- If it's been less than a day since we last started it, then don't do it yet.
		 | Just lastStarted	<- eventLastStarted event
		 , (curTime `diffUTCTime` lastStarted) < day
		 -> False
		
		 | otherwise
		 -> let -- If we were going to run it today, this is when it would be.
			startTimeToday
				= curTime
				{ utctDayTime	= timeOfDayToTime timeOfDay }
				
			-- If it's after that time then quit fooling around..
			in curTime > startTimeToday


-- Schedule ---------------------------------------------------------------------------------------
-- | Map of event names to their details and build commands.	
data Schedule cmd
	= Schedule (Map EventName (Event, cmd))


-- | Get the list of events in a schedule, ignoring the build commands.
eventsOfSchedule :: Schedule cmd -> [Event]
eventsOfSchedule (Schedule sched)
	= map fst $ Map.elems sched


-- | A nice way to produce a schedule.
makeSchedule :: [(EventName, When, Maybe WhenModifier, cmd)] -> Schedule cmd
makeSchedule tuples
 = let makeSched (name, whn, mMod, cmd)
		= (name, (Event name whn mMod Nothing Nothing, cmd))
   in Schedule $ Map.fromList $ map makeSched tuples


-- | Given an event name, lookup the associated event from a schedule.
lookupEventOfSchedule :: EventName -> Schedule cmd -> Maybe Event
lookupEventOfSchedule name (Schedule sched)
	= liftM fst $ Map.lookup name sched


-- | Given an event name, lookup the associated build command from a schedule.
lookupCommandOfSchedule :: EventName -> Schedule cmd -> Maybe cmd
lookupCommandOfSchedule name (Schedule sched)
	= liftM snd $ Map.lookup name sched


-- | Given a new version of an event, update any matching event in the schedule.
--   If the event not already there then return the original schedule.
adjustEventOfSchedule :: Event -> Schedule cmd -> Schedule cmd
adjustEventOfSchedule event (Schedule sched)
	= Schedule 
	$ Map.adjust 
		(\(_, build) -> (event, build))
		(eventName event) 
		sched

