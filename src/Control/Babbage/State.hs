{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Babbage.State where

import Control.Monad ((<=<))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, get, put)
import Data.Maybe (maybe)

--
-- a pure state machine
--

class StateMachine a b | b -> a where
    transition :: b -> a -> Maybe b


data DoorState
    = Open
    | Closed
    | Locked
    deriving (Eq, Show, Read)

data DoorActions
    = OpenDoor
    | CloseDoor
    | LockDoor
    | UnlockDoor
    deriving (Eq, Show, Read)

instance StateMachine DoorActions DoorState where
    transition Open CloseDoor = Just Closed
    transition Closed OpenDoor = Just Open
    transition Closed LockDoor = Just Locked
    transition Locked UnlockDoor = Just Closed
    transition _ _ = Nothing

--Allows side effects, but only based on the incoming event or the current state

class StateMachineEff a b | b -> a where
    transitionEff :: (MonadIO m, MonadState b m) =>
        a -> 
        m Bool

type SimpleM a = StateT (IO a)

instance (StateMachine a DoorState) => StateMachineEff a DoorState where
    transitionEff input = do
        st <- get
        let res = transition st input
        maybe (pure False) ( fmap (const False) . put) res

data family Crated a
data instance Crated DoorState = StrDoorState DoorState

class Crateable a key | a -> key where
    encase :: (MonadIO m) => 
        Crated a -> 
        m ()
    extract :: (MonadIO m) =>
        key ->
        m (Crated a)
    unpack :: Crated a -> a
    pack :: a -> Crated a

instance Crateable DoorState String where
    encase (StrDoorState ds) = liftIO . putStrLn $ show ds
    extract k = pure $ StrDoorState (read k :: DoorState)
    unpack (StrDoorState a) = a
    pack = StrDoorState


class (StateMachine a b) => SerializableStateMachine a b key where
    transitionSer :: (MonadIO m, Crateable b key) =>
        key ->
        a ->
        m (Maybe b)
    transitionSer key event = do
        st <- unpack <$> extract key 
        let res = transition st event
        case res of
            Nothing -> pure Nothing
            Just nextState -> do
                encase $ pack nextState
                pure $ Just nextState

instance SerializableStateMachine DoorActions DoorState String
