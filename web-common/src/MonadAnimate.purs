module MonadAnimate where

import Control.Monad (class Monad)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Data.Function (($), (<<<))
import Data.Time.Duration (class Duration, fromDuration)
import Data.Unit (Unit)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (HalogenM)

class
  Monad m <= MonadAnimate m where
  delay :: forall t. Duration t => t -> m Unit

instance monadAnimateState :: MonadAnimate m => MonadAnimate (StateT s m) where
  delay = lift <<< delay

instance monadAnimateHalogenM :: MonadAff m => MonadAnimate (HalogenM s f g p m) where
  delay time = liftAff $ Aff.delay $ fromDuration time
