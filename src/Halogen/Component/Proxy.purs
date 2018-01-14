module Halogen.Component.Proxy
  ( ProxyComponent
  , ProxyQ
  , queryQ
  , proxy
  , proxyQI
  , proxyQL
  , proxyQR
  , proxyTrans
  , proxyEval
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Coyoneda (Coyoneda, liftCoyoneda, unCoyoneda)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as HQ

-- | The type of a proxied component.
type ProxyComponent f i o = H.Component HH.HTML (ProxyQ f i o) i o

-- | The query algebra for proxied components. The `queryQ` function is used
-- | to construct values of this type as the constructors are private.
data ProxyQ f i o a
  = Query (Coyoneda f a)
  | Receive i a
  | Raise o a

-- | Lifts a query into `ProxyQ`. Every query sent to a `ProxyComponent` will
-- | need to go through this function.
queryQ :: forall f i o. f ~> ProxyQ f i o
queryQ = Query <<< liftCoyoneda

-- | Proxies a component, completely hiding the original query algebra.
proxy
  :: forall f i o m
  . H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyQ (Const Void) i o) i o m
proxy = proxyEval (const (absurd <<< un Const))

-- | Proxies a component, reusing the original query algebra as the proxied
-- | query algebra. Essentially creates a component that lifts `f` into
-- | `ProxyQ`.
proxyQI
  :: forall f i o m
   . H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyQ f i o) i o m
proxyQI = proxyTrans id

-- | Proxies a component, hiding the left side of a coproduct query algebra
-- | but continuing to expose the right.
proxyQL
  :: forall f g i o m
   . H.Component HH.HTML (Coproduct f g) i o m
  -> H.Component HH.HTML (ProxyQ f i o) i o m
proxyQL = proxyTrans left

-- | Proxies a component, hiding the right side of a coproduct query algebra
-- | but continuing to expose the right.
proxyQR
  :: forall f g i o m
   . H.Component HH.HTML (Coproduct f g) i o m
  -> H.Component HH.HTML (ProxyQ g i o) i o m
proxyQR = proxyTrans right

-- | Proxies a component with a custom query algebra transform.
proxyTrans
  :: forall f g i o m
   . (g ~> f)
  -> H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyQ g i o) i o m
proxyTrans eta =
  proxyEval \k q ->
    H.query unit (eta q) >>= case _ of
      Nothing -> HQ.halt "Proxy inner component query failed (this shouldn't be possible)"
      Just a -> pure (k a)

-- | Proxies a component with a function that handles the running of interior
-- | queries.
-- |
-- | It's unlikely this is useful for common use cases - it's used to implement
-- | the rest of the `proxy*` functions, but may also allow for some use cases
-- | that were not considered by the functions provided in this library.
proxyEval
  :: forall f g i o m
   . (forall a b. (b -> a) -> g b -> H.ParentDSL i (ProxyQ g i o) f Unit o m a)
  -> H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyQ g i o) i o m
proxyEval evalQuery component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input Receive
    }
  where
    render :: i -> H.ParentHTML (ProxyQ g i o) f Unit m
    render i = HH.slot unit component i (HE.input Raise)

    eval :: ProxyQ g i o ~> H.ParentDSL i (ProxyQ g i o) f Unit o m
    eval = case _ of
      Query iq -> unCoyoneda evalQuery iq
      Receive i next -> H.put i $> next
      Raise o next -> H.raise o $> next
