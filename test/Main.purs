module Test.Main where

import Prelude
import Data.CoreFn.Parser as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
-- import Test.QuickCheck ((===))

assertEquals :: forall a e. Eq a => a -> a -> Eff (console :: CONSOLE | e) Unit
assertEquals a b = if (a == b) then log "OK" else log "BAD"

infix 2 assertEquals as ===

main :: forall e. Eff (console :: CONSOLE, fs :: FS, err :: EXCEPTION | e) Unit
main = do
  let p s = do
        json <- readTextFile UTF8 s
        logShow $ runExcept $ (readJSON json :: F P.CoreFn)
  p "output/Control.Alt/corefn.json"
  p "output/Control.Alternative/corefn.json"
  p "output/Control.Applicative/corefn.json"
  p "output/Control.Apply/corefn.json"
  p "output/Control.Biapplicative/corefn.json"
  p "output/Control.Biapply/corefn.json"
  p "output/Control.Bind/corefn.json"
  p "output/Control.Category/corefn.json"
  p "output/Control.Comonad/corefn.json"
  p "output/Control.Comonad.Env/corefn.json"
  p "output/Control.Comonad.Env.Class/corefn.json"
  p "output/Control.Comonad.Env.Trans/corefn.json"
  p "output/Control.Comonad.Store/corefn.json"
  p "output/Control.Comonad.Store.Class/corefn.json"
  p "output/Control.Comonad.Store.Trans/corefn.json"
  p "output/Control.Comonad.Traced/corefn.json"
  p "output/Control.Comonad.Traced.Class/corefn.json"
  p "output/Control.Comonad.Traced.Trans/corefn.json"
  p "output/Control.Comonad.Trans.Class/corefn.json"
  p "output/Control.Extend/corefn.json"
  p "output/Control.Lazy/corefn.json"
  p "output/Control.Monad/corefn.json"
  p "output/Control.Monad.Cont/corefn.json"
  p "output/Control.Monad.Cont.Class/corefn.json"
  p "output/Control.Monad.Cont.Trans/corefn.json"
  p "output/Control.Monad.Eff/corefn.json"
  p "output/Control.Monad.Eff.Class/corefn.json"
  p "output/Control.Monad.Eff.Console/corefn.json"
  p "output/Control.Monad.Eff.Exception/corefn.json"
  p "output/Control.Monad.Eff.Exception.Unsafe/corefn.json"
  p "output/Control.Monad.Eff.Random/corefn.json"
  p "output/Control.Monad.Eff.Unsafe/corefn.json"
  p "output/Control.Monad.Error.Class/corefn.json"
  p "output/Control.Monad.Except/corefn.json"
  p "output/Control.Monad.Except.Trans/corefn.json"
  p "output/Control.Monad.List.Trans/corefn.json"
  p "output/Control.Monad.Maybe.Trans/corefn.json"
  p "output/Control.Monad.Reader/corefn.json"
  p "output/Control.Monad.Reader.Class/corefn.json"
  p "output/Control.Monad.Reader.Trans/corefn.json"
  p "output/Control.Monad.Rec.Class/corefn.json"
  p "output/Control.Monad.RWS/corefn.json"
  p "output/Control.Monad.RWS.Trans/corefn.json"
  p "output/Control.Monad.ST/corefn.json"
  p "output/Control.Monad.State/corefn.json"
  p "output/Control.Monad.State.Class/corefn.json"
  p "output/Control.Monad.State.Trans/corefn.json"
  p "output/Control.Monad.Trans.Class/corefn.json"
  p "output/Control.Monad.Writer/corefn.json"
  p "output/Control.Monad.Writer.Class/corefn.json"
  p "output/Control.Monad.Writer.Trans/corefn.json"
  p "output/Control.MonadPlus/corefn.json"
  p "output/Control.MonadZero/corefn.json"
  p "output/Control.Plus/corefn.json"
  p "output/Control.Semigroupoid/corefn.json"
  p "output/Data.Array/corefn.json"
  p "output/Data.Array.Partial/corefn.json"
  p "output/Data.Array.ST/corefn.json"
  p "output/Data.Bifoldable/corefn.json"
  p "output/Data.Bifunctor/corefn.json"
  p "output/Data.Bifunctor.Clown/corefn.json"
  p "output/Data.Bifunctor.Flip/corefn.json"
  p "output/Data.Bifunctor.Join/corefn.json"
  p "output/Data.Bifunctor.Joker/corefn.json"
  p "output/Data.Bifunctor.Product/corefn.json"
  p "output/Data.Bifunctor.Wrap/corefn.json"
  p "output/Data.Bitraversable/corefn.json"
  p "output/Data.Boolean/corefn.json"
  p "output/Data.BooleanAlgebra/corefn.json"
  p "output/Data.Bounded/corefn.json"
  p "output/Data.Char/corefn.json"
  p "output/Data.CommutativeRing/corefn.json"
  p "output/Data.CoreFn.Parser/corefn.json"
  p "output/Data.Date/corefn.json"
  p "output/Data.Date.Component/corefn.json"
  p "output/Data.DateTime/corefn.json"
  p "output/Data.DateTime.Instant/corefn.json"
  p "output/Data.DateTime.Locale/corefn.json"
  p "output/Data.Distributive/corefn.json"
  p "output/Data.Either/corefn.json"
  p "output/Data.Either.Nested/corefn.json"
  p "output/Data.Enum/corefn.json"
  p "output/Data.Eq/corefn.json"
  p "output/Data.EuclideanRing/corefn.json"
  p "output/Data.Field/corefn.json"
  p "output/Data.Foldable/corefn.json"
  p "output/Data.Foreign/corefn.json"
  p "output/Data.Foreign.Class/corefn.json"
  p "output/Data.Foreign.Index/corefn.json"
  p "output/Data.Foreign.Keys/corefn.json"
  p "output/Data.Foreign.Null/corefn.json"
  p "output/Data.Foreign.NullOrUndefined/corefn.json"
  p "output/Data.Foreign.Undefined/corefn.json"
  p "output/Data.Function/corefn.json"
  p "output/Data.Function.Uncurried/corefn.json"
  p "output/Data.Functor/corefn.json"
  p "output/Data.Functor.Invariant/corefn.json"
  p "output/Data.Generic/corefn.json"
  p "output/Data.HeytingAlgebra/corefn.json"
  p "output/Data.Identity/corefn.json"
  p "output/Data.Int/corefn.json"
  p "output/Data.Int.Bits/corefn.json"
  p "output/Data.JSDate/corefn.json"
  p "output/Data.Lazy/corefn.json"
  p "output/Data.List/corefn.json"
  p "output/Data.List.Lazy/corefn.json"
  p "output/Data.List.Lazy.NonEmpty/corefn.json"
  p "output/Data.List.Lazy.Types/corefn.json"
  p "output/Data.List.NonEmpty/corefn.json"
  p "output/Data.List.Partial/corefn.json"
  p "output/Data.List.Types/corefn.json"
  p "output/Data.List.ZipList/corefn.json"
  p "output/Data.Maybe/corefn.json"
  p "output/Data.Maybe.First/corefn.json"
  p "output/Data.Maybe.Last/corefn.json"
  p "output/Data.Monoid/corefn.json"
  p "output/Data.Monoid.Additive/corefn.json"
  p "output/Data.Monoid.Conj/corefn.json"
  p "output/Data.Monoid.Disj/corefn.json"
  p "output/Data.Monoid.Dual/corefn.json"
  p "output/Data.Monoid.Endo/corefn.json"
  p "output/Data.Monoid.Multiplicative/corefn.json"
  p "output/Data.NaturalTransformation/corefn.json"
  p "output/Data.Newtype/corefn.json"
  p "output/Data.NonEmpty/corefn.json"
  p "output/Data.Nullable/corefn.json"
  p "output/Data.Ord/corefn.json"
  p "output/Data.Ord.Unsafe/corefn.json"
  p "output/Data.Ordering/corefn.json"
  p "output/Data.Ring/corefn.json"
  p "output/Data.Semigroup/corefn.json"
  p "output/Data.Semiring/corefn.json"
  p "output/Data.Show/corefn.json"
  p "output/Data.String/corefn.json"
  p "output/Data.String.CaseInsensitive/corefn.json"
  p "output/Data.String.Regex/corefn.json"
  p "output/Data.String.Regex.Flags/corefn.json"
  p "output/Data.String.Unsafe/corefn.json"
  p "output/Data.Time/corefn.json"
  p "output/Data.Time.Component/corefn.json"
  p "output/Data.Time.Duration/corefn.json"
  p "output/Data.Traversable/corefn.json"
  p "output/Data.Tuple/corefn.json"
  p "output/Data.Tuple.Nested/corefn.json"
  p "output/Data.Unfoldable/corefn.json"
  p "output/Data.Unit/corefn.json"
  p "output/Data.Void/corefn.json"
  p "output/Debug.Trace/corefn.json"
  p "output/Global/corefn.json"
  p "output/Global.Unsafe/corefn.json"
  p "output/Math/corefn.json"
  p "output/Node.Buffer/corefn.json"
  p "output/Node.Buffer.Unsafe/corefn.json"
  p "output/Node.Encoding/corefn.json"
  p "output/Node.FS/corefn.json"
  p "output/Node.FS.Async/corefn.json"
  p "output/Node.FS.Internal/corefn.json"
  p "output/Node.FS.Perms/corefn.json"
  p "output/Node.FS.Stats/corefn.json"
  p "output/Node.FS.Stream/corefn.json"
  p "output/Node.FS.Sync/corefn.json"
  p "output/Node.Path/corefn.json"
  p "output/Node.Stream/corefn.json"
  p "output/Partial/corefn.json"
  p "output/Partial.Unsafe/corefn.json"
  p "output/Prelude/corefn.json"
  p "output/Test.QuickCheck/corefn.json"
  p "output/Test.QuickCheck.Arbitrary/corefn.json"
  p "output/Test.QuickCheck.Data.AlphaNumString/corefn.json"
  p "output/Test.QuickCheck.Gen/corefn.json"
  p "output/Test.QuickCheck.LCG/corefn.json"
  p "output/Type.Proxy/corefn.json"
  p "output/Unsafe.Coerce/corefn.json"