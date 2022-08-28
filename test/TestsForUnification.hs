
module TestsForUnification where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Core.Syntax
import Analysis.Unification (patternMatch, PatternMatch(..))

import qualified TestConfig as Config
import Control.Monad.State

import Data.Bifunctor

-- *| Properties:

type Unifies      = APairOfStructurallyEquvialentPatterns -> Bool
type DoesNotUnify = APairOfStructurallyDifferentPatterns  -> Bool

equivalentPatternsUnify :: Unifies
equivalentPatternsUnify (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch -> False
    _       -> True

substitutionIsIdempotent :: Unifies
substitutionIsIdempotent (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch   -> False
    MatchBy f ->   f p == f (f p)
                && f q == f (f q)

substitutionUnifies :: Unifies
substitutionUnifies (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch   -> False
    MatchBy f -> f p == f q

differentPatternsDontUnify :: DoesNotUnify
differentPatternsDontUnify (APOSDP (p, q)) =
  case patternMatch p q of
    NoMatch -> True
    _       -> False

testsOnAPOSEP :: [(String, Unifies)]
testsOnAPOSEP =
  [ ("Equivalent patterns always unify"              , equivalentPatternsUnify)
  , ("Substitution is idempotent"                    , substitutionIsIdempotent)
  , ("The pattern matching substitution is a unifier", substitutionUnifies)
  ]

testsOnAPOSDP :: [(String, DoesNotUnify)]
testsOnAPOSDP =
  [ ("Structurally different patterns never unify"  , differentPatternsDontUnify)
  ]

qcProperties :: TestTree
qcProperties =
  testGroup "Tested by Quick Check" $
    map qc testsOnAPOSEP ++
    map qc testsOnAPOSDP
  where
    qc (s, p) = uncurry QC.testProperty (s, withMaxSuccess Config.numberOfUnificationTests p)

-- *| Generators:

constructorName :: Gen C
constructorName = oneof $ return . return <$> ['A'..'Z']

variableName :: Gen X
variableName = oneof $ return . return <$> ['a'..'z']

variableSort :: Gen Sort
variableSort = oneof $ [return] <*> [Existential, Ordinary]

-- TODO:
-- This should be StateT [X] (ReaderT Int) ...
-- And also, it should generalize to make patterns that work further
-- than commutativity (like associative unificaiton or something).

-- -- A sized variable pattern that uses a fresh variable name.
-- variable :: StateT ([X], Int) Gen (Pattern ())
-- variable =
--   do x       <- lift variableName
--      k       <- lift variableSort
--      (xs, n) <- get
--      if x `elem` xs
--        then variable
--        else do put (x : xs, n)
--                return $ Variable k x ()

-- -- A sized constructor pattern that uses a fresh variable name.
-- constructor :: StateT ([X], Int) Gen (Pattern ())
-- constructor =
--   do c      <- lift constructorName
--      (_, m) <- get
--      n      <- lift (choose (0, m) :: Gen Int)
--      ps     <- mapM (const pattern_) [1..n]
--      return $ Constructor c ps ()

-- -- Generates a sized pattern, which is free of certain names.  Appends the newly
-- -- generated name.
-- pattern_ :: StateT ([X], Int) Gen (Pattern ())
-- pattern_ =
--   do b      <- lift arbitrary
--      (xs, n) <- get
--      put (xs, n - 1)
--      if b || n <= 0
--        then variable
--        else constructor

-- -- Generates an equivalent pattern given an arbitrary pattern.
-- -- generated name.
-- equivalent :: AnyPattern -> StateT ([X], Int) Gen (Pattern ())
-- equivalent (AP p) =
--   case p of
--     (Variable _  x   ()) ->
--       do b <- lift arbitrary
--          if b
--            then return p
--            else do modify (first (x:))
--                    pattern_
--     (Constructor c ps _) ->
--       do ps' <- mapM (equivalent . AP) ps
--          return $ Constructor c ps' ()

-- names :: Pattern () -> [X]
-- names (Variable     _ x _) = [x]
-- names (Constructor _ ps _) = ps >>= names

-- Given an arbitrary pattern, generates a fresh one.
newtype AnyPattern
  = AP { unAP :: Pattern () }
  deriving (Eq, Show)

instance Arbitrary AnyPattern where
  -- arbitrary =
  --   resize Config.sizeOfGeneratedPatterns $
  --   AP . fst <$> sized (\n -> runStateT pattern_ ([], n))
  arbitrary = resize Config.sizeOfGeneratedPatterns $ AP <$> sized linearlySized
    where
      linearlySized = sizedPattern (\n -> n - 1)
      sizedPattern f 0 =
        do vname <- variableName
           return (Variable Ordinary vname ())
      sizedPattern f n =
        oneof
          [ do vname <- variableName
               return (Variable Ordinary vname ())
          , do cname <- constructorName
               ps    <- resize n $ listOf (sizedPattern f (f n))
               return (Constructor cname ps ())
          ]
  shrink (AP (Variable k _    _)) =
    do x <- return <$> ['a'..'z']
       return $ AP $ Variable k x ()
  shrink (AP (Constructor c ps _)) =
    do ps' <- shrink (AP <$> ps)
       return $ AP $ Constructor c (unAP <$> ps') ()

newtype AnyPairOfPatterns
  = APOP { unAPOP :: (Pattern (), Pattern ()) }

instance Arbitrary AnyPairOfPatterns where
  arbitrary = curry APOP <$> (unAP <$> arbitrary) <*> (unAP <$> arbitrary)
  shrink  p =
    do p' <- shrink $ AP $ fst $ unAPOP p
       q' <- shrink $ AP $ snd $ unAPOP p
       return $ APOP (unAP p', unAP q')

newtype APairOfStructurallyEquvialentPatterns
  = APOSEP { unAPOSEP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyEquvialentPatterns where
  arbitrary = APOSEP . equivalentify <$> arbitrary
  shrink p  = APOSEP . equivalentify <$> shrink (APOP $ unAPOSEP p)

newtype ThreeStructurallyEquvialentPatterns
  = TSEP { unTSEP :: (Pattern (), Pattern (), Pattern()) }
  deriving (Show)

instance Arbitrary ThreeStructurallyEquvialentPatterns where
  arbitrary = TSEP . f <$> ((,) <$> arbitrary <*> arbitrary)
    where f (a1, a2) =
            let (p, q)   = unAPOSEP a1
                (_, r)   = equivalentify $ APOP (q, unAP a2)
            in (p, q, r)

newtype APairOfStructurallyDifferentPatterns
  = APOSDP { unAPOSDP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyDifferentPatterns where
  arbitrary = APOSDP <$> (arbitrary >>= forceDifferent . unAPOSEP)
  shrink p  = APOSDP . unAPOP <$> shrink (APOP $ unAPOSDP p)

-- *| Exports:

coreUnificationTests :: TestTree
coreUnificationTests =
  testGroup "Unification without existentials."
    [ qcProperties
    ]

-- *| Nasty details:

-- Produces a pair of unifiable patterns from a pair of (possibly)
-- ununifiable ones (turn into a Gen instead)?
equivalentify :: AnyPairOfPatterns -> (Pattern (), Pattern ())
equivalentify =
    regularify . fst . flip runState [] . forceEquivalent . unAPOP
  where
    forceEquivalent
      :: (Pattern (), Pattern ())
      -> State [(Name, Pattern ())] (Pattern (), Pattern ())
    forceEquivalent (p, q) =
      case (p, q) of
        (Variable _ x _, _) ->
           do xqs <- get
              q'  <- case filter ((==x) . fst) xqs of
                        ((_, q'') : _) -> return q''
                        [            ] ->
                          do let q'' = x `isFreeIn` q
                             put $ (x, q'') : xqs
                             return q''
              return (p, q')
        (Constructor c ps a, Constructor _ qs _) ->
          do (ps', qs') <- iter ps qs
             return (Constructor c ps' a, Constructor c qs' a)
          where
            iter (p':s) (q':t) =
              do (p'',  q'' ) <- forceEquivalent (p', q')
                 (p's', q's') <- iter s t
                 return (p'' : p's', q'' : q's')
            iter _     _     = return ([], [])
        _ ->
          do (q', p') <- forceEquivalent (q, p)
             return (p', q')
    -- TODO: find a weaker way than forcing regularity ?
    regularify (p, q) =
      let (Constructor _ [p', q'] _) =
            forceRegular (Constructor "Pair" [p, q] ())
      in  (p', q')
    -- Produces a regular pattern from a (possibly) irregular one.
    forceRegular = fst . regular []
      where
        regular xs (Variable  k x    a) = fresh k xs x a
        regular xs (Constructor c ps a) =
            (Constructor c ps' a, xs')
          where
            (ps', xs') = regular' xs ps
        regular' xs [     ] = ([], xs)
        regular' xs (p : s) = (p' : s', xs')
          where
            (p', xs'') = regular  xs   p
            (s', xs' ) = regular' xs'' s
        fresh k xs x a | x `elem` xs = fresh k xs (x ++ "'") a
        fresh k xs x a               = (Variable k x a, x : xs)

-- Adds "plings" to make names differ from `x`.
isFreeIn :: Name -> Pattern a -> Pattern a
isFreeIn x (Variable k y m)     | x == y = Variable k (y ++ "'") m
isFreeIn x (Constructor c ps m)          = Constructor c ((x`isFreeIn`) <$> ps) m
isFreeIn _ p                             = p

-- Produces a pair of ununifiable patterns from a pair of (possibly)
-- unifiable ones.
forceDifferent :: (Pattern (), Pattern ()) -> Gen (Pattern (), Pattern())
forceDifferent (Variable k x _, Variable _ y _)  =
  oneof $ map return
    [ (Constructor y [Variable k x ()] (), Variable k x ())
    , (Variable k x (), Constructor y [Variable k x ()] ())
    ]
forceDifferent (Variable k x _, Constructor y ps _) =
  do ps' <- somethingEquals x ps
     return (Variable k x (), Constructor y ps' ())
forceDifferent (Constructor y ps _, Variable k x _) =
  do ps' <- somethingEquals x ps
     return (Constructor y ps' (), Variable k x ())
forceDifferent (Constructor x ps _, Constructor y qs _) =
  oneof
    [ return (Constructor x ps (), Constructor (x++"'") qs ())
    , do (ps', qs') <- makeSomethingDifferent ps qs
         return (Constructor x ps' (), Constructor y qs' ())
    ]

-- Makes sure that the variable "x" occurs at least once.
somethingEquals :: Name -> [Pattern ()] -> Gen [Pattern ()]
somethingEquals x []
  = do sort <- variableSort
       return $ return $ Variable sort x ()
somethingEquals x ((Variable sort y _) : rest)
  = oneof
      [ return (Variable sort x () : rest)
      , do rest' <- somethingEquals x rest
           return (Variable sort y () : rest')
      ]
somethingEquals x (Constructor c ps _ : rest)
  = oneof
      [ do ps' <- somethingEquals x ps
           return (Constructor c ps' () : rest)
      , do rest' <- somethingEquals x rest
           return (Constructor c ps () : rest')
      ]

-- Generates pairs of lists of patterns where at least one pair
-- is different.
makeSomethingDifferent
  :: [Pattern ()]
  -> [Pattern ()]
  -> Gen ([Pattern ()], [Pattern ()])
makeSomethingDifferent [] [] =
  do ps <- listOf1 (unAP <$> arbitrary)
     return (ps, [])
makeSomethingDifferent (p:s) [] =
  return (p:s, [])
makeSomethingDifferent [] (q:s) =
  return ([], q:s)
makeSomethingDifferent (p:s) (q:t) =
  oneof
    [ do (p', q') <- makeSomethingDifferent [p] [q]
         return (p' ++ s, q' ++ t)
    , do (s', t') <- makeSomethingDifferent s t
         return (p : s', q : t')
    ]
