
module TestsForUnification where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Core.Syntax
import Analysis.Unification

import TestConfig          ( sizeOfGeneratedPatterns )
import Control.Monad.State ( State , runState , get, put )

-- *| Generators:

newtype AnyPattern
  = AP { unAP :: Pattern () }

instance Arbitrary AnyPattern where
  arbitrary = resize sizeOfGeneratedPatterns $ AP <$> sized linearlySized
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
      constructorName = oneof $ return . return <$> ['A'..'Z']
      variableName    = oneof $ return . return <$> ['a'..'z']
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

newtype APairOfStructurallyDifferentPatterns
  = APOSDP { unAPOSDP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyDifferentPatterns where
  arbitrary = APOSDP <$> (arbitrary >>= forceDifferent . unAPOP)
  shrink p  = APOSDP . unAPOP <$> shrink (APOP $ unAPOSDP p)


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
    (NoMatch  ) -> False
    (MatchBy f) -> f p == f (f p)
                && f q == f (f q)

substitutionUnifies :: Unifies
substitutionUnifies (APOSEP (p, q)) =
  case patternMatch p q of
    (NoMatch  ) -> False
    (MatchBy f) -> f p == f q

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
    map (uncurry QC.testProperty) testsOnAPOSEP ++
    map (uncurry QC.testProperty) testsOnAPOSDP

-- *| Exports:

coreUnificationTests =
  testGroup "Unification without existentials."
    [ qcProperties
    ]

-- *| Nasty details:

-- Produces a pair of unifiable patterns from a pair of (possibly)
-- ununifiable ones.
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
            iter (p:s) (q:t) =
              do (p',  q' ) <- forceEquivalent (p, q)
                 (ps', qs') <- iter s t
                 return (p' : ps', q' : qs')
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
isFreeIn x p                             = p

-- Produces a pair of ununifiable patterns from a pair of (possibly)
-- unifiable ones.
forceDifferent :: (Pattern (), Pattern ()) -> Gen (Pattern (), Pattern())
forceDifferent (Variable k x _, Variable k' y _)       =
  oneof $ map return $
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
  = return $ return $ Variable Ordinary x ()
somethingEquals x ((Variable Ordinary y _) : rest)
  = oneof
      [ return (Variable Ordinary x () : rest)
      , do rest' <- somethingEquals x rest
           return (Variable Ordinary y () : rest')
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