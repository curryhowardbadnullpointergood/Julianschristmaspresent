


-- This is the file where we define the datatypes, do this in a monadic/monoid context so that we can keep
-- support for monads and concurrecny in our language as well as making the type systeam easier to 
-- implement in the future 

-- For question 1, it was decided to implement a basic String, Integer and Boolean datatype 


-- implementing String monoid for our language 
newtype StringMonoid = StringMonoid String

instance Semigroup StringMonoid where
    StringMonoid x <> StringMonoid y = StringMonoid (x ++ y)


instance Monoid StringMonoid where

    mempty = StringMonoid ""
    mappend = (<>)

instance Show StringMonoid where
    show (StringMonoid string) = show string



-- implementing custom boolean monoid 

newtype BooleanMonoid = BooleanMonoid Bool

instance Semigroup BooleanMonoid where
    (BooleanMonoid x) <> (BooleanMonoid y) = BooleanMonoid (x && y)

instance Monoid BooleanMonoid where
    mempty = BooleanMonoid True
    mappend = (<>)


instance Show BooleanMonoid where 
    show (BooleanMonoid x ) = show x 


-- implementing custom number monoid -- this requires a bit of work :) 

-- implementing sum monoid 

newtype SumMonoid a = SumMonoid a

instance Num a => Semigroup (SumMonoid a) where
    (SumMonoid x) <> (SumMonoid y) = SumMonoid (x + y)

instance Num a => Monoid (SumMonoid a) where
    mempty = SumMonoid 0
    mappend = (<>)

instance Show a => Show (SumMonoid a) where 
    show (SumMonoid x) = show x

-- monoid for ordering numbers which will be required for the first question: 

newtype MinMonoid a = MinMonoid a

instance Ord a => Semigroup (MinMonoid a) where
    (MinMonoid x) <> (MinMonoid y) = MinMonoid (min x y)

instance (Ord a, Bounded a) => Monoid (MinMonoid a) where
    mempty = MinMonoid maxBound
    mappend = (<>)


instance Show a => Show (MinMonoid a) where 
    show (MinMonoid x) = show x

