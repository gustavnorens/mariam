{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module RefCount (rc) where



import Anf (Body(..), Exp(..), AFunction(..))

import Data.Set (Set)
import qualified Data.Set as Set
import Core (Type(..))
import Prelude hiding (exp)
import Data.Maybe (maybeToList)

rc :: [AFunction] -> [AFunction]
rc = map (\(AFunction name arg body) -> AFunction name arg (rc_body  (Set.fromList (maybeToList arg)) Set.empty body))

rc_body :: Set (String, Type) ->  Set (String, Type) -> Body -> Body
rc_body args owned = \case
    Ret v -> let body = (if check args v then Inc v (Ret v) else Ret v) in
        foldr (add_dec v) body (Set.toList owned)
    Let v exp body -> case exp of
        AInt n -> Let v (AInt n) (rc_body args owned body)
        AATom a -> Let v (AATom a) (rc_body args owned body)
        ACons cons vs -> foldr add_inc (Let v (ACons cons vs) (rc_body args owned' body)) vs
        ABinOp op v1 v2 -> Let v (ABinOp op v1 v2) (rc_body args owned body)
        ACall constant -> Let v (ACall constant) (rc_body args owned' body)
        AApp v1 v2 -> Let v (AApp v1 v2) (rc_body args owned' body)
        AProject field var -> Let v (AProject field var) (rc_body args' owned body)
            where 
                args' = if Set.member var args || fst var == "clos" then Set.insert v args else args
        AClosure fname vs -> foldr add_inc (Let v (AClosure fname vs) (rc_body args owned' body)) vs
        where 
            owned' = Set.insert v owned
    Case v alts -> Case v (map (\(i, b) -> (i, rc_body args owned b)) alts)
    Inc {} -> error "Inc: Does not exist at this stage"
    Dec {} -> error "Dec: Does not exist at this stage"
    where
        add_dec :: (String, Type) -> (String, Type) -> Body -> Body
        add_dec retvar (v, t) body = case t of
            Integer -> body
            _ -> if retvar == (v,t) then body else  Dec (v, t) body

        add_inc :: (String, Type) -> Body -> Body
        add_inc (v, t) body = case t of
            Integer -> body
            _ -> Inc (v, t) body

        check :: Set (String, Type) -> (String, Type) -> Bool
        check vars v = Set.member v vars