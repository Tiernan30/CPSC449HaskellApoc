module AggressiveAI where

import ApocTools
import ApocFunction

aggressiveAI :: Chooser
aggressiveAI gameState Normal player =
    let board = theBoard gameState in
    -- Knight moves
    let moveOfK = (bestStrategies board (getCordForEntity board player Knight)) in
    -- Pawn moves
    let moveOfP = (bestStrategies board (getCordForEntity board player Pawn)) in
    let otherMoves = (listOfOtherMoves board (getCord board player)) in
    -- Choose a move to use
    let move = listPick ((moveOfK ++ moveOfP) ++ otherMoves) in
    if move /= Nothing then return $ Just [(fst (fromJust move)), (snd (fromJust move))] else return Nothing
  
aggressiveAI gameState PawnPlacement player =
  let e_entities = pawnDef (theBoard gameState) E in
  let move = randomPick e_entities in
  if move /= Nothing then return $ Just [(fromJust move)] else return Nothing