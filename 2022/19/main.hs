-- Day 19: wow --

{-
  part 1 8s, part 2 1m20s, total 1m30s
  part 1 test input 8s (blueprint 1 8s, blueprint 2 0.1s)
  part 2 test input eternity (blueprint 1 23 MINUTES, blueprint 2 9s)
  what the heck
-}

import Data.Maybe

main :: IO ()
main
  = do input <- readInputStrs "input.txt"
       let bloops = map readBlueprint input
           -- Part 1
           start = startState 24
           peakGeodeLevels = [ maxGeodes (run start blp) | blp <- bloops ]
           peakGeodesIDs = zip peakGeodeLevels [1..30]
           part1answer = sum [ x*y | (x,y) <- peakGeodesIDs ]
           -- Part 2
           stuart = startState 32
           part2first = maxGeodes (run stuart (bloops!!0))
           part2second = maxGeodes (run stuart (bloops!!1))
           part2third = maxGeodes (run stuart (bloops!!2))
           part2answer = part2first*part2second*part2third
         in do putStrLn ("Part One: " ++ (show part1answer))
               putStrLn ("Part Two: " ++ (show part2answer))


readInputStrs :: FilePath -> IO [String]
readInputStrs fp = do input <- readFile fp
                      return (lines input)


data RobotType = OreBot | ClayBot | ObsBot | GeodBot
data Inventory = Inv { ore, clay, obs, geod :: Int }
data RobotArmy = Army { oreBots, clayBots, obsBots, geodBots :: Int }
data Blueprint = Blp { oreBotCost, clayBotCost,
                       obsBotCost, geodBotCost :: Cost }
type Cost = Inventory -- NB a cost never has a geode component
data Countdown = Countdown Int
data State = State { inv :: Inventory,
                     army :: RobotArmy,
                     countdown :: Countdown }

startState mins = State (Inv 0 0 0 0) (Army 1 0 0 0) (Countdown mins)

-- Test blueprints
blueprint1 = Blp (Inv 4 0 0 0) (Inv 2 0 0 0) (Inv 3 14 0 0) (Inv 2 0 7 0)
blueprint2 = Blp (Inv 2 0 0 0) (Inv 3 0 0 0) (Inv 3 8 0 0) (Inv 3 0 12 0)

readBlueprint :: String -> Blueprint
readBlueprint s
  = let wds = words s  -- >:)
        oreOre = read (head (drop 6 wds))
        clayOre = read (head (drop 12 wds))
        obsOre = read (head (drop 18 wds))
        obsClay = read (head (drop 21 wds))
        geodOre = read (head (drop 27 wds))
        geodObs = read (head (drop 30 wds))
      in Blp (Inv oreOre 0 0 0) (Inv clayOre 0 0 0)
             (Inv obsOre obsClay 0 0) (Inv geodOre 0 geodObs 0)


maxGeodes :: [State] -> Int
maxGeodes ss = mg ss 0 where
  mg [] g = g
  mg (s:ss) g = if (geod (inv s)) > g then mg ss (geod (inv s)) else mg ss g

maxOreCost :: Blueprint -> Int
maxOreCost (Blp (Inv oreBotOCost _ _ _) (Inv clayBotOCost _ _ _)
                (Inv obsBotOCost _ _ _) (Inv geodBotOCost _ _ _))
  = maximum [oreBotOCost, clayBotOCost, obsBotOCost, geodBotOCost]


run :: State -> Blueprint -> [State]
run s bp = run' [s] bp where
  run' :: [State] -> Blueprint -> [State]
  run' [] _ = []
  run' states@((State _ _ (Countdown 0)):_) _ = states
  run' states@((State _ (Army _ _ _ _) _):sts) bp
    = run' (concatMap (futures (maxOreCost bp) bp) states) bp


minute :: Blueprint -> State -> Maybe RobotType -> Maybe State
minute (Blp   (Inv oreBotOreCost _ _ _)
              (Inv clayBotOreCost _ _ _)
              (Inv obsBotOreCost obsBotClayCost _ _)
              (Inv geodBotOreCost _ geodBotObsCost _))
       (State (Inv oreHave clayHave obsHave geodHave)
              (Army oreBots clayBots obsBots geodBots)
              (Countdown cd))
       robotOrder = case robotOrder of
  Nothing
    -> Just (State (Inv (oreHave + oreBots) (clayHave + clayBots)
                        (obsHave + obsBots) (geodHave + geodBots))
                   (Army oreBots clayBots obsBots geodBots)
                   (Countdown (cd-1)))
  (Just OreBot)
    -> if (oreHave >= oreBotOreCost)
       then Just (State (Inv (oreHave - oreBotOreCost + oreBots)
                             (clayHave + clayBots)
                             (obsHave + obsBots)
                             (geodHave + geodBots))
                        (Army (oreBots+1) clayBots obsBots geodBots)
                        (Countdown (cd-1))) else Nothing
  (Just ClayBot)
    -> if (oreHave >= clayBotOreCost)
       then Just (State (Inv (oreHave - clayBotOreCost + oreBots)
                             (clayHave + clayBots)
                             (obsHave + obsBots)
                             (geodHave + geodBots))
                        (Army oreBots (clayBots+1) obsBots geodBots)
                        (Countdown (cd-1))) else Nothing
  (Just ObsBot)
    -> if (oreHave >= obsBotOreCost && clayHave >= obsBotClayCost)
       then Just (State (Inv (oreHave - obsBotOreCost + oreBots)
                             (clayHave - obsBotClayCost + clayBots)
                             (obsHave + obsBots)
                             (geodHave + geodBots))
                        (Army oreBots clayBots (obsBots+1) geodBots)
                        (Countdown (cd-1))) else Nothing
  (Just GeodBot)
    -> if (oreHave >= geodBotOreCost && obsHave >= geodBotObsCost)
       then Just (State (Inv (oreHave - geodBotOreCost + oreBots)
                             (clayHave + clayBots)
                             (obsHave - geodBotObsCost + obsBots)
                             (geodHave + geodBots))
                        (Army oreBots clayBots obsBots (geodBots+1))
                        (Countdown (cd-1))) else Nothing


futures :: Int -> Blueprint -> State -> [State]
futures maxOreCost
        blp@(Blp (Inv oreBotOreCost _ _ _)
                 (Inv clayBotOreCost _ _ _)
                 (Inv obsBotOreCost obsBotClayCost _ _)
                 (Inv geodBotOreCost _ geodBotObsCost _))
        state@(State inv@(Inv oreHave clayHave obsHave geodHave)
                     (Army oreBots clayBots obsBots geodBots)
                     (Countdown cd))

  -- The "do nothing" (make no robot) thing is the dynamite, so avoid that.
  -- You can only build one robot a minute, so if your ore is sorted, then
  -- if you can make a robot, you should make a robot (and not do Nothing).
  | oreHave - maxOreCost + oreBots >= maxOreCost
  = case catMaybes [minute blp state (Just GeodBot),
                   -- Late stage obsidian empire: Don't make more ObsBots
                   (if obsHave - geodBotObsCost + obsBots >= geodBotObsCost
                   then Nothing else minute blp state (Just ObsBot)),
                   -- Late stage clay empire: Likewise
                   (if clayHave - obsBotClayCost + clayBots >= obsBotClayCost
                   then Nothing else minute blp state (Just ClayBot)),
                   -- This here takes pt2 from 23s and incorrect to 1m20s and
                   -- correct. Ore is special for obv reasons; late stage ore
                   -- empire has a higher bar...
                   -- BUT if your OBSIDIAN is sorted then you just need the ore
                   -- supply to keep up with geode bot ore cost.
                   (if oreBots >= maxOreCost
                       || (geodBots > 0 && oreBots >= geodBotOreCost)
                    then Nothing else minute blp state (Just OreBot))] of
      [] -> [fromJust (minute blp state Nothing)]
      s  -> s
  -- Otherwise you are ore-precarious which upsettingly can still be optimal
  | otherwise
  = catMaybes [minute blp state (Just GeodBot),
               (if obsHave - geodBotObsCost + obsBots >= geodBotObsCost
               then Nothing else minute blp state (Just ObsBot)),
               (if clayHave - obsBotClayCost + clayBots >= obsBotClayCost
               then Nothing else minute blp state (Just ClayBot)),
               (if (geodBots > 0 && oreBots >= geodBotOreCost) -- :|
               then Nothing else minute blp state (Just OreBot)),
               minute blp state Nothing]
