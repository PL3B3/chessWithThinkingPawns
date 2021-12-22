import LinearAlgebra
import Game
import Nets
import System.Random
import Debug.Trace
import Stats

main = do


--type Network = [(Tensor Double, Tensor Double)]
type Layer = [[Tensor Double]]

--default sigmoid feedforward
feed :: [[[Tensor Double]]] -> Tensor Double -> Tensor Double
feed net input = last $ foldl (\list elem@[weight:bias:[]] -> list ++ [fmap (\a -> 1 / (1 + exp a)) $ ta (bias) (mm (last list) weight)]) [input] net

zerotensor shape = makeTensor shape (take (product shape) $ repeat 0)
numtensor num shape = makeTensor shape (take (product shape) $ repeat num)

--takes networks, scores them by fitness, then takes the "topnum" and varies / crossbreeds them to create new generation
--evolve :: [Network] -> [Network] -> Int -> (Network -> Double) -> [Network]
evolve :: [[[[Tensor Double]]]] -> [[[[Tensor Double]]]] -> Int -> ([[[Tensor Double]]] -> Int) -> [[[[Tensor Double]]]]
evolve nets rands topnum fitness = (zipWith addNets eugenes rands)
  where eugenes = map fst $ take topnum $ quickSort (map (\i -> (i, fitness i)) nets)
        netsize = length $ nets !! 0

tis = map (\a -> makeTensor [1,2] a) [[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0]]

--get summary stats from a initNetsAndRunEpochsMultir
grab_stats znets = (foldl (\a b -> a ++ [centTrue $ map b (cnets !! 0)]) [] [tft0, aggro0, peaceful0], foldl (\a b -> a ++ [centTrue $ map b (cnets !! 1)]) [] [tft1, aggro1, peaceful1])
  where cnets = (categ (concat znets) 2)

tft0 net = if (net_stat) net (map (\a@(b,c) -> (makeTensor [1,2] b, makeTensor [1,1] c)) [([0.0,0.0],[0.0]), ([1.0,0.0],[0.0]), ([0.0,1.0],[1.0]), ([1.0,1.0],[1.0])]) == 0 then True else False
tft1 net = if (net_stat) net (map (\a@(b,c) -> (makeTensor [1,2] b, makeTensor [1,1] c)) [([0.0,0.0],[0.0]), ([1.0,0.0],[1.0]), ([0.0,1.0],[0.0]), ([1.0,1.0],[1.0])]) == 0 then True else False
aggro0 net = if (net_stat) net (map (\a@(b,c) -> (makeTensor [1,2] b, makeTensor [1,1] c)) [([0.0,0.0],[1.0]), ([1.0,0.0],[1.0]), ([0.0,1.0],[1.0]), ([1.0,1.0],[1.0])]) == 0 then True else False
aggro1 net = if (net_stat) net (map (\a@(b,c) -> (makeTensor [1,2] b, makeTensor [1,1] c)) [([0.0,1.0],[1.0]), ([1.0,0.0],[1.0]), ([0.0,1.0],[1.0]), ([1.0,1.0],[1.0])]) == 0 then True else False
peaceful0 net = if (net_stat) net (map (\a@(b,c) -> (makeTensor [1,2] b, makeTensor [1,1] c)) [([0.0,0.0],[0.0]), ([1.0,0.0],[0.0]), ([0.0,1.0],[0.0]), ([1.0,1.0],[0.0])]) == 0 then True else False
peaceful1 net = if (net_stat) net (map (\a@(b,c) -> (makeTensor [1,2] b, makeTensor [1,1] c)) [([0.0,0.0],[0.0]), ([1.0,0.0],[0.0]), ([0.0,1.0],[0.0]), ([1.0,1.0],[0.0])]) == 0 then True else False



gameswithtft g = avg [tft0s, tft1s]
  where tft0s = centTrue $ map (\a -> centTrue (map tft0 a)  > 0.0) $ map (\z -> ((categ z 2) !! 0)) g
        tft1s = centTrue $ map (\a -> centTrue (map tft1 a)  > 0.0) $ map (\z -> ((categ z 2) !! 1)) g

--utilities

test_nets :: [[[[Tensor Double]]]] -> Int -> Int -> [[Int]]
test_nets nets pnum turns = map (\a -> map (\b -> snd b) $ fst a) results
  where results = playGames (splitInto nets pnum) turns

net_stat :: [[[Tensor Double]]] -> [(Tensor Double, Tensor Double)] -> Double
net_stat net expecteds = sum $ map (\xpek@(input, out) -> cost (fmap (\a -> fromIntegral $ round_sigmoid a) (feed net input)) out) expecteds
  where cost real xpecs = sum $ zipWith (\a b -> 0.5 * (a - b) ^ 2) (infoOf real) (infoOf xpecs)


getBoards :: [[[[Tensor Double]]]] -> Int -> Int -> [Tensor Int]
getBoards nets pnum turns = map (\a -> fmap round_sigmoid a) $ map (snd) results
  where results = playGames (splitInto nets pnum) turns


addNets :: [[[Tensor Double]]] -> [[[Tensor Double]]] -> [[[Tensor Double]]]
addNets n1 n2 = zipWith (zipWith (zipWith (ta))) n1 n2

categ :: [a] -> Int -> [[a]]
categ list num_categs = foldl (\a b -> map (\c -> (a !! c) ++ [b !! c]) (ps num_categs)) (take (num_categs) $ repeat []) (splitInto list num_categs)

deCateg :: [[a]] -> Int -> [a]
deCateg list num_per_categ = concat $ foldl (\cume categ_dex -> cume ++ [map (!! categ_dex) list]) [] [0..(pred $ num_per_categ)]

displacement :: [[[[Tensor Double]]]] -> Int
displacement ls = sum $ map totalSize $ map getDims ls

segCateg :: [[a]] -> Int -> [[a]]
segCateg list num_per_categ = concat $ foldl (\cume categ_dex -> cume ++ [[map (!! categ_dex) list]]) [] [0..(pred $ num_per_categ)]

appendNets :: [[[Tensor Double]]] -> [[[Tensor Double]]] -> [[[Tensor Double]]]
appendNets n1 n2 = zipWith (zipWith (++)) n1 n2

appendNetsUp :: [[[Tensor Double]]] -> [[[Tensor Double]]] -> [[[Tensor Double]]]
appendNetsUp n1 n2 = zipWith (++) n1 n2

divideTo :: [a] -> Int -> [[a]]
divideTo l n = splitInto l (div (length l) n)

shiftList :: [a] -> Int -> [a]
shiftList list shift = post ++ prev
  where (prev, post) = splitAt (l - (mod shift l)) list
        l = length list

netMap :: (Functor f) => (a -> b) -> [[[f a]]] -> [[[f b]]]
netMap fun net = map (map (map (fmap fun))) net

getDims :: [[[Tensor Double]]] -> [[[[Int]]]]
getDims tensors = map (map (map shapeOf)) tensors

fragment :: [Int] -> [a] -> [[a]]
fragment seglengths list
  | seglengths == [] = []
  | otherwise = [take (head seglengths) list] ++ fragment (tail seglengths) (drop (head seglengths) list)

netToPlayer :: [[[Tensor Double]]] -> Player
netToPlayer wbs = ((\ins -> infoOf $ feed wbs ins), 0) :: Player

--hi to lo
quickSortR :: [([[[Tensor Double]]], Int)] -> [([[[Tensor Double]]], Int)]
quickSortR a = reverse $ quickSort a

--sort the nets. yay! (lo to hi)
quickSort :: [([[[Tensor Double]]], Int)] -> [([[[Tensor Double]]], Int)]
quickSort a
  | a == [] = []
  | xs == [] = [x]
  | otherwise = quickSort (filter (\p -> snd p < snd x) a) ++ (filter (\p -> snd p == snd x) a) ++ quickSort (filter (\p -> snd x < snd p) a)
    where x = head a
          xs = tail a

fln = fromIntegral . length

centTrue ls = (fln $ filter id ls) / (fln ls)


totalSize :: [[[[Int]]]] -> Int
totalSize dims = sum $ concat $ concat $ concat (netMap product [dims])

tdimup :: [[[[Int]]]] -> [Int]
tdimup dims = concat $ concat $ concat (netMap product [dims])

zipNets :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
zipNets f n1 n2 = zipWith (zipWith (zipWith (f))) n1 n2

--takes dims, a list of nums, and makes a net of doubles shaped like dims
sigNetMake :: [[[[Int]]]] -> [Double] -> [[[[Double]]]]
sigNetMake dims nums = tail $ foldl (\a b -> [[[[fromIntegral $ (newhead a) + (newprod b)]]]] ++ (tail a) ++ [sigLayerMake b (take (newprod b) (drop (newhead a) nums))]) [[[[0.0]]]] dims
  where newprod a = sum $ concat $ map (map product) a
        newhead = floor . head . head . head . head

sigLayerMake :: [[[Int]]] -> [Double] -> [[[Double]]]
sigLayerMake dims nums = tail $ foldl (\a b -> [[[fromIntegral $ (newhead a) + (newprod b)]]] ++ (tail a) ++ [sigStepMake b (take (newprod b) (drop (newhead a) nums))]) [[[0.0]]] dims
  where newprod a = sum $ map product a
        newhead = floor . head . head . head

sigStepMake :: [[Int]] -> [Double] -> [[Double]]
sigStepMake dims nums = tail $ foldl (\a b -> [[fromIntegral $ (newhead a) + product b]] ++ (tail a) ++ [take (product b) (drop (newhead a) nums)]) [[0.0]] dims
  where newhead = floor . head . head


darwin3 :: [[[[Tensor Double]]]] -> [Double] -> Int -> Int -> Int -> [[[[Tensor Double]]]]
darwin3 nets rands rand num_per_game turns = out
  where categs = categ nets num_per_game
        shifted_categs = foldl (\e f -> e ++ [shiftList (categs !! f) (f * rand)]) [] (pl categs)
        games_players_list = segCateg shifted_categs num_per_categ
        results = playGames games_players_list turns
        all_net_score_pairs = concat $ map fst results
        --these are the categorized net-score pairs for each playerspot category
        -- trace ("net score pairs: " ++ show all_net_score_pairs ++ "\n") $
        net_score_categs = categ all_net_score_pairs num_per_game
        --still in category form. the proto eugenes are
        proto_eugenes = map (\i -> map fst $ concat $ rpt (div num_per_categ num_per_game) $ take num_per_game $ quickSort i) net_score_categs
        --proto_eugenes = map (\i -> map fst $ concat $ rpt (div num_per_categ num_per_game) $ (take (num_per_game - 1) $ quickSortR i) ++ (take 1 (quickSort i))) net_score_categs
        --proto_eugenes = map (\i -> map fst $ concat $ rpt num_per_game $ take (div num_per_categ num_per_game) $ quickSortR i) net_score_categs
        eugenes = concat $ segCateg proto_eugenes num_per_categ
        --trace ("eugenes: " ++ show eugenes ++ "\n")$
        out = mutate eugenes rands
        num_per_categ = div (length nets) num_per_game

--spawns randomized nets yet again but doesn't bother segmenting the games
initRandNets :: [[[[[Int]]]]] -> IO ([[[[Tensor Double]]]])
initRandNets dims = do
  gen <- newStdGen
  let rands1 = randomRs (-3.0, 3.0) gen
  let nets = genNets dims rands1
  return nets

--cycler for evolution. essentially an epochs machine
runEpochs :: [[[[Tensor Double]]]] -> Int -> Int -> Int -> IO ([[[[Tensor Double]]]])
runEpochs nets pnum turns cyc
  | cyc == 0 = do
      return nets
  | otherwise = do
      gen <- newStdGen
      let rands = randomRs (-0.5, 0.5) gen
      let rand = fst $ randomR (1, div (length nets) pnum) gen
      let new_nets = darwin3 nets rands rand pnum turns
      geta <- runEpochs new_nets pnum turns (pred cyc)
      return geta

--combines creator and cycler
initNetsAndRunEpochs :: [[[[[Int]]]]] -> Int -> Int -> Int -> IO ([[[[Tensor Double]]]])
initNetsAndRunEpochs dims pnum turns cyc = do
  alpha <- initRandNets dims
  zeta <- runEpochs alpha pnum turns cyc
  return zeta

initNetsAndRunEpochsMulti :: [[[[[Int]]]]] -> Int -> Int -> Int -> Int -> IO ([[[[[Tensor Double]]]]])
initNetsAndRunEpochsMulti dims pnum turns cyc metacyc
  | metacyc == 1 = do
      alpha <- initRandNets dims
      zeta  <- runEpochs alpha pnum turns cyc
      return [zeta]
  | otherwise = do
      alpha <- initRandNets dims
      zeta  <- runEpochs alpha pnum turns cyc
      next <- initNetsAndRunEpochsMulti dims pnum turns cyc (pred metacyc)
      return ([zeta] ++ next)

mutate :: [[[[Tensor Double]]]] -> [Double] -> [[[[Tensor Double]]]]
mutate nets nums = zipWith (addNets) nets (genNets (map getDims nets) nums)

--the param "list_game_players" is a list of "lobbies" i.e. the group of nets playing one game
playGames :: [[[[[Tensor Double]]]]] -> Int -> [([([[[Tensor Double]]], Int)], Tensor Double)]
playGames list_game_players t = map (\a -> game3 (playersToGame a) t) list_game_players

playersToGame :: [[[[Tensor Double]]]] -> ([([[[Tensor Double]]], Int)], Tensor Double)
playersToGame nets = (map (\a -> (a, 0) :: ([[[Tensor Double]]], Int)) nets, numtensor 0.0 [1,(length nets) * (pred $ length nets)])

rpt :: Int -> [a] -> [[a]]
rpt num ls = take num $ repeat ls

netsFrom :: ([([[[Tensor Double]]], Int)], Tensor Double) -> [[[[Tensor Double]]]]
netsFrom game = map fst $ fst game

--[([(player, score)], gameboard)]
manyGames :: [([([[[Tensor Double]]], Int)], Tensor Double)] -> Int -> [([([[[Tensor Double]]], Int)], Tensor Double)]
manyGames gs t = map (\a -> game3 a t) gs

--game but with tensors. players/board and turns are params
game3 :: ([([[[Tensor Double]]], Int)], Tensor Double) -> Int -> ([([[[Tensor Double]]], Int)], Tensor Double)
game3 i t = foldl (\a b -> newturn3 a) i [1..t]

initGame3 :: [[[[Tensor Double]]]] -> Int -> ([([[[Tensor Double]]], Int)], Tensor Double)
initGame3 nets turns = game3 (map (\a -> (a, 0) :: ([[[Tensor Double]]], Int)) nets, numtensor 0.0 [1,(length nets) * (pred $ length nets)]) turns

initGame4 :: [[[[Tensor Double]]]] -> Int -> Tensor Double -> ([([[[Tensor Double]]], Int)], Tensor Double)
initGame4 nets turns startboard = game3 (map (\a -> (a, 0) :: ([[[Tensor Double]]], Int)) nets, startboard) turns

getscores a = map snd (fst a)
getboard a = fmap (round_sigmoid) $ snd a

scores a = map snd $ fst a

--modded ngb to round the outputs. map (\a -> fromIntegral $ round_sigmoid a) $
newturn3 :: ([([[[Tensor Double]]], Int)], Tensor Double) -> ([([[[Tensor Double]]], Int)], Tensor Double)
newturn3 prev@(p,g@(Tensor s i)) = (nps, ngb)
  where nps = foldl (\c d -> c ++ [(fst (p !! d), (snd (p !! d)) + (sum $ zipWith (score) (dci !! d) (aln d)))]) [] cn1
        ngb = makeTensor [succ $ s !! 0, s !! 1] (i ++ (concat dcs))
        (cn1, cn2) = ([0..(pred $ length p)], [0..(pred $ pred $ length p)])
        dcs = foldl (\a b -> a ++ [infoOf $ feed b (get_row g (pred $ s !! 0))]) [] (map fst p)
        dci = map (map round_sigmoid) dcs
        aln d = foldl (\e f -> e ++ [if (f < d) then ((except d dci) !! f) !! (d - 1) else ((except d dci) !! f) !! d]) [] cn2

--takes in list of dimensions, each dimension is a whole net, converts to tensor by filling in dimensions with numbers
genNets :: [[[[[Int]]]]] -> [Double] -> [[[[Tensor Double]]]]
genNets dims nums = (foldl (\l c -> l ++ [zipNets makeTensor (dims !! (length l)) (sigNetMake (dims !! (length l)) c)]) [] (fragment chunks nums))
  where chunks = map totalSize dims

--special genNets for multigames
genNetsMultiGame :: [[[[[Int]]]]] -> [Double] -> Int -> [[[[Tensor Double]]]]
genNetsMultiGame dims nums displacement = (foldl (\l c -> l ++ [zipNets makeTensor (dims !! (length l)) (sigNetMake (dims !! (length l)) c)]) [] (fragment chunks (drop displacement nums)))
  where chunks = map totalSize dims

xstart :: ([Player], Tensor Double)
xstart = ((map netToPlayer (foldl (\l c -> l ++ [zipNets makeTensor (dims !! (length l)) (sigNetMake (dims !! (length l)) c)]) [] (fragment chunks rands))), (zerotensor [1, (length dims) * (pred $ length dims)]))
  where dims = xdims 4
        chunks = map totalSize dims

rands = [1.9083831041282062,1.1100858868423282,-1.598223901073399,0.8008114196592704,1.2450868443746685,1.033584717474609,1.0957746860751278,1.8439228541068253,0.9108758440722142,1.1306059086638838,-2.1304010588888205,2.953692231013787,-0.4313294653036488,1.9691947322981012,2.902210877509553,1.2647761296591442,-0.4496396440511843,9.945471401060679e-2,-2.954902760503635,-2.6635755678069426,0.5507953895828743,-1.3915012586565862,-1.8780657906465184,0.2255770622837252,-2.41477170538284,1.1724450270394167,-0.8097985978265259,2.131553051404129,0.3810299941762074,1.293417090607556,-1.590304272487214,-2.666019075770307,-2.518122329674897,2.9843315709385623,0.9149342312914142,0.2466046019529049,1.0912567817803138,1.2186949078328349,2.634557404927225,1.9229954717939108,1.4037482509752985,-0.8813363199865671,2.7621902905147913,-2.8625757443699564,2.4385162043084065,-1.5342120315582966,1.3147754301779147,-0.181245426958081,-1.7306674891059677,2.8612838506024465,-3.1980213065200935e-2,-1.2129405881136963,2.904981769718776,1.639139413093245,1.2167052560066942,6.892923490598779e-2,-2.6401286183778945,2.011167281631618,-0.7479767780433431,-1.1092715769180823,-1.9034268227796243,1.6434814562050244,-2.7894089747635924,-1.9879427504124616,2.6840131105318727,1.9360654144374685,1.3015547549051316,-1.9423413028773409,-1.615459026583102,1.1037307812890615,2.982035108458735,-1.5470198085039932,1.78357842530178,-2.584818125433671,-0.8856278882407143,-1.6772497725300093,-1.1718612996051587,-1.3825675621914437,-2.3027849970518584,-0.46887235026058827,-0.9658259345882261,1.0491559367291803,-6.161867459066883e-4,1.922585319971489,2.609246399326464,1.2635971053749246,5.793117688991245e-2,1.338505095165126,1.0363384000027445,-1.6530206032316206,-1.818553117389699,2.2583974387532306,-2.584025756639738,-1.9964411932882664,-2.3221381091393587,2.7789992837789494,1.8954564404957344,1.6222693724254533,0.2496634357158971,2.2663851028206246,-1.3924502595821493,-1.4405326855916232,2.0176182422027233,-1.998866405713206,2.3104800080230206,1.7583561120649929,1.079763629017588,-2.3164644001273875,1.9534960665793042,0.7557011867780985,-0.7064472479507447,2.5643731085674446,0.7944370069419859,-2.9320217431296847,-2.384826818782102,-0.13871718814942735,-1.0070414428356522,-0.4161541678205323,2.433382442999404,-2.251097988534309,1.6142100492480438,2.644219624080292,0.48835893743621295,-1.4543043179480981,2.3713843830458217,2.0350417544623953,1.555492744740345,1.7292658517897488,2.7622461047133733,-2.5246400447052215,0.24279861379967294,2.0622439269443467,0.7090180092877469,1.891687059607488,-1.7648567100705037,1.2268001376622815,1.1189627446500445,5.166555879697876e-2,-1.1045536075078617,-1.6126978462675965,2.911075370721467,-1.7377569959965122,-1.8554875568080813,-1.8081646640950733,-1.5264151467451779,-0.30477149137015047,-0.9372850595151032,0.5751707235677901,2.586684593801442,1.180565338000707,1.8282716856796606,-0.11385453207207075,-2.7483377106759437,-1.2966789685819733,-0.39902082058989086,2.845185252517293,1.2828467428082995,-2.934536905734286,1.5932008440805197,-1.1253632929748432,-0.20394213938385253,0.6055559382464728,1.998714394979494,-1.4655090418797054,2.796432019857381,-1.8469016889583822,2.030442566983723,-2.5232900053221154,0.1429611999477638,-2.1800996103258194,-0.7546260296962273,2.130529295915645,2.48123114725614,2.9701573110568127,1.1087203020070575,2.99176219658683,1.602388355329846,1.2051647223022401,-2.6387253835964293,0.18381759605796644,-7.086674413619054e-2,-0.10307854197888489,0.7215482409613472,-1.455778384144849,-2.5587829688982384,-1.6985762791497134,2.2359590809907965,2.714850831330322,2.679315583001941,-1.5816845012504632,-2.989451933645615e-2,2.186703412455583,2.6022370646131883,1.456332849245622,1.8515257323968637,-0.6468247164019738,-1.2817449000324008,-0.37310858003572456,-2.5975056881633587,0.3146445732029317,-1.9881039155384816,1.270065187685276,-1.6863566553489306e-2,-1.1721980825174754,1.8615911918763466,-0.22550726723350234,-1.8585853710163747e-2,-1.506925112685513,-1.728534019515215,1.7329176739469787,2.5059222156922685,1.3592500570218942,-0.900454306094641,-2.99843878075046,-2.5383881204920895,1.1879031292783617,-2.218330439749874,0.7492233465996083,2.628882040327655,-2.148266883467338,-1.8448319691261577,-1.9661237659163275,2.5243449892440175,-2.5645053056562483,0.7791320124828913,-2.4224971370542,-1.878417565979133,-2.6944323106528127,1.0405391888231792,-1.606153168942507,-1.5007946708541766,-2.629418333097653,0.2938543752167826,-1.3644592706682264,2.805606119030645,-1.5180817069601071,-2.7508825361807214,2.938143877529848,0.3053777206746404,1.2365980665153735,-2.1996347064232413,-2.667477576746037,-0.6274057651801734,0.9591659620934267,-1.4668840291804834,-1.6148857809876391,1.2275664939236481,2.0081761339364226,1.3209015247556604,0.7059773789617783,1.8478433729226893,-2.554097052663405,0.3268139265567518,0.9084232311404699,0.9422270936718133,-2.3017216216145777,0.43092714851919744,-2.848115410973156,-0.4279234218305592,1.7268895934368214,-1.2891606722846516,-1.7110919694907325,-0.7502460248900045,-2.2209528587816156,-1.3407049982146664,-0.20739566767182183,-2.8479592881273152,-1.4112755854548475,1.841848353246327,0.48564856012507285,-2.465972540193035,-0.3878248764418428,-1.5856584370349602,1.7998605948736932,-1.669287597600929,1.6283156920348514,1.5602388138686578,-0.7958320653641615,-1.4403665689961467,-2.334419704665634,-2.158184594723524,-2.4637491877468554,-2.6413885598578126,1.161321751287109,1.3268861149270599,-2.9292670060706962,-1.9952040711642678,-1.9913516383979082,-2.8728101380249416,2.5574487466162417,-2.6071339014147528,-2.1799297179765578,2.6354786850802947,2.57083846829929,1.9631496092738558,2.345174611850153,-0.7427504248586185,2.597089861247463,2.3902352082244125,1.9646264854193918,-1.042774897668337,-0.8869465469337143,0.3801288165144481,-0.517132493079421,-0.5297579312334979,0.3341007398995681,0.2571216489742376,-2.299650303351891,-2.9712530264441073,2.386067554228669,-1.2829025339182478,1.782448952785229,-0.9368631179301774,0.8277002287107695,1.2183624599110816,-0.14086930200771697,2.0878979730742415,0.47654399295628336,-0.4540108256680071,-2.792136595378677,-2.83235440342487,2.31319166759428,2.0868552650042496,-2.6462920712639244,-1.11684581446927,1.0670050859871942,-0.8436763716639879,2.013283305482217,2.3242793812338576,0.29121921159338005,2.9235724276381747,1.0690915442643947,-1.425135602455161,1.7631166219596563,-1.1961328616315001,0.684859545498548,0.7186398283182589,-2.9364027239368573,1.5541988826443687,-0.72290190148279,1.9283219429052334,0.3645357075926867,0.10268104098309383,-1.1105971042511769,0.7266069974522149,-1.976975840030074,-1.7766632753763107,2.717220818709759,-0.7752399311409812,2.3385124402956547,0.9138457135964528,1.6998875372705466,1.262674913128536,-1.2033156093958899,-2.4136752250535487,-0.28635506593901816,1.8584933739603073,2.628295442654384,2.9322418627542177,0.6653047763817308,-1.082974405144002,2.341454148931004,-2.15387109708981,-0.7184903409290735,-1.3148500303732813,-2.8011393770267334,-2.566198375597454,2.6972512531772086,2.12836042126452,0.26305679888966127,1.9669802621604289,1.3970126801460454,2.746751962177072,1.7298657246257205,-2.597265442378214,1.9491711119374049,-1.5928151245118507,1.2850516662223397,-1.0488018247095685,0.9271212440458267,0.4390276677279674,2.8674620747020523,-0.5481871858793692,-0.4674343660252811,-0.8506743555190397,0.9355904412842531,-2.508323151880975,1.7532011241321381,-2.0900763436464382,-1.8950613625276556,1.5889299560010173e-2,0.1404731757730695,-0.7992659498142936,-0.5076265338368757,-2.7346021303942027,2.8192868835292595,1.1368841257007247,-8.000824881226087e-2,-2.945278823203056,0.5187022684213343,-1.5026658366361418,-1.0748409737811502,-2.1385676007793433,-2.421800944060099,0.3320553181261161,-0.6642867470466243,-0.8771942791958738,-0.5204193771345533,2.0862232060309003,0.13543741835084022,1.3004229930435827,-1.2548755119779862,-1.1196036284531452,-2.5411556282336867,0.7946663394817244,0.8803876178316274,0.9076560298842526,2.8398934184361835,1.3441979378614732,-1.4028943211960596,-0.6234818500679848,2.925385359438069,0.20464715369723585,-1.1176003781532842,-6.737452393366716e-2,-0.7830729190007686,-0.5918118763898308,0.6143521176744877,-1.9289066410648688,-2.4649169935019013,-1.15645318618503,-1.4825105643582521,0.8107109131103067,1.637040489759956,-1.6387662313616018,-2.105602703538277,0.4707397917797156,-2.457073022793156,2.538016165570397,1.6656645013638407,1.9399580186697563,-0.9975862269977074,2.730833225479139,-0.4711878836568877,-2.9662271328328282,0.7841251003158414,-1.988398931051555,1.1043243785756385e-2,-2.42776166155485,7.845434114154237e-2,-2.549834298727017,-0.31843725192541505,0.6264612394052858,1.8219853703417899,-1.4554425216647842,-1.910047539140038,2.355320958178871,-2.702411253573679,-0.918004718512913,1.218910282285547]
