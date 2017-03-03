module SpecLayered where

import Language.Atom hiding (compile)
import qualified Language.Atom as A

import Language.Sally


-- | Instantiate a switched star shaped ethernet network. The outputs are a 
-- channel input/output for each node.
mkSWEther :: Int   -- ^ number of nodes on the network
          -> Int   -- ^ number of switches in the network
                   --   (# of redundant channels)
          -> Type  -- ^ type of messages on the network
          -> Atom [(ChanInput, ChanOutput)]
          -- ^ n-(input, output) pairs to connect to nodes
mkSWEther n m typ = do
  let makeEndChans nm = unzip <$> mapM (flip channel typ) [nm ++ show i | i <- [0..n-1]]
  (nte_i, nte_o) <- mkEndChans "node_to_end_"
  (etn_i, etn_o) <- mkEndChans "end_to_node_"
  let res = zip nte_i etn_o  -- nte_o and etn_i are internal to the network

  -- generate the internal channels: [ [ (chan_into_net, chan_out_net) ] ]
  internalChans <- forM [0..m-1] $ \j -> do    -- loop over switches
                     forM [0..n-1] $ \i -> do  -- loop over endpoints
                       c1 <- channel (printf "e%d_to_sw%d" i j) typ
                       c2 <- channel (printf "sw%d_to_e%d" j i) typ
                       return (c1, c2)
  let getIncoming cs = map (fst . snd) cs ++ map (snd . snd) cs
  let getOutgoing cs = map (fst . fst) cs ++ map (snd . fst) cs

  -- generate the switches
  forM_ [0..m-1] $ \j -> do
    atom (printf "sw%d" j) $ do
      -- listen on each incoming chan and broadcast to all outgoing chans
      let myChans = internalChans !! j  -- :: [ (chan_into_net, chan_out_net) ]
      let myIncoming = getIncoming myChans
      let myOutgoing = getOutgoing myChans
      forM (zip myIncoming [0..]) $ \(cout, k) -> do
        atom (printf "handler%d" k) $ do
          cond $ fullChannel cout
          v <- readChannel cout
          mapM_ (flip writeChannel v) myOutgoing

  -- generate the endpoints
  endPoints <- forM [0..n-1] $ \i -> do
    let myChans = map (!! i) internalChans  -- :: [ (chan_into_net, chan_out_net) ]
    let myIncoming = getIncoming myChans
    let myOutgoing = getOutgoing myChans
    let myNodeCout = nte_o !! i
    atom ("endpoint_to_net" ++ show i) $ do
      -- listen on special endpoint channel and broadcast
      cond $ fullChannel myNodeCout
      v <- readChannel myNodeCout
      mapM_ (flip writeChannel v) myOutgoing  -- broadcast

    -- listen to all switches and write any receives to special node channel
    -- input
    forM_ [0..m-1] $ \j -> do
      let swOutput = myIncoming !! j
      atom ("endpoint_from_net" ++ show i) $ do
        cond $ fullChannel swOutput
        v <- readChannel swOutput
        writeChannel (etn_i !! j) v

  return res


-- | A simple system A --> B where the link inbetween is realized as a
-- redundant switched ethernet network.
atomLayered  = atom "atomLayered" $ do

  -- declare the switched ethernet fabric for 2 nodes with 2 internal switches
  chans <- mkSWEther 2 2 Bool
  let (nodeAToE, eToNodeA) = chans !! 0  :: (ChanInput, ChanOutput)
  let (nodeBToE, eToNodeB) = chans !! 1  :: (ChanInput, ChanOutput)

  -- node A
  atom "node_A" $ do
    msg <- bool "msg" False
    -- send a message
    atom "sender" $ do
      done <- bool "done" False
      cond $ not_ (value done)
      writeChannel nodeAToE True
      done <== Const True

    atom "receiver" $ do
      -- store received messages
      cond $ fullChannel eToNodeA
      v <- readChannel eToNodeA
      msg <== v

  atom "node_B" $ do
    msg <- bool "msg" False
    -- store received messages
    cond $ fullChannel nodeBToE
    v <- readChannel nodeBToE
    msg <== v
